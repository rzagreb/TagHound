"""TagHound: evaluate tag rules against single mappings or whole DataFrames."""

from __future__ import annotations

from pathlib import Path
from typing import Any
from collections.abc import Mapping

import pandas as pd

from taghound.constants import DEFAULT_REGEX_MERGE_PATTERN
from taghound.models import TagRule
from taghound.serializers import to_tag_rule


class TagHound:
    """Holds a set of tag rules and finds the tags matching given data."""

    def __init__(self, rules: list[TagRule]) -> None:
        """Initialize the TagHound object.

        Args:
            rules (list[TagRule]): The rules to search tags with.
        """
        self.set_rules(rules)

    def find_all_tags(
        self,
        data: Mapping[str, Any],
        multi_threaded: bool = False,
        thread_exec_params: dict | None = None,
    ) -> tuple[str, ...]:
        """Find all tags in the data.

        Args:
            data (Mapping[str, Any]): The data to search for tags
            multi_threaded (bool): Whether to use multi-threading to search for tags
                NOTE: rule checks are CPU-bound Python, so threads usually add
                overhead instead of speed; measure before enabling
            thread_exec_params (Optional[dict]): The parameters to pass to the ThreadPoolExecutor

        Returns:
            tuple[str, ...]: The ids of the rules that matched the data

        Example:
            >>> from taghound.models import TagRule
            >>> rules = [
            ...     TagRule(
            ...         id="python",
            ...         label="Python",
            ...         weight=10.0,
            ...         required_fields={"language", "year"},
            ...         scalar_check=lambda d: d["language"] == "python" and d["year"] > 1990,
            ...     )
            ... ]
            >>> th = TagHound(rules)
            >>> th.find_all_tags({"language": "python", "year": 1991, "version": "3.10"})
            ('python',)

        """
        if not self.__rules:
            raise ValueError("No rules to search for tags")

        if not self.__required_fields.issubset(data.keys()):
            raise ValueError(
                f"Missing required fields: {self.__required_fields - data.keys()}"
            )

        if multi_threaded:
            from concurrent.futures import ThreadPoolExecutor, as_completed

            thread_exec_params = thread_exec_params or {}

            tag_ids: list[str] = []
            with ThreadPoolExecutor(**thread_exec_params) as executor:
                future_to_rule = {}
                for rule in self.__rules:
                    if not rule.scalar_check:
                        raise ValueError("No scalar check function found")
                    future_to_rule[executor.submit(rule.scalar_check, data)] = rule

                for future in as_completed(future_to_rule):
                    rule: TagRule = future_to_rule[future]
                    if future.result():
                        tag_ids.append(rule.id)

            return tuple(tag_ids)
        else:
            return tuple(rule.id for rule in self.__rules if rule.scalar_check(data))  # type: ignore

    def find_tags_using_vector(
        self,
        data: pd.DataFrame | list[Mapping[str, Any]],
        output_format: str = "list",
        output_column_name: str = "tags",
    ) -> pd.DataFrame:
        """Find tags in the data using vectorized operations.

        Evaluates every rule against the whole DataFrame at once; this usually
        pays off over `find_all_tags` for large batches — measure for your own
        rule set and data sizes.

        Args:
            data (Union[pd.DataFrame, list[Mapping[str, Any]]]): The data to search for tags
            output_format (str): The format of the output. Supported values are 'list' and 'columns'
            output_column_name (str): The name of the column to store the tags

        Returns:
            pd.DataFrame: The data with an additional column 'tags' containing the assigned tags
        """
        if not self.__rules:
            raise ValueError("No rules to search for tags")

        if isinstance(data, list):
            df = pd.DataFrame(data)
        elif isinstance(data, pd.DataFrame):
            df = data
        else:
            raise ValueError(f"Unsupported data type: {type(data)}")

        tags = {}
        for rule in self.rules:
            if not rule.vector_check:
                raise ValueError("No vector check function found")
            tags[rule.id] = rule.vector_check(df)

        tags_df = pd.DataFrame(tags)

        if output_format == "list":
            # NaN (e.g. regex matches on missing values) stays truthy, as it
            # was when this used `if match` on each cell
            bool_matrix = tags_df.to_numpy(dtype=bool)
            tag_ids = tags_df.columns.to_numpy()
            df[output_column_name] = pd.Series(
                [list(tag_ids[row]) for row in bool_matrix], index=df.index
            )
            return df
        elif output_format == "columns":
            result_df = pd.concat([df, tags_df], axis=1)
            return result_df
        else:
            raise NotImplementedError(
                f"Output format '{output_format}' is not supported"
            )

    @property
    def rules(self) -> list[TagRule]:
        """Get the rules used to search for tags."""
        return self.__rules

    @property
    def rule_id_to_weight_map(self) -> dict[str, float]:
        """Get the mapping of rule id to weight."""
        return self.__rule_id_to_weight_map

    @property
    def rule_id_to_label_map(self) -> dict[str, str]:
        """Get the mapping of rule id to label."""
        return self.__rule_id_to_label_map

    def set_rules(self, rules: list[TagRule]) -> None:
        """Set the rules for the TagHound object.

        Args:
            rules (list[TagRule]): The rules to search tags with.
        """
        self.__rules = rules
        self.__required_fields = set().union(
            *(rule.required_fields for rule in self.__rules)
        )

        self.__rule_id_to_weight_map = {rule.id: rule.weight for rule in self.__rules}
        self.__rule_id_to_label_map = {rule.id: rule.label for rule in self.__rules}

    def add_rule(self, rule: TagRule) -> None:
        """Add a rule to the TagHound object.

        Args:
            rule (TagRule): The rule to add.
        """
        self.__rules.append(rule)
        self.__required_fields.update(rule.required_fields)

        self.__rule_id_to_weight_map[rule.id] = rule.weight
        self.__rule_id_to_label_map[rule.id] = rule.label

    @classmethod
    def _from_rule_dicts(
        cls,
        data: list[Mapping[str, Any]],
        version: str,
        add_scalar_func: bool,
        add_vector_fn: bool,
        merge_pattern: str,
    ) -> TagHound:
        """Create a TagHound from already-parsed rule dicts.

        Args:
            data (list[Mapping[str, Any]]): The parsed rule definitions.
            version (str): The version of the file format.
            add_scalar_func (bool): Whether to add scalar functions to the rules.
            add_vector_fn (bool): Whether to add vector functions to the rules.
            merge_pattern (str): The merge pattern to use for regex.

        Returns:
            TagHound: A TagHound object with the given rules.
        """
        if version != "1":
            raise ValueError(f"Unsupported version: {version}")
        rules = [
            to_tag_rule(
                rule_data=rule,
                add_scalar_func=add_scalar_func,
                add_vector_fn=add_vector_fn,
                merge_pattern=merge_pattern,
            )
            for rule in data
        ]
        return cls(rules=rules)

    @classmethod
    def rules_from_yaml(
        cls,
        filepath: str,
        version: str = "1",
        add_scalar_func: bool = True,
        add_vector_fn: bool = True,
        merge_pattern: str = DEFAULT_REGEX_MERGE_PATTERN,
    ) -> TagHound:
        """Load rules from a YAML file.

        Args:
            filepath (str): The file path to load the rules
            version (str): The version of the file format
            add_scalar_func (bool): Whether to add scalar functions to the rules
            add_vector_fn (bool): Whether to add vector functions to the rules
            merge_pattern (str): The merge pattern to use for regex

        Returns:
            TagHound: A TagHound object with the rules loaded from the file
        """
        import yaml

        data = yaml.safe_load(Path(filepath).read_text())
        return cls._from_rule_dicts(
            data,
            version=version,
            add_scalar_func=add_scalar_func,
            add_vector_fn=add_vector_fn,
            merge_pattern=merge_pattern,
        )

    @classmethod
    def rules_from_json(
        cls,
        filepath: str,
        version: str = "1",
        add_scalar_func: bool = True,
        add_vector_fn: bool = True,
        merge_pattern: str = DEFAULT_REGEX_MERGE_PATTERN,
    ) -> TagHound:
        """Load rules from a JSON file.

        Args:
            filepath (str): The file path to load the rules
            version (str): The version of the file format
            add_scalar_func (bool): Whether to add scalar functions to the rules
            add_vector_fn (bool): Whether to add vector functions to the rules
            merge_pattern (str): The merge pattern to use for regex

        Returns:
            TagHound: A TagHound object with the rules loaded from the file
        """
        import json

        data = json.loads(Path(filepath).read_text())
        return cls._from_rule_dicts(
            data,
            version=version,
            add_scalar_func=add_scalar_func,
            add_vector_fn=add_vector_fn,
            merge_pattern=merge_pattern,
        )
