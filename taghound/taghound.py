from __future__ import annotations

from typing import Any, Mapping, Optional, Union

import pandas as pd

from taghound.constants import DEFAULT_REGEX_MERGE_PATTERN
from taghound.models import TagRule
from taghound.serializers import to_tag_rule


class TagHound:
    def __init__(self, rules: list[TagRule]) -> None:
        self.set_rules(rules)

    def find_all_tags(
        self,
        data: Mapping[str, Any],
        multi_threaded: bool = False,
        thread_exec_params: Optional[dict] = None,
    ) -> tuple[str, ...]:
        """Find all tags in the data

        Args:
            data (Mapping[str, Any]): The data to search for tags
            multi_threaded (bool): Whether to use multi-threading to search for tags
                NOTE: give about 12% speedup on average
            thread_exec_params (Optional[dict]): The parameters to pass to the ThreadPoolExecutor

        Returns:
            tuple[TagFound, ...]: A set of TagFound objects found in the data

        Example:
            ```python
            rules = [
                TagRule(
                    name="python",
                    weight=10,
                    required_fields={"language", "year"},
                    condition_function=lambda d: d["language"] == "python" and d["year"] > 1990,
                )
            ]
            data = {
                "language": "python",
                "year": 1991,
                "version": "3.10",
            }
            th = TagHound(rules)
            tags = th.find_all_tags(data)
            ```

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
        data: Union[pd.DataFrame, list[Mapping[str, Any]]],
        output_format: str = "list",
        output_column_name: str = "tags",
    ) -> pd.DataFrame:
        """Find tags in the data using vectorized operations
        NOTE: about 94% faster than scalar methods for 1,000 rows with 1,000 tags
        NOTE: About the same for 25 rows with 1,000 tags

        Args:
            data (Union[pd.DataFrame, list[Mapping[str, Any]]]): The data to search for tags
            output_format (str): The format of the output. Supported values are 'list' and 'columns'
            output_column_name (str): The name of the column to store the tags

        Returns:
            pd.DataFrame: The data with an additional column 'tags' containing the assigned tags
        """
        if isinstance(data, list):
            df = pd.DataFrame(data)
        elif isinstance(data, pd.DataFrame):
            df = data
        else:
            raise ValueError(f"Unsupported data type: {type(data)}")

        tags = {}
        for rule in self.rules:
            try:
                tags[rule.id] = rule.vector_check(df)  # type: ignore
            except Exception:
                raise

        tags_df = pd.DataFrame(tags)

        if output_format == "list":
            tags_series = tags_df.apply(
                lambda row: [tag for tag, match in row.items() if match], axis=1
            )
            df[output_column_name] = tags_series
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
        return self.__rules

    @property
    def rule_id_to_weight_map(self) -> dict[str, float]:
        """Get the mapping of rule id to weight"""
        return self.__rule_id_to_weight_map

    @property
    def rule_id_to_label_map(self) -> dict[str, str]:
        """Get the mapping of rule id to label"""
        return self.__rule_id_to_label_map

    def set_rules(self, rules: list[TagRule]) -> None:
        """Set the rules for the TagHound object"""
        self.__rules = rules
        self.__required_fields = set().union(
            *(rule.required_fields for rule in self.__rules)
        )

        self.__rule_id_to_weight_map = {rule.id: rule.weight for rule in self.__rules}
        self.__rule_id_to_label_map = {rule.id: rule.label for rule in self.__rules}

    def add_rule(self, rule: TagRule) -> None:
        """Add a rule to the TagHound object"""
        self.__rules.append(rule)
        self.__required_fields.update(rule.required_fields)

        self.__rule_id_to_weight_map[rule.id] = rule.weight
        self.__rule_id_to_label_map[rule.id] = rule.label

    @classmethod
    def rules_from_yaml(
        cls,
        filepath: str,
        version: str = "1",
        add_scalar_func: bool = True,
        add_vector_fn: bool = True,
        merge_pattern: str = DEFAULT_REGEX_MERGE_PATTERN,
    ) -> "TagHound":
        """Load rules from a YAML file

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

        with open(filepath, "r") as f:
            data = yaml.safe_load(f)
        if version == "1":
            rules = [
                to_tag_rule(
                    rule_data=rule,
                    add_scalar_func=add_scalar_func,
                    add_vector_fn=add_vector_fn,
                    merge_pattern=merge_pattern,
                )
                for rule in data
            ]
        else:
            raise ValueError(f"Unsupported version: {version}")
        return TagHound(rules=rules)

    @classmethod
    def rules_from_json(
        cls,
        filepath: str,
        version: str = "1",
        add_scalar_func: bool = True,
        add_vector_fn: bool = True,
        merge_pattern: str = DEFAULT_REGEX_MERGE_PATTERN,
    ) -> "TagHound":
        """Load rules from a JSON file

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

        with open(filepath, "r") as f:
            data = json.load(f)
        if version == "1":
            rules = [
                to_tag_rule(
                    rule_data=rule,
                    add_scalar_func=add_scalar_func,
                    add_vector_fn=add_vector_fn,
                    merge_pattern=merge_pattern,
                )
                for rule in data
            ]
        else:
            raise ValueError(f"Unsupported version: {version}")
        return TagHound(rules=rules)
