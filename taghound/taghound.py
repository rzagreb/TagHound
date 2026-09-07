"""TagHound: evaluate tag rules against single mappings or whole DataFrames."""

from __future__ import annotations

import json
from collections.abc import Mapping
from pathlib import Path
from typing import Any

import pandas as pd
import yaml

from taghound.constants import (
    DEFAULT_REGEX_MERGE_PATTERN,
    RE2_SAFE_REGEX_MERGE_PATTERN,
)
from taghound.exceptions import DuplicateRuleIdError
from taghound.models import TagRule
from taghound.serializers import to_tag_rule


class TagHound:
    """Holds a set of tag rules and finds the tags matching given data.

    The `rules_from_*` loaders share four knobs. `version` is the rule file format
    version, and only "1" exists. `add_scalar_func` and `add_vector_fn` choose which
    of the two check functions each rule gets built. `merge_pattern` wraps every
    regex on the scalar path, which runs on Python `re`; `vector_merge_pattern` does
    the same for the vectorized path, which runs on RE2 for pyarrow-backed columns
    and therefore has to stay RE2-safe.
    """

    def __init__(self, rules: list[TagRule]) -> None:
        """Hold `rules` and build the lookups derived from them."""
        self.set_rules(rules)

    def find_all_tags(self, data: Mapping[str, Any]) -> tuple[str, ...]:
        """Return the ids of every rule matching one mapping.

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

        if not self.__has_scalar_checks:
            raise ValueError(
                "Some rules have no scalar check function, so they were loaded with "
                "add_scalar_func=False. Reload with it on, or use find_tags_using_vector."
            )

        if not self.__required_fields.issubset(data.keys()):
            raise ValueError(
                f"Missing required fields: {self.__required_fields - data.keys()}"
            )

        return tuple(rule.id for rule in self.__rules if rule.scalar_check(data))  # type: ignore

    def find_tags_using_vector(
        self,
        data: pd.DataFrame | list[Mapping[str, Any]],
        output_format: str = "list",
        output_column_name: str = "tags",
    ) -> pd.DataFrame:
        """Find tags across a whole DataFrame at once.

        Every rule is evaluated against all rows in one pass, which usually pays off
        over `find_all_tags` for large batches; measure for your own rule set and
        data sizes.

        `output_format="list"` names the matching rule ids in a single column and
        returns the caller's own DataFrame, mutated in place. `"columns"` returns a
        copy carrying one boolean column per rule.
        """
        if not self.__rules:
            raise ValueError("No rules to search for tags")

        if isinstance(data, list):
            df = pd.DataFrame(data)
        elif isinstance(data, pd.DataFrame):
            df = data
        else:
            # TypeError would be the better fit, but this has been a ValueError
            # since 0.1 and callers catch it; same reasoning as the ValueError /
            # TypeError bases in exceptions.py.
            raise ValueError(f"Unsupported data type: {type(data)}")  # noqa: TRY004

        tags = {}
        for rule in self.__rules:
            if not rule.vector_check:
                raise ValueError("No vector check function found")
            tags[rule.id] = rule.vector_check(df)

        tags_df = pd.DataFrame(tags)

        if output_format == "list":
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
        """Replace the rule set and rebuild everything derived from it."""
        self.__rules = rules
        self.__required_fields = set().union(
            *(rule.required_fields for rule in self.__rules)
        )
        self.__has_scalar_checks = all(
            rule.scalar_check is not None for rule in self.__rules
        )

        self.__rule_id_to_weight_map = {rule.id: rule.weight for rule in self.__rules}
        self.__rule_id_to_label_map = {rule.id: rule.label for rule in self.__rules}

    def add_rule(self, rule: TagRule) -> None:
        """Append one rule and update the lookups derived from the set.

        There is no duplicate-`id` check here; that belongs to the loaders, which
        see the whole set at once. Adding an id already present leaves the weight
        and label maps holding the newcomer while `output_format="columns"` emits
        the id twice.
        """
        self.__rules.append(rule)
        self.__required_fields.update(rule.required_fields)
        self.__has_scalar_checks &= rule.scalar_check is not None

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
        vector_merge_pattern: str,
    ) -> TagHound:
        """Build a TagHound from already-parsed rule dicts.

        The single funnel every loader passes through, so cross-rule checks such as
        the duplicate-`id` refusal belong here rather than in each `rules_from_*`.
        """
        if version != "1":
            raise ValueError(f"Unsupported version: {version}")
        rules = [
            to_tag_rule(
                rule_data=rule,
                add_scalar_func=add_scalar_func,
                add_vector_fn=add_vector_fn,
                merge_pattern=merge_pattern,
                vector_merge_pattern=vector_merge_pattern,
            )
            for rule in data
        ]

        seen: set[str] = set()
        for rule in rules:
            if rule.id in seen:
                raise DuplicateRuleIdError(
                    f"`{rule.id}` appears twice. An id is what a rule is keyed on, "
                    "so only one of the two would survive: the weight and label "
                    "maps keep the last, and `output_format='columns'` emits the "
                    "id twice."
                )
            seen.add(rule.id)

        return cls(rules=rules)

    @classmethod
    def rules_from_yaml(
        cls,
        filepath: str,
        version: str = "1",
        add_scalar_func: bool = True,
        add_vector_fn: bool = True,
        merge_pattern: str = DEFAULT_REGEX_MERGE_PATTERN,
        vector_merge_pattern: str = RE2_SAFE_REGEX_MERGE_PATTERN,
    ) -> TagHound:
        """Load rules from a YAML file. See the class docstring for the knobs."""
        return cls._from_rule_dicts(
            yaml.safe_load(Path(filepath).read_text()),
            version=version,
            add_scalar_func=add_scalar_func,
            add_vector_fn=add_vector_fn,
            merge_pattern=merge_pattern,
            vector_merge_pattern=vector_merge_pattern,
        )

    @classmethod
    def rules_from_string(
        cls,
        text: str,
        version: str = "1",
        add_scalar_func: bool = True,
        add_vector_fn: bool = True,
        merge_pattern: str = DEFAULT_REGEX_MERGE_PATTERN,
        vector_merge_pattern: str = RE2_SAFE_REGEX_MERGE_PATTERN,
    ) -> TagHound:
        """Load rules from YAML text, for rules that never were a file.

        The counterpart to `taghound.serializers.dump_rules`, and what to use when
        the rules arrive from a database row or an upload.
        """
        return cls._from_rule_dicts(
            yaml.safe_load(text),
            version=version,
            add_scalar_func=add_scalar_func,
            add_vector_fn=add_vector_fn,
            merge_pattern=merge_pattern,
            vector_merge_pattern=vector_merge_pattern,
        )

    @classmethod
    def rules_from_json(
        cls,
        filepath: str,
        version: str = "1",
        add_scalar_func: bool = True,
        add_vector_fn: bool = True,
        merge_pattern: str = DEFAULT_REGEX_MERGE_PATTERN,
        vector_merge_pattern: str = RE2_SAFE_REGEX_MERGE_PATTERN,
    ) -> TagHound:
        """Load rules from a JSON file. See the class docstring for the knobs."""
        return cls._from_rule_dicts(
            json.loads(Path(filepath).read_text()),
            version=version,
            add_scalar_func=add_scalar_func,
            add_vector_fn=add_vector_fn,
            merge_pattern=merge_pattern,
            vector_merge_pattern=vector_merge_pattern,
        )
