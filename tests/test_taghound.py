from __future__ import annotations

import json
import os
import re
import tempfile
import unittest

import context  # type: ignore  # noqa: F401
import yaml

from taghound.constants import ComparisonKey, LogicalOperator, RuleKey
from taghound.models import TagFound, TagRule
from taghound.taghound import TagHound


class TestTagHound(unittest.TestCase):
    def setUp(self):
        self.rules = [
            TagRule(
                id="sector_group/advanced_filtering2",
                label="Advanced Filtering 2",
                weight=-150,
                required_fields={
                    "description",
                    "title",
                    "location",
                    "clearance_required",
                    "clearance_level",
                },
                scalar_check=lambda d: (
                    bool(
                        re.search(
                            "(government contract|public sector)", d["description"]
                        )
                    )
                    and not bool(re.search("(data|analyst|intern|junior)", d["title"]))
                    and d["location"] in ["Washington", "New York"]
                    and (
                        d["clearance_required"] == "false"
                        or (
                            d["clearance_required"] == "true"
                            and d["clearance_level"] == "Top Secret"
                        )
                    )
                ),
            )
        ]
        self.th = TagHound(rules=self.rules)

    def test_find_all_tags(self):
        data = {
            "description": "government contract",
            "title": "senior developer",
            "location": "Washington",
            "clearance_required": "true",
            "clearance_level": "Top Secret",
        }
        expected_tags = (
            TagFound(id="sector_group/advanced_filtering2", rule=self.rules[0]),
        )
        tags = self.th.find_all_tags(data)
        self.assertEqual(tags, expected_tags)

        data = {
            "description": "public sector",
            "title": "junior developer",
            "location": "New York",
            "clearance_required": "false",
            "clearance_level": "Top Secret",
        }
        expected_tags = tuple()  # Because title contains "junior"
        tags = self.th.find_all_tags(data)
        self.assertEqual(tags, expected_tags)

        data = {
            "description": "public sector",
            "title": "senior developer",
            "location": "Chicago",
            "clearance_required": "false",
            "clearance_level": "Top Secret",
        }
        expected_tags = tuple()  # Because location is not Washington or New York
        tags = self.th.find_all_tags(data)
        self.assertEqual(tags, expected_tags)

    def test_rules_from_yaml(self):
        yaml_data = [
            {
                RuleKey.ID.value: "sector_group/advanced_filtering2",
                RuleKey.LABEL.value: "Advanced Filtering 2",
                RuleKey.WEIGHT.value: -150,
                LogicalOperator.OR.value: [
                    {
                        LogicalOperator.AND.value: [
                            {
                                ComparisonKey.KEY.value: "description",
                                ComparisonKey.OPERATOR.value: "~",
                                ComparisonKey.VALUE.value: "(government contract|public sector)",
                            }
                        ]
                    },
                    {
                        LogicalOperator.AND.value: [
                            {
                                ComparisonKey.KEY.value: "description",
                                ComparisonKey.OPERATOR.value: "~",
                                ComparisonKey.VALUE.value: "(government contract|public sector)",
                            },
                            {
                                ComparisonKey.KEY.value: "title",
                                ComparisonKey.OPERATOR.value: "!~",
                                ComparisonKey.VALUE.value: [
                                    "data",
                                    "analyst",
                                    "intern",
                                    "junior",
                                ],
                            },
                            {
                                ComparisonKey.KEY.value: "location",
                                ComparisonKey.OPERATOR.value: "in",
                                ComparisonKey.VALUE.value: [
                                    "Washington",
                                    "New York",
                                ],
                            },
                            {
                                LogicalOperator.OR.value: [
                                    {
                                        ComparisonKey.KEY.value: "clearance_required",
                                        ComparisonKey.OPERATOR.value: "=",
                                        ComparisonKey.VALUE.value: "false",
                                    },
                                    {
                                        LogicalOperator.AND.value: [
                                            {
                                                ComparisonKey.KEY.value: "clearance_required",
                                                ComparisonKey.OPERATOR.value: "=",
                                                ComparisonKey.VALUE.value: "true",
                                            },
                                            {
                                                ComparisonKey.KEY.value: "clearance_level",
                                                ComparisonKey.OPERATOR.value: "=",
                                                ComparisonKey.VALUE.value: "Top Secret",
                                            },
                                        ]
                                    },
                                ]
                            },
                        ]
                    },
                ],
            }
        ]

        with tempfile.NamedTemporaryFile(delete=False, mode="w") as tmp_file:
            yaml.dump(yaml_data, tmp_file)
            tmp_file.close()
            taghound = TagHound.rules_from_yaml(tmp_file.name)
            self.assertEqual(len(taghound.rules), len(self.rules))
            self.assertEqual(taghound.rules[0].id, self.rules[0].id)
            self.assertEqual(taghound.rules[0].weight, self.rules[0].weight)
            os.remove(tmp_file.name)

    def test_rules_from_json(self):
        json_data = [
            {
                RuleKey.ID.value: "sector_group/advanced_filtering2",
                RuleKey.LABEL.value: "Advanced Filtering 2",
                RuleKey.WEIGHT.value: -150,
                LogicalOperator.OR.value: [
                    {
                        LogicalOperator.AND.value: [
                            {
                                ComparisonKey.KEY.value: "description",
                                ComparisonKey.OPERATOR.value: "~",
                                ComparisonKey.VALUE.value: "(government contract|public sector)",
                            }
                        ]
                    },
                    {
                        LogicalOperator.AND.value: [
                            {
                                ComparisonKey.KEY.value: "description",
                                ComparisonKey.OPERATOR.value: "~",
                                ComparisonKey.VALUE.value: "(government contract|public sector)",
                            },
                            {
                                ComparisonKey.KEY.value: "title",
                                ComparisonKey.OPERATOR.value: "!~",
                                ComparisonKey.VALUE.value: [
                                    "data",
                                    "analyst",
                                    "intern",
                                    "junior",
                                ],
                            },
                            {
                                ComparisonKey.KEY.value: "location",
                                ComparisonKey.OPERATOR.value: "in",
                                ComparisonKey.VALUE.value: [
                                    "Washington",
                                    "New York",
                                ],
                            },
                            {
                                LogicalOperator.OR.value: [
                                    {
                                        ComparisonKey.KEY.value: "clearance_required",
                                        ComparisonKey.OPERATOR.value: "=",
                                        ComparisonKey.VALUE.value: "false",
                                    },
                                    {
                                        LogicalOperator.AND.value: [
                                            {
                                                ComparisonKey.KEY.value: "clearance_required",
                                                ComparisonKey.OPERATOR.value: "=",
                                                ComparisonKey.VALUE.value: "true",
                                            },
                                            {
                                                ComparisonKey.KEY.value: "clearance_level",
                                                ComparisonKey.OPERATOR.value: "=",
                                                ComparisonKey.VALUE.value: "Top Secret",
                                            },
                                        ]
                                    },
                                ]
                            },
                        ]
                    },
                ],
            }
        ]

        with tempfile.NamedTemporaryFile(delete=False, mode="w") as tmp_file:
            json.dump(json_data, tmp_file)
            tmp_file.close()
            taghound = TagHound.rules_from_json(tmp_file.name)
            self.assertEqual(len(taghound.rules), len(self.rules))
            self.assertEqual(taghound.rules[0].id, self.rules[0].id)
            self.assertEqual(taghound.rules[0].weight, self.rules[0].weight)
            os.remove(tmp_file.name)

    def test_rules_from_yaml_invalid_version(self):
        yaml_data = [
            {
                RuleKey.ID.value: "sector_group/advanced_filtering2",
                RuleKey.WEIGHT.value: -150,
                LogicalOperator.OR.value: [
                    {
                        LogicalOperator.AND.value: [
                            {
                                ComparisonKey.KEY.value: "description",
                                ComparisonKey.OPERATOR.value: "~",
                                ComparisonKey.VALUE.value: "(government contract|public sector)",
                            }
                        ]
                    },
                    {
                        LogicalOperator.AND.value: [
                            {
                                ComparisonKey.KEY.value: "description",
                                ComparisonKey.OPERATOR.value: "~",
                                ComparisonKey.VALUE.value: "(government contract|public sector)",
                            },
                            {
                                ComparisonKey.KEY.value: "title",
                                ComparisonKey.OPERATOR.value: "!~",
                                ComparisonKey.VALUE.value: [
                                    "data",
                                    "analyst",
                                    "intern",
                                    "junior",
                                ],
                            },
                            {
                                ComparisonKey.KEY.value: "location",
                                ComparisonKey.OPERATOR.value: "in",
                                ComparisonKey.VALUE.value: [
                                    "Washington",
                                    "New York",
                                ],
                            },
                            {
                                LogicalOperator.OR.value: [
                                    {
                                        ComparisonKey.KEY.value: "clearance_required",
                                        ComparisonKey.OPERATOR.value: "=",
                                        ComparisonKey.VALUE.value: "false",
                                    },
                                    {
                                        LogicalOperator.AND.value: [
                                            {
                                                ComparisonKey.KEY.value: "clearance_required",
                                                ComparisonKey.OPERATOR.value: "=",
                                                ComparisonKey.VALUE.value: "true",
                                            },
                                            {
                                                ComparisonKey.KEY.value: "clearance_level",
                                                ComparisonKey.OPERATOR.value: "=",
                                                ComparisonKey.VALUE.value: "Top Secret",
                                            },
                                        ]
                                    },
                                ]
                            },
                        ]
                    },
                ],
            }
        ]

        with tempfile.NamedTemporaryFile(delete=False, mode="w") as tmp_file:
            yaml.dump(yaml_data, tmp_file)
            tmp_file.close()
            with self.assertRaises(ValueError):
                TagHound.rules_from_yaml(tmp_file.name, version="2")
            os.remove(tmp_file.name)

    def test_rules_from_json_invalid_version(self):
        json_data = [
            {
                RuleKey.ID.value: "sector_group/advanced_filtering2",
                RuleKey.WEIGHT.value: -150,
                LogicalOperator.OR.value: [
                    {
                        LogicalOperator.AND.value: [
                            {
                                ComparisonKey.KEY.value: "description",
                                ComparisonKey.OPERATOR.value: "~",
                                ComparisonKey.VALUE.value: "(government contract|public sector)",
                            }
                        ]
                    },
                    {
                        LogicalOperator.AND.value: [
                            {
                                ComparisonKey.KEY.value: "description",
                                ComparisonKey.OPERATOR.value: "~",
                                ComparisonKey.VALUE.value: "(government contract|public sector)",
                            },
                            {
                                ComparisonKey.KEY.value: "title",
                                ComparisonKey.OPERATOR.value: "!~",
                                ComparisonKey.VALUE.value: [
                                    "data",
                                    "analyst",
                                    "intern",
                                    "junior",
                                ],
                            },
                            {
                                ComparisonKey.KEY.value: "location",
                                ComparisonKey.OPERATOR.value: "in",
                                ComparisonKey.VALUE.value: [
                                    "Washington",
                                    "New York",
                                ],
                            },
                            {
                                LogicalOperator.OR.value: [
                                    {
                                        ComparisonKey.KEY.value: "clearance_required",
                                        ComparisonKey.OPERATOR.value: "=",
                                        ComparisonKey.VALUE.value: "false",
                                    },
                                    {
                                        LogicalOperator.AND.value: [
                                            {
                                                ComparisonKey.KEY.value: "clearance_required",
                                                ComparisonKey.OPERATOR.value: "=",
                                                ComparisonKey.VALUE.value: "true",
                                            },
                                            {
                                                ComparisonKey.KEY.value: "clearance_level",
                                                ComparisonKey.OPERATOR.value: "=",
                                                ComparisonKey.VALUE.value: "Top Secret",
                                            },
                                        ]
                                    },
                                ]
                            },
                        ]
                    },
                ],
            }
        ]

        with tempfile.NamedTemporaryFile(delete=False, mode="w") as tmp_file:
            json.dump(json_data, tmp_file)
            tmp_file.close()
            with self.assertRaises(ValueError):
                TagHound.rules_from_json(tmp_file.name, version="2")
            os.remove(tmp_file.name)


if __name__ == "__main__":
    unittest.main()
