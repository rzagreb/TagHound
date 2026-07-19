# TagHound

Declarative tagging for Python: write matching rules in YAML or JSON, and TagHound attaches tags to your dicts or whole pandas DataFrames.

Rules stay readable and editable by non-technical users, while your pipeline code stays a one-liner. Typical uses:

- **Categorize bank transactions** — regex rules on merchant strings turn a CSV export into budget categories, no ML training required
- **Triage tickets and log events** — keyword and threshold rules attach routing tags (severity, team, topic) to each incoming record
- **Enrich scraped datasets** — bulk-tag job postings or product listings in a pandas pipeline, then rank matches by rule weights

## Installation

Requires Python 3.11+.

```bash
pip install taghound
```

Latest from source: `pip install git+https://github.com/rzagreb/TagHound.git`

## Quick start

Create `rules.yml`:

```yaml
- id: food/coffee
  label: Coffee
  weight: 3
  and:
    - key: merchant
      op: "~"
      value: starbucks|blue bottle

- id: alerts/big-purchase
  and:
    - key: amount
      op: ">"
      value: 100
```

Then:

```python
from taghound import TagHound

th = TagHound.rules_from_yaml("rules.yml")

print(th.find_all_tags({"merchant": "STARBUCKS #1234", "amount": 6.40}))
# ('food/coffee',)
print(th.find_all_tags({"merchant": "Delta Airlines", "amount": 420.00}))
# ('alerts/big-purchase',)
```

## Rule format

A rule is a unique `id` plus a tree of conditions under `and` / `or`, nested as deep as you need:

```yaml
- id: inventory/tall-tropical-tree  # required, unique; returned as the tag
  label: Tall tropical tree         # optional, defaults to id
  weight: 12                        # optional score, defaults to 0
  info: internal note, not matched  # optional
  and:
    - key: type
      value: tree                   # no `op` means `=`
    - or:
        - key: height
          op: ">"
          value: 20
        - key: location
          op: "~"
          value: tropical
```

### Operators

| Op | Meaning | Value types |
|---|---|---|
| `=` | equal (default when `op` is omitted) | int, float, str, bool |
| `!=` | not equal | int, float, str, bool |
| `>` `<` `>=` `<=` | numeric comparison | int, float |
| `in` | field value is in the list | list |
| `not_in` | field value is not in the list | list |
| `~` | regex match | str or list of str |
| `!~` | regex does not match | str or list of str |

Regex matching is case-insensitive, and a list value is OR-joined (`starbucks|blue bottle`). Patterns are wrapped in `(?<!\w)(?:...)(?!\w)` so they match whole words; pass `merge_pattern=r"{pattern}"` to `rules_from_yaml`/`rules_from_json` for raw substring behavior, or any other wrapper with a `{pattern}` placeholder.

Invalid rules (bad regex, unknown operator) raise at load time, not on first evaluation.

## Tagging a DataFrame

For large batches, evaluating a whole DataFrame at once is usually faster than calling `find_all_tags` per row (the break-even depends on your rules, so measure):

```python
import pandas as pd

df = pd.DataFrame([
    {"merchant": "Blue Bottle Coffee", "amount": 5.75},
    {"merchant": "Delta Airlines", "amount": 420.00},
])

print(th.find_tags_using_vector(df))
#              merchant  amount                   tags
# 0  Blue Bottle Coffee    5.75          [food/coffee]
# 1      Delta Airlines  420.00  [alerts/big-purchase]
```

The tags column is added to the input DataFrame in place; rename it with `output_column_name=`. Alternatively, `output_format="columns"` returns a copy with one boolean column per rule (`food/coffee`, `alerts/big-purchase`) — handy for filtering and aggregation.

## Scoring and labeling matches

Rule weights and labels are exposed as maps, so ranking matched records is a couple of lines:

```python
tags = th.find_all_tags({"merchant": "STARBUCKS #1234", "amount": 6.40})

score = sum(th.rule_id_to_weight_map[t] for t in tags)
labels = [th.rule_id_to_label_map[t] for t in tags]
print(score, labels)
# 3.0 ['Coffee']
```

## Rules in code

Skip the files entirely by building rules with any Python callable:

```python
from taghound import TagHound
from taghound.models import TagRule

rules = [
    TagRule(
        id="python_rule",
        label="Python",
        weight=10,
        required_fields={"language", "year"},
        scalar_check=lambda d: d["language"] == "python" and d["year"] > 1990,
    )
]

th = TagHound(rules=rules)
```

JSON works the same as YAML with an identical structure: `TagHound.rules_from_json("rules.json")`.

## Example

[examples/greek_gods](examples/greek_gods) tags a CSV of Greek gods with role/domain rules and prints the resulting DataFrame: `uv run python examples/greek_gods/attribute_tags.py`

## Development

```bash
uv sync      # set up the environment
just         # list recipes: test, lint, bench, profile, ...
just check   # lint + tests, same as CI
```

## License

MIT — see [LICENSE](LICENSE).
