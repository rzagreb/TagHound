"""Tag a CSV of Greek gods using the rules in rules.yml."""

# ruff: noqa: T201 -- example script; the printed DataFrame is the demo output

from pathlib import Path

import pandas as pd

from taghound import TagHound


def main() -> None:
    """Load the example rules and data, then print the tagged DataFrame."""
    cur_dir = Path(__file__).parent
    th = TagHound.rules_from_yaml(str(cur_dir / "rules.yml"))

    df = pd.read_csv(cur_dir / "data.csv")
    df_with_tags = th.find_tags_using_vector(df)

    print(df_with_tags)
    #           name                                        description                                               tags
    # 0         Zeus  King of the gods, Zeus is the ruler of Mount O...  [role/god, domain/sky, zeus, zeus_or_hera, des...
    # 1         Hera  Queen of the gods, Hera is the goddess of marr...                       [role/goddess, zeus_or_hera]
    # 2     Poseidon  God of the sea, Poseidon rules the seas and co...            [role/god, domain/sea, description/god]
    # 3        Hades  God of the underworld, Hades governs the under...  [role/god, domain/underworld, description/god,...
    # ...


if __name__ == "__main__":
    main()
