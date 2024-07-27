import os
import sys

import pandas as pd

DIR_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.abspath(DIR_ROOT))


from taghound import TagHound  # noqa: E402


def main():
    cur_dir = os.path.dirname(__file__)
    th = TagHound.rules_from_yaml(os.path.join(cur_dir, "rules.yml"))

    print(os.path.join(cur_dir, "data.csv"))
    df = pd.read_csv(os.path.join(cur_dir, "data.csv"))
    df_with_tags = th.find_tags_using_vector(df)

    print(df_with_tags)
    #           name                                        description                                               tags
    # 0         Zeus  King of the gods, Zeus is the ruler of Mount O...  [role/god, domain/sky, zeus, zeus_or_hera, des...
    # 1         Hera  Queen of the gods, Hera is the goddess of marr...                       [role/goddess, zeus_or_hera]
    # 2     Poseidon  God of the sea, Poseidon rules the seas and co...            [role/god, domain/sea, description/god]
    # 3        Hades  God of the underworld, Hades governs the under...  [role/god, domain/underworld, description/god,...
    # 4       Athena  Goddess of wisdom, Athena is the goddess of wi...                      [role/goddess, domain/wisdom]
    # 5       Apollo  God of the sun and arts, Apollo is the god of ...     [role/god, description/god, description/music]
    # 6      Artemis  Goddess of the hunt, Artemis is the goddess of...                   [role/goddess, description/hunt]
    # 7         Ares   God of war, Ares is the god of war and violence.                        [role/god, description/god]
    # 8    Aphrodite  Goddess of love, Aphrodite is the goddess of l...                                     [role/goddess]
    # 9   Hephaestus  God of fire and forge, Hephaestus is the god o...      [role/god, description/god, description/fire]
    # 10      Hermes  Messenger of the gods, Hermes is the god of tr...                        [role/god, description/god]
    # 11     Demeter  Goddess of the harvest, Demeter is the goddess...                                     [role/goddess]
    # 12    Dionysus  God of wine, Dionysus is the god of wine, plea...                        [role/god, description/god]
    # 13  Persephone  Queen of the underworld, Persephone is the god...                  [role/goddess, domain/underworld]
    # 14      Helios  God of the sun, Helios is the god who drives t...            [role/god, domain/sky, description/god]
    # 15      Selene  Goddess of the moon, Selene is the goddess who...                         [role/goddess, domain/sky]
    # 16        Eros  God of love, Eros is the god of love and attra...                        [role/god, description/god]
    # 17      Hypnos  God of sleep, Hypnos is the personification of...                        [role/god, description/god]
    # 18        Nike  Goddess of victory, Nike is the goddess of vic...                                     [role/goddess]
    # 19       Tyche  Goddess of fortune, Tyche is the goddess of fo...                                     [role/goddess]
    # 20        Iris  Goddess of the rainbow, Iris is the messenger ...                                     [role/goddess]
    # 21         Pan  God of the wild, Pan is the god of the wild, s...                        [role/god, description/god]
    # 22     Nemesis  Goddess of retribution, Nemesis is the goddess...                                     [role/goddess]
    # 23      Hestia  Goddess of the hearth, Hestia is the goddess o...                                     [role/goddess]


if __name__ == "__main__":
    main()
