#!/usr/bin/env python3
"""Convert ~/org/words-drill.org (org-drill format) into
~/org/words-anki.org (anki-editor format) for pushing to Anki.

Reads only; writes the anki file. Decks are derived from the category
hierarchy, e.g. Words::Math::Linear algebra.
"""
import re
import os

HOME = os.path.expanduser("~")
SRC = os.path.join(HOME, "org/words-drill.org")
DST = os.path.join(HOME, "org/words-anki.org")

DATE_RE = re.compile(r"^<20\d\d-\d\d-\d\d")
HEADING_RE = re.compile(r"^(\*+)\s+(.*?)\s*(:[a-zA-Z0-9:@%:]+:)?$")

def clean_deck_part(title):
    return title.replace("::", "-").strip()

cards = []
stack = []        # category headings: [(level, title)]
current = None    # current card dict
card_level = 0    # heading level of current card
section = None

with open(SRC, encoding="utf-8") as f:
    for raw in f:
        line = raw.rstrip("\n")
        m = HEADING_RE.match(line)
        if not m:
            if current is not None and section:
                current[section].append(line)
            continue

        level, title, tags = len(m.group(1)), m.group(2).strip(), m.group(3) or ""

        if ":drill:" in tags:
            # new card; parent chain is the category stack
            current = {
                "word": title,
                "path": [t for _, t in stack],
                "tags": tags,
                "english": [],
                "meaning": [],
                "examples": [],
            }
            cards.append(current)
            card_level = level
            section = None
            continue

        if current is not None and level == card_level + 1 and \
           title.lower() in ("english", "中文释义", "例句"):
            # card subsection; does not affect the category stack
            low = title.lower()
            section = {"english": "english", "中文释义": "meaning",
                       "例句": "examples"}[low]
            continue

        # genuine category heading
        current = None
        section = None
        stack = [h for h in stack if h[0] < level]
        stack.append((level, title))

def tidy(lines):
    return "\n".join(lines).strip("\n").strip()

out = ["#+title: Words (Anki)",
       "",
       "由 words-drill.org 转换而来。打开 Anki 桌面版后，",
       "在本文件内执行 =C-c v p=（anki-editor-push-notes）推送到 Anki。",
       ""]

n = 0
seen = set()
for c in cards:
    word = re.sub(r"\*+", "", c["word"]).strip()
    if not word:
        continue
    # Anki rejects notes whose first field duplicates an existing note
    dedup_key = word.lower()
    if dedup_key in seen:
        continue
    seen.add(dedup_key)
    deck_parts = [clean_deck_part(t) for t in c["path"] if not DATE_RE.match(t)]
    deck = "::".join(["Words"] + [p for p in deck_parts if p])
    tags = []
    if ":known:" in c["tags"]:
        tags.append("known")
    if ":todo:" in c["tags"]:
        tags.append("todo")
    front = tidy(c["english"]) or word
    back_parts = []
    meaning = tidy(c["meaning"])
    if meaning and meaning != "TODO":
        back_parts.append(meaning)
    examples = tidy(c["examples"])
    if examples:
        back_parts.append(examples)
    back = "\n\n".join(back_parts) if back_parts else "(待补充)"

    out.append(f"* {word}")
    out.append(":PROPERTIES:")
    out.append(f":ANKI_DECK: {deck}")
    out.append(":ANKI_NOTE_TYPE: 问答题（附翻转卡片）")
    if tags:
        out.append(f":ANKI_TAGS: {' '.join(tags)}")
    out.append(":END:")
    out.append("** 正面")
    out.append(front)
    out.append("** 背面")
    out.append(back)
    out.append("")
    n += 1

with open(DST, "w", encoding="utf-8") as f:
    f.write("\n".join(out))
print(f"wrote {DST}: {n} notes")

decks = sorted({re.search(r":ANKI_DECK: (.+)", l).group(1)
                for l in out if l.startswith(":ANKI_DECK:")})
print(f"decks: {len(decks)}")
for d in decks:
    print(" ", d)
