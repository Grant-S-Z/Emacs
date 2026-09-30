#!/usr/bin/env python3
"""Convert ~/org/words-drill.org (org-drill format) into
~/org/fc/words-fc.org (org-fc format, double cards).

Reads only; writes the fc file. Category hierarchy is preserved as
non-card headings; review data is initialized afterwards by
`M-x org-fc-update-all`.
"""
import re
import os
import uuid
from datetime import datetime, timezone

NOW = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")

HOME = os.path.expanduser("~")
SRC = os.path.join(HOME, "org/words-drill.org")
DST_DIR = os.path.join(HOME, "org/fc")
DST = os.path.join(DST_DIR, "words-fc.org")

HEADING_RE = re.compile(r"^(\*+)\s+(.*?)\s*(:[a-zA-Z0-9:@%:]+:)?$")

cards = []
stack = []
current = None
card_level = 0
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
            current = {
                "word": title,
                "path": list(stack),
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
            low = title.lower()
            section = {"english": "english", "中文释义": "meaning",
                       "例句": "examples"}[low]
            continue

        current = None
        section = None
        stack = [h for h in stack if h[0] < level]
        stack.append((level, title))

def tidy(lines):
    return "\n".join(lines).strip("\n").strip()

os.makedirs(DST_DIR, exist_ok=True)

out = ["#+title: Words (org-fc)",
       "",
       "由 words-drill.org 转换而来。复习：=C-c v f=（org-fc-review）。",
       "卡片为 double 类型，正反两个方向都会考。",
       ""]

n = 0
last_path = []
seen = set()
for c in cards:
    word = re.sub(r"\*+", "", c["word"]).strip()
    if not word or word.lower() in seen:
        continue
    seen.add(word.lower())

    path = c["path"]
    common = 0
    for a, b in zip(last_path, path):
        if a == b:
            common += 1
        else:
            break
    for level, title in path[common:]:
        out.append("*" * level + " " + title)
    last_path = path

    level = (path[-1][0] + 1) if path else 1
    stars = "*" * level
    taglist = ":fc:"
    if ":known:" in c["tags"]:
        taglist += "known:"
    if ":todo:" in c["tags"]:
        taglist += "todo:"

    front = tidy(c["english"]) or word
    # 标题已是单词本身，正文不再重复；只保留音标和相关词
    if front == word:
        front = ""
    elif front.startswith(word + " /"):
        front = front[len(word):].strip()
    back_parts = []
    meaning = tidy(c["meaning"])
    if meaning and meaning != "TODO":
        back_parts.append(meaning)
    examples = tidy(c["examples"])
    if examples:
        back_parts.append(examples)
    back = "\n\n".join(back_parts) if back_parts else "(待补充)"

    out.append(f"{stars} {word} {taglist}")
    out.append(":PROPERTIES:")
    out.append(":FC_TYPE:  double")
    out.append(":FC_ALGO:  sm2")
    out.append(f":ID:       {uuid.uuid4()}")
    out.append(f":FC_CREATED: {NOW}")
    out.append(":END:")
    out.append(":REVIEW_DATA:")
    out.append("| position | ease | box | interval | due" + " " * 19 + "|")
    out.append("|----------+------+-----+----------+----------------------|")
    out.append(f"| front    | 2.50 |   0 |     0.00 | {NOW} |")
    out.append(f"| back     | 2.50 |   0 |     0.00 | {NOW} |")
    out.append(":END:")
    out.append("")
    out.append(front)
    out.append("")
    out.append("*" * (level + 1) + " Back")
    out.append("")
    out.append(back)
    out.append("")
    n += 1

with open(DST, "w", encoding="utf-8") as f:
    f.write("\n".join(out))
print(f"wrote {DST}: {n} double cards")
