#!/usr/bin/env python3
"""Convert ~/org/words.org checklist vocabulary into org-drill twosided cards.

Reads the original file without modifying it; writes ~/org/words-drill.org.
Chinese definitions are looked up from the word lists in
~/.emacs.d/english-wordlists (COCA_with_translation, TOEFL, CET_4+6).
"""
import re
import os

HOME = os.path.expanduser("~")
SRC = os.path.join(HOME, "org/words.org")
DST = os.path.join(HOME, "org/words-drill.org")
WL = os.path.join(HOME, ".emacs.d/english-wordlists")

# ---------------------------------------------------------------- dictionary
def load_inline_dict(path):
    """Lines like: word [phonetic] pos.meaning  (TOEFL.txt, CET_4+6_edited.txt)"""
    d = {}
    with open(path, encoding="utf-8", errors="ignore") as f:
        for line in f:
            line = line.strip()
            m = re.match(r"^([A-Za-z][A-Za-z' -]*?)\s+(?:\[([^\]]*)\]\s*)?(\S.*?[一-鿿].*)$", line)
            if m:
                word, phon, meaning = m.group(1), m.group(2), m.group(3)
                key = word.lower()
                if key not in d:
                    d[key] = (phon or "", meaning.strip())
    return d

def load_coca(path):
    """COCA_with_translation.txt: word line, then 'pos. meaning' lines."""
    d = {}
    word = None
    with open(path, encoding="utf-8", errors="ignore") as f:
        for raw in f:
            line = raw.rstrip("\n")
            if re.match(r"^[A-Za-z][A-Za-z' -]*$", line.strip()) and line == line.strip() and not line.startswith(" "):
                cand = line.strip()
                # COCA file: word lines are flush-left single tokens
                word = cand.lower()
            elif word and re.match(r"^\s*\S+\.\s*", line) and re.search(r"[一-鿿]", line):
                if word not in d:
                    d[word] = ("", re.sub(r"^\s+", "", line))
                word = None  # take only the first sense line
    return d

def load_gre_csv(path):
    """红宝书 GRE词汇精选.csv: word,pos.meaning"""
    d = {}
    with open(path, encoding="utf-8", errors="ignore") as f:
        for line in f:
            line = line.strip()
            m = re.match(r"^([A-Za-z][A-Za-z' -]*),(\S.*)$", line)
            if m and re.search(r"[一-鿿]", m.group(2)):
                key = m.group(1).lower()
                if key not in d:
                    d[key] = ("", m.group(2).strip())
    return d

dictionary = {}
for loader, name in [
    (load_coca, "COCA_with_translation.txt"),
    (load_inline_dict, "TOEFL.txt"),
    (load_inline_dict, "NPEE_Wordlist.txt"),
    (load_gre_csv, "红宝书 GRE词汇精选.csv"),
]:
    path = os.path.join(WL, name)
    if os.path.exists(path):
        sub = loader(path)
        for k, v in sub.items():
            dictionary.setdefault(k, v)
        print(f"{name}: {len(sub)} entries")
print(f"dictionary total: {len(dictionary)}")

# ---------------------------------------------------------------- parsing
ANNOT_RE = re.compile(r"(=([^=]+)=|/([^/]+)/|~([^~]+)~|_([^_]+)_)")

def split_annotations(text):
    """Return (headword, related[list], examples[list]) from an item line."""
    related, examples = [], []
    def repl(m):
        inner = next(g for g in m.groups()[1:] if g is not None)
        if m.group(0).startswith("="):
            related.append(inner.strip())
        else:  # /.../ ~...~ _..._ are usage phrases/examples
            examples.append(inner.strip())
        return ""
    head = ANNOT_RE.sub(repl, text)
    head = re.sub(r"\s+", " ", head).strip().rstrip(".")
    return head, related, examples

cards = []          # list of dicts
heading_stack = []  # list of (level, title)
current = None

with open(SRC, encoding="utf-8") as f:
    lines = f.readlines()

for line in lines:
    raw = line.rstrip("\n")
    m = re.match(r"^(\*+)\s+(.*)$", raw)
    if m:
        level, title = len(m.group(1)), m.group(2).strip()
        heading_stack = [h for h in heading_stack if h[0] < level]
        heading_stack.append((level, title))
        current = None
        continue
    m = re.match(r"^- \[([ X])\]\s*(.*)$", raw)
    if m:
        checked, text = m.group(1) == "X", m.group(2).strip()
        current = None
        if not text:
            continue
        head, related, examples = split_annotations(text)
        if not head:
            continue
        current = {
            "word": head,
            "checked": checked,
            "related": related,
            "examples": examples,
            "path": list(heading_stack),
        }
        cards.append(current)
        continue
    if current is not None and re.match(r"^\s+\S", raw):
        ex = raw.strip()
        if ex:
            current["examples"].append(ex)

print(f"parsed {len(cards)} cards")

# ---------------------------------------------------------------- lookup
def lookup(word):
    key = word.lower()
    if key in dictionary:
        return dictionary[key]
    # try first token for simple suffix variants, e.g. "stem (from)" -> "stem"
    first = re.split(r"[ (]", key)[0]
    if first and first != key and first in dictionary:
        return dictionary[first]
    # naive singular: "digits" -> "digit"
    if key.endswith("s"):
        for cand in (key[:-1], key[:-2] if key.endswith("es") else ""):
            if cand and cand in dictionary:
                return dictionary[cand]
    return None

hit = miss = 0
for c in cards:
    r = lookup(c["word"])
    if r:
        c["phonetic"], c["meaning"] = r
        hit += 1
    else:
        c["phonetic"], c["meaning"] = "", ""
        miss += 1
print(f"dictionary hit: {hit}, miss: {miss}")

# ---------------------------------------------------------------- emit
out = ["#+title: Words (org-drill)",
       "#+filetags: :drill:",
       "",
       "由 ~/org/words.org 自动转换生成；原文件保持不变。",
       "复习：打开本文件后 =C-c v d=。释义缺失的卡片标记了 =TODO=，可随手补全。",
       ""]

def esc(s):
    return s.replace("*", "")

last_path = []
n_card = 0
for c in cards:
    path = c["path"]
    # emit headings for path divergence
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
    n_card += 1
    tags = ":drill:" + ("known:" if c["checked"] else "") + ("" if c["meaning"] else "todo:")
    # normalize tag string
    taglist = ":drill:"
    if c["checked"]:
        taglist += "known:"
    if not c["meaning"]:
        taglist += "todo:"
    # 标题必须中性：org-drill 复习时标题始终可见，
    # 若标题含英文单词，中->英方向会被直接剧透
    out.append(f"{stars} Word {n_card:04d} {taglist}")
    out.append(":PROPERTIES:")
    out.append(":DRILL_CARD_TYPE: twosided")
    out.append(":END:")
    out.append("")
    out.append("*" * (level + 1) + " English")
    line = c["word"]
    if c["phonetic"]:
        line += f" /{c['phonetic']}/"
    out.append(line)
    if c["related"]:
        out.append("")
        out.append("相关: " + "; ".join(c["related"]))
    out.append("")
    out.append("*" * (level + 1) + " 中文释义")
    out.append(c["meaning"] if c["meaning"] else "TODO")
    if c["examples"]:
        out.append("")
        out.append("*" * (level + 1) + " 例句")
        for e in c["examples"]:
            out.append(f"- {e}")
    out.append("")

with open(DST, "w", encoding="utf-8") as f:
    f.write("\n".join(out))

missing = [c["word"] for c in cards if not c["meaning"]]
print(f"wrote {DST}: {len(cards)} cards, {len(missing)} without definition")
print("sample missing:", ", ".join(missing[:15]))
