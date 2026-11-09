#!/usr/bin/env python3
r"""Lint an Org-mode file for the silent-failure traps that bite when exporting to
LaTeX/PDF -- especially CJK (Chinese/Japanese/Korean) documents.

Checks include problems that compile "successfully" (or with a warning buried in
thousands of lines) but produce wrong output, so the eye misses them:

  1. FULL-WIDTH PUNCTUATION in Chinese Org prose.  Use ASCII punctuation so
     emphasis boundaries remain predictable: ``, . ; : ! ? () []`` rather than
     ``，。；：！？（）【】``; use ``...`` / ``---`` for ``…`` / ``—``.

  2. EMPHASIS next to CJK.  Org only recognises ``*bold*`` / ``/italic/`` /
     ``=code=`` / ``~verb~`` when the marker is flanked by whitespace or a small
     set of ASCII border chars.  A CJK character or full-width punctuation is
     NOT a valid border, so ``某*词*某`` exports as a LITERAL asterisk instead of
     \\textbf{}.  Fix: put an ASCII space on each side -> ``某 *词* 某``.

  3. UNICODE SYMBOLS missing from the Latin body font.  Dingbats (✓ ✗), arrows
     (→ ⇒), and math operators (≈ ≤ ∑) typed as literal text have no glyph in
     fonts like Times New Roman -> "Missing character" warning + blank output.
     Fix: use a LaTeX command in math (\\checkmark, \\times, \\Rightarrow,
     \\approx, \\le, \\sum) or a plain word (有/无).

  4. EN-DASH used for ranges/connections.  A raw "–" (U+2013) should be written
     as ``--`` in the source (``\\text{--}`` inside math) so LaTeX renders the
     proper dash.  Use ASCII source for other punctuation too.

  5. DOLLAR-STYLE MATH DELIMITERS.  Prefer ``\(...\)`` for inline math and
     ``\[...\]`` for unnumbered display math.  Do not generate ``$...$`` or
     ``$$...$$`` in Org prose.

  6. MATH-ONLY COMMAND in running text.  Having correctly chosen a LaTeX command
     over a unicode glyph (``\\to`` instead of →), it must still be in math mode.
     ``\\to``, ``\\Rightarrow``, ``\\alpha``, ``\\frac`` ... in plain text raise
     "Missing $ inserted".  Fix: wrap it -> ``\\(\\to\\)``.

  7. HAND-ESCAPED \\% in plain text.  Org escapes % itself (``50%`` exports as
     ``50\\%``), so a hand-written ``\\%`` exports as ``$\\backslash$%`` -- a
     literal backslash, and the now-unescaped % starts a TeX COMMENT that eats
     the rest of the source line.  Fix: write plain ``%`` in Org text.  Inside
     math fragments and raw export/src blocks content passes through verbatim,
     so THERE ``\\%`` is required (``\\(\\approx 1\\%\\)`` is correct).

  8. BACKSLASH-UNDERSCORE in plain text.  ``\\_`` in Org prose (not inside
     ``~verbatim~``, not in math) is exported by org-latex as
     ``$\\backslash$\\textsubscript{...}`` -- NOT a literal underscore.  This
     silently garbles identifiers like ``pywa\\_iminuit`` in the PDF.
     Fix: wrap the whole identifier in verbatim: ``~pywa_iminuit~``.

Usage:
    python lint_org.py FILE.org [--export-check]

Exit code is non-zero if any problem is found, so it can gate a build.
``--export-check`` additionally runs a headless ``emacs --batch`` LaTeX export to
confirm the file exports without error -- note this uses the default Org config,
so it validates STRUCTURE only, not the final fonts/CJK/theme rendering.
"""
import argparse
import re
import subprocess
import sys
import unicodedata

# Check bold, italic, verbatim, and code. Keep skipping _ (underline) and +
# (strike) because they collide heavily with identifiers, subscripts, and
# ordinary notation. Slash-separated CJK prose should use spaces
# ("反射 / 扩张 / 收缩"), which also prevents accidental /italic/ parsing.
MARKERS = "*/=~"

# Chinese and typographic punctuation that should be written with ASCII source
# in Org prose. Raw export/source/example blocks are exempt because Org does not
# parse their contents as prose or emphasis.
ASCII_PUNCTUATION = {
    "，": ",", "。": ".", "；": ";", "：": ":", "！": "!", "？": "?",
    "（": "(", "）": ")", "【": "[", "】": "]", "〔": "[", "〕": "]",
    "“": '"', "”": '"', "‘": "'", "’": "'", "、": ", or / with spaces",
    "…": "...", "—": "---", "《": '"', "》": '"', "〈": '"', "〉": '"',
    "「": '"', "」": '"', "『": '"', "』": '"', "　": "an ASCII space",
}

# Valid characters immediately BEFORE an opening marker / AFTER a closing marker,
# per Org's default org-emphasis-regexp-components. CJK chars are absent on
# purpose -- that's the whole trap.
PRE_OK = set(" \t-({'\"")          # plus beginning-of-line
POST_OK = set(" \t-.,;:!?')}[\"\\")  # plus end-of-line


def is_cjk(ch: str) -> bool:
    o = ord(ch)
    return (
        0x4E00 <= o <= 0x9FFF      # CJK Unified Ideographs
        or 0x3400 <= o <= 0x4DBF   # Ext A
        or 0x3000 <= o <= 0x303F   # CJK symbols & punctuation （。、「」etc.)
        or 0xFF00 <= o <= 0xFFEF   # full-width forms （，：（）！？etc.)
    )


def is_safe_char(ch: str) -> bool:
    """Chars that render fine in a normal CJK LaTeX setup."""
    o = ord(ch)
    if o < 0x80:
        return True                      # ASCII
    if is_cjk(ch):
        return True                      # punctuation policy is checked separately
    # These may render, but the punctuation policy is checked separately:
    return ch in "—…“”‘’、　"            # em-dash, ellipsis, curly quotes, ideographic space


def suggest_for_symbol(ch: str) -> str:
    o = ord(ch)
    # --- Mathematical Alphanumeric Symbols (U+1D400–U+1D7FF) ---
    # These look like italic/bold/sans-serif letters but are separate Unicode
    # code points with NO glyph in standard LaTeX fonts. They sneak in when
    # text is copy-pasted from PDFs, arXiv abstracts (which use MathJax),
    # Word documents, or AI-generated content. Parse the Unicode name to
    # give a precise replacement.
    if 0x1D400 <= o <= 0x1D7FF:
        name = unicodedata.name(ch, "")
        m = re.match(
            r"MATHEMATICAL (.+?) (CAPITAL|SMALL|DIGIT) (.+)", name
        )
        if m:
            style_str, _case, letter = m.groups()
            style_str = style_str.strip()
        else:
            parts = name.split()
            style_str = " ".join(parts[1:-1]) if len(parts) > 2 else parts[-1]
            letter = parts[-1]
        # Greek letter names -> LaTeX commands
        _GREEK = {
            "ALPHA": r"A", "BETA": r"B", "GAMMA": r"\Gamma",
            "DELTA": r"\Delta", "EPSILON": r"E", "ZETA": r"Z",
            "ETA": r"H", "THETA": r"\Theta", "IOTA": r"I",
            "KAPPA": r"K", "LAMBDA": r"\Lambda", "MU": r"M",
            "NU": r"N", "XI": r"\Xi", "OMICRON": r"O", "PI": r"\Pi",
            "RHO": r"R", "SIGMA": r"\Sigma", "TAU": r"T",
            "UPSILON": r"\Upsilon", "PHI": r"\Phi", "CHI": r"X",
            "PSI": r"\Psi", "OMEGA": r"\Omega",
            "alpha": r"\alpha", "beta": r"\beta", "gamma": r"\gamma",
            "delta": r"\delta", "epsilon": r"\epsilon", "zeta": r"\zeta",
            "eta": r"\eta", "theta": r"\theta", "iota": r"\iota",
            "kappa": r"\kappa", "lambda": r"\lambda", "mu": r"\mu",
            "nu": r"\nu", "xi": r"\xi", "omicron": r"o", "pi": r"\pi",
            "rho": r"\rho", "sigma": r"\sigma", "tau": r"\tau",
            "upsilon": r"\upsilon", "phi": r"\phi", "chi": r"\chi",
            "psi": r"\psi", "omega": r"\omega",
        }
        if letter in _GREEK:
            base = _GREEK[letter]
            wrap = f"\\({base}\\)"
        elif len(letter) == 1 and letter.isascii():
            base = letter if _case == "CAPITAL" else letter.lower()
            wrap = f"\\({base}\\)"
        else:
            wrap = f"\\(\\mathrm{{{letter}}}\\)"
        _STYLE_HINT = {
            "ITALIC": "math italic",
            "BOLD": "bold",
            "BOLD ITALIC": "bold italic",
            "SCRIPT": "script",
            "BOLD SCRIPT": "bold script",
            "FRAKTUR": "fraktur",
            "BOLD FRAKTUR": "bold fraktur",
            "DOUBLE-STRUCK": "double-struck",
            "SANS-SERIF": "sans-serif",
            "SANS-SERIF BOLD": "sans-serif bold",
            "SANS-SERIF ITALIC": "sans-serif italic",
            "SANS-SERIF BOLD ITALIC": "sans-serif bold italic",
            "MONOSPACE": "monospace",
        }
        hint = _STYLE_HINT.get(style_str, "mathematical")
        return (
            f"{wrap} (math mode) — this is a Unicode {hint} letter, "
            f"NOT the same as a regular ASCII letter; standard LaTeX fonts "
            f"have NO glyph for it"
        )
    # --- known symbols ---
    table = {
        "✓": r"有 / \(\checkmark\) (amssymb)", "✗": r"无 / \(\times\)",
        "→": r"\(\to\)", "←": r"\(\gets\)", "⇒": r"\(\Rightarrow\)", "⇐": r"\(\Leftarrow\)",
        "≈": r"\(\approx\)", "≤": r"\(\le\)", "≥": r"\(\ge\)", "≠": r"\(\ne\)",
        "×": r"\(\times\)", "·": r"\(\cdot\)", "∑": r"\(\sum\)", "∏": r"\(\prod\)",
        "∈": r"\(\in\)", "√": r"\(\sqrt{}\)", "∞": r"\(\infty\)", "±": r"\(\pm\)",
        "–": r"-- (en-dash; \text{--} inside math)",
    }
    if ch in table:
        return table[ch]
    if 0x2190 <= o <= 0x21FF:
        return r"a LaTeX arrow command, e.g. \(\to\) / \(\Rightarrow\)"
    if 0x2200 <= o <= 0x22FF:
        return "a LaTeX math command (it's a math operator)"
    if 0x2700 <= o <= 0x27BF or 0x2600 <= o <= 0x26FF:
        return "a plain word or \\ding{} (pifont); this dingbat has no Latin glyph"
    return "a LaTeX command or plain word (no Latin-font glyph)"


# Common math-ONLY commands that people paste into running text after correctly
# avoiding a unicode glyph (e.g. they write \to instead of →) but forget to wrap
# it in \(...\). Outside math mode these raise "Missing $ inserted". This list is
# the high-frequency offenders, not exhaustive.
MATH_CMDS = {
    # arrows / relations
    "to", "gets", "Rightarrow", "Leftarrow", "rightarrow", "leftarrow",
    "leftrightarrow", "mapsto", "approx", "le", "ge", "leq", "geq", "ne",
    "neq", "equiv", "sim", "propto", "in", "notin", "subset", "supset",
    "ll", "gg", "ldots", "cdots",
    # operators / symbols
    "times", "cdot", "pm", "mp", "div", "otimes", "oplus", "sum", "prod",
    "int", "partial", "nabla", "sqrt", "infty", "forall", "exists",
    # greek (lowercase + common capitals)
    "alpha", "beta", "gamma", "delta", "epsilon", "varepsilon", "zeta",
    "eta", "theta", "vartheta", "iota", "kappa", "lambda", "mu", "nu", "xi",
    "pi", "rho", "sigma", "tau", "upsilon", "phi", "varphi", "chi", "psi",
    "omega", "Gamma", "Delta", "Theta", "Lambda", "Xi", "Pi", "Sigma",
    "Phi", "Psi", "Omega",
    # structures (math-mode constructs)
    "frac", "mathbb", "mathcal", "mathrm", "mathbf", "hat", "tilde", "bar",
    "vec", "overline", "underbrace", "overbrace",
}
_MATHENVS = {
    "equation", "equation*", "align", "align*", "gather", "gather*",
    "multline", "multline*", "eqnarray", "eqnarray*", "math", "displaymath",
    "alignat", "alignat*", "flalign", "flalign*",
}


def _math_spans(text: str):
    """Char-index spans that are inside math mode: \\(..\\), \\[..\\], $..$, and
    math environments. Best-effort (assumes well-formed delimiters)."""
    spans = []
    for op, cl in (("\\(", "\\)"), ("\\[", "\\]")):
        idx = 0
        while (s := text.find(op, idx)) >= 0:
            e = text.find(cl, s + 2)
            e = len(text) if e < 0 else e + 2
            spans.append((s, e))
            idx = e
    dollars = [m.start() for m in re.finditer(r"(?<!\\)\$", text)]
    for k in range(0, len(dollars) - 1, 2):
        spans.append((dollars[k], dollars[k + 1] + 1))
    for m in re.finditer(r"\\begin\{([a-zA-Z*]+)\}", text):
        if m.group(1) in _MATHENVS:
            e = text.find("\\end{" + m.group(1) + "}", m.end())
            spans.append((m.start(), len(text) if e < 0 else e))
    return spans


def check_math_mode(text: str):
    """Flag math-only commands used outside any math span."""
    spans = _math_spans(text)
    line_off = [0]
    for i, ch in enumerate(text):
        if ch == "\n":
            line_off.append(i + 1)

    def lineno(pos):
        import bisect
        return bisect.bisect_right(line_off, pos)

    out = []
    cmd_re = re.compile(r"\\([A-Za-z]+)")
    for m in cmd_re.finditer(text):
        if m.group(1) in MATH_CMDS and not any(s <= m.start() < e for s, e in spans):
            out.append((lineno(m.start()), "mathmode",
                        f"\\{m.group(1)} is math-only but used in text -> wrap it: "
                        f"\\(\\{m.group(1)}\\) (else 'Missing $ inserted')"))
    return out


def check_math_delimiters(text: str):
    """Reject paired dollar math delimiters in exported Org prose."""
    ignored_spans = []
    for m in re.finditer(r"(?im)^[ \t]*#\+begin_(export|src|example)\b", text):
        end = re.search(
            r"(?im)^[ \t]*#\+end_" + m.group(1) + r"\b", text[m.end():]
        )
        ignored_spans.append((
            m.start(), len(text) if end is None else m.end() + end.end()
        ))

    line_starts = [0]
    for i, ch in enumerate(text):
        if ch == "\n":
            line_starts.append(i + 1)

    def lineno(pos):
        import bisect
        return bisect.bisect_right(line_starts, pos)

    def ignored(pos):
        if any(start <= pos < end for start, end in ignored_spans):
            return True
        ln = lineno(pos)
        start = line_starts[ln - 1]
        end = text.find("\n", start)
        line = text[start:] if end < 0 else text[start:end]
        if re.match(r"^[ \t]*#(?:[ \t]|$)", line):
            return True
        keyword = re.match(r"^[ \t]*#\+([A-Za-z_]+):", line)
        return bool(keyword and keyword.group(1).upper() in {
            "LATEX_HEADER", "BEAMER_HEADER"
        })

    out = []
    display_spans = []
    for m in re.finditer(r"(?<!\\)\$\$[\s\S]*?(?<!\\)\$\$", text):
        display_spans.append(m.span())
        if not ignored(m.start()):
            out.append((
                lineno(m.start()), "dollar_math",
                r"$$...$$ display math -> use \[...\] in Org prose"
            ))

    inline_re = re.compile(
        r"(?<![\\$])\$(?!\$)[^\n$]+?(?<![\\$])\$(?!\$)"
    )
    for m in inline_re.finditer(text):
        if any(start <= m.start() < end for start, end in display_spans):
            continue
        if not ignored(m.start()):
            out.append((
                lineno(m.start()), "dollar_math",
                r"$...$ inline math -> use \(...\) in Org prose"
            ))
    return out


_BACKSLASH_UNDER = re.compile(r"\\_")


def _verbatim_spans(line: str):
    """Char-index spans inside inline verbatim (~...~ and =...=) on one line.

    These are safe from the backslash-underscore trap: Org exports them as
    \\texttt{} or \\verb -- the content is not re-interpreted as LaTeX commands.
    """
    spans = []
    for marker in ("~", "="):
        pat = re.compile(
            re.escape(marker) + r"[^" + re.escape(marker) + r"\n]+" + re.escape(marker)
        )
        for m in pat.finditer(line):
            spans.append((m.start(), m.end()))
    return spans


def check_latex_specials(text: str):
    r"""Flag hand-escaped LaTeX specials (\%, \&, \$, \#, \{, \}) in Org
    plain text. Org escapes these itself, so a hand-written backslash-escape
    exports as $\backslash$ followed by the raw special — e.g. \& becomes
    $\backslash$&, and the raw & then acts as a tabular alignment tab (breaking
    \maketitle if it's in the title/subtitle).

    Inside math fragments and raw blocks (export/src/example) content passes
    through verbatim, so backslash-escapes ARE correct — and required — THERE."""
    spans = _math_spans(text)
    for m in re.finditer(r"(?im)^[ \t]*#\+begin_(export|src|example)\b", text):
        e = re.search(r"(?im)^[ \t]*#\+end_" + m.group(1) + r"\b", text[m.end():])
        spans.append((m.start(), len(text) if e is None else m.end() + e.end()))
    line_off = [0]
    for i, ch in enumerate(text):
        if ch == "\n":
            line_off.append(i + 1)

    def lineno(pos):
        import bisect
        return bisect.bisect_right(line_off, pos)

    _MSG = {
        "%": r"\% in plain text exports as a literal backslash + a TeX "
             r"comment that eats the rest of the line -> write plain % "
             r"(Org escapes it itself)",
        "&": r"\& in plain text exports as $\backslash$& -> the raw & "
             r"becomes a tabular alignment tab (breaks \maketitle if in "
             r"the title/subtitle). Write plain & (Org escapes it itself).",
        "$": r"\$ in plain text exports as $\backslash$$ -> the raw $ "
             r"starts math mode. Write plain $ (Org escapes it itself).",
        "#": r"\# in plain text exports as $\backslash$# -> the raw # "
             r"is a macro parameter char. Write plain # (Org escapes it itself).",
        "{": r"\{ in plain text exports as $\backslash${ -> unbalanced "
             r"braces may break the build. Write plain { (Org escapes it itself).",
        "}": r"\} in plain text exports as $\backslash$} -> unbalanced "
             r"braces may break the build. Write plain } (Org escapes it itself).",
    }
    out = []
    for m in re.finditer(r"\\([%&#{}$])", text):
        ch = m.group(1)
        if any(s <= m.start() < e for s, e in spans):
            continue
        ln = lineno(m.start())
        if text[line_off[ln - 1]:m.start()].lstrip().startswith("#"):
            continue  # Org comment / keyword line: not exported
        out.append((ln, "latex_special", _MSG.get(ch, _MSG["%"])))
    return out


def iter_emphasis_spans(line: str):
    """Yield (marker, start, end) for each balanced single-line emphasis span."""
    for m in MARKERS:
        # content can't start/end with whitespace and can't contain the marker
        pat = re.compile(re.escape(m) + r"([^\s" + re.escape(m) + r"][^" +
                         re.escape(m) + r"]*?)" + re.escape(m))
        for mo in pat.finditer(line):
            yield m, mo.start(), mo.end()


def lint(path: str):
    problems = []  # (lineno, kind, msg)
    in_block = False  # inside #+begin_..#+end_ (skip emphasis scan; raw content)
    with open(path, encoding="utf-8") as f:
        lines = f.readlines()

    for i, raw in enumerate(lines, 1):
        line = raw.rstrip("\n")
        low = line.strip().lower()
        if low.startswith("#+begin_"):
            in_block = True
        elif low.startswith("#+end_"):
            in_block = False
            continue

        is_comment = bool(re.match(r"^[ \t]*#(?:[ \t]|$)", line))
        keyword = re.match(r"^[ \t]*#\+([A-Za-z_]+):[ \t]*(.*)$", line)
        raw_keyword = bool(keyword and keyword.group(1).upper() in {
            "LATEX_HEADER", "BEAMER_HEADER"
        })
        scan_line = keyword.group(2) if keyword and not raw_keyword else line

        # --- (1) ASCII punctuation in exported Org prose ---
        if not in_block and not is_comment and not raw_keyword:
            for ch in scan_line:
                if ch in ASCII_PUNCTUATION:
                    problems.append((
                        i, "punctuation",
                        f"use ASCII punctuation: {ch!r} -> {ASCII_PUNCTUATION[ch]!r}"
                    ))

        # --- (3)+(4) font-unsafe unicode: scan every line, even in blocks/math,
        # because a missing glyph fails everywhere ---
        for ch in line:
            if ch in ASCII_PUNCTUATION:
                continue  # already reported with a precise ASCII replacement
            if not is_safe_char(ch):
                name = unicodedata.name(ch, "?")
                problems.append((
                    i, "unicode",
                    f"U+{ord(ch):04X} {name} ({ch!r}) -> use {suggest_for_symbol(ch)}"
                ))

        if in_block:
            continue
        if is_comment or raw_keyword:
            continue

        # --- (2) invalid emphasis boundaries ---
        link_spans = [m.span() for m in re.finditer(
            r"\[\[[^\]\n]+(?:\]\[[^\]\n]*)?\]\]", scan_line
        )]
        for marker, s, e in iter_emphasis_spans(scan_line):
            if any(start <= s < end for start, end in link_spans):
                continue  # path/URL syntax inside an Org link target
            if marker == "/" and re.search(r"https?:/$", scan_line[:s]):
                continue  # the second slash in https://..., not italic markup
            before = scan_line[s - 1] if s > 0 else None
            after = scan_line[e] if e < len(scan_line) else None
            bad_before = before is not None and before not in PRE_OK
            bad_after = after is not None and after not in POST_OK
            if bad_before or bad_after:
                span = scan_line[s:e]
                sides = []
                if bad_before:
                    sides.append(f"before ({before!r})")
                if bad_after:
                    sides.append(f"after ({after!r})")
                problems.append((
                    i, "emphasis",
                    f"{span!r} has an invalid Org boundary {' & '.join(sides)} "
                    f"-> add an ASCII space, e.g. ` {span} `"
                ))

        # --- (6) backslash-underscore in plain text ---
        # In Org, \_ outside verbatim/math is exported by org-latex as
        # \backslash + \textsubscript{...}, NOT a literal underscore.
        # This silently garbles identifiers (e.g. pywa\_iminuit ->
        # "pywa$\backslash$\textsubscript{iminuit}" in the PDF).
        # Fix: wrap the identifier in ~verbatim~, e.g. ~pywa_iminuit~,
        # so Org exports it as \texttt{pywa_iminuit} without mangling.
        if _BACKSLASH_UNDER.search(line):
            safe_spans = _verbatim_spans(line) + _math_spans(line)
            for m in _BACKSLASH_UNDER.finditer(line):
                pos = m.start()
                if not any(s <= pos < e for s, e in safe_spans):
                    ctx_start = max(0, pos - 15)
                    ctx_end = min(len(line), pos + 25)
                    ctx = line[ctx_start:ctx_end]
                    problems.append((
                        i, "backslash_under",
                        r"'\\_' in plain text exports as \backslash\textsubscript{...} "
                        r"-> wrap identifier in ~verbatim~ (e.g. ~name_with_under~) "
                        f"[context: '{ctx}']"
                    ))
    return problems


def export_check(path: str) -> str | None:
    """Return None on success, else an error string."""
    try:
        r = subprocess.run(
            ["emacs", "--batch", "-l", "org", path,
             "-f", "org-latex-export-to-latex"],
            capture_output=True, text=True, timeout=180,
        )
    except FileNotFoundError:
        return "emacs not found; skip --export-check or install emacs"
    except subprocess.TimeoutExpired:
        return "emacs export timed out"
    tex = path.rsplit(".", 1)[0] + ".tex"
    import os
    if not os.path.exists(tex):
        return f"export produced no .tex\n{r.stderr[-800:]}"
    os.remove(tex)  # headless export lacks project fonts/CJK config; not the real PDF
    return None  # exported to LaTeX without error


def main(advisory=None, *, description=__doc__):
    ap = argparse.ArgumentParser(description=description,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("file")
    ap.add_argument("--export-check", action="store_true",
                    help="also run a headless emacs LaTeX export (structure only)")
    args = ap.parse_args()

    problems = lint(args.file)
    text = open(args.file, encoding="utf-8").read()
    problems += check_math_delimiters(text)
    problems += check_math_mode(text)
    problems += check_latex_specials(text)
    by_kind = {}
    for ln, kind, msg in problems:
        by_kind.setdefault(kind, []).append((ln, msg))

    labels = {
        "punctuation": "Full-width punctuation in Org prose (use ASCII punctuation)",
        "emphasis": "Invalid Org emphasis boundary (would export as a literal marker)",
        "unicode": "Font-unsafe unicode (missing-glyph / wrong dash)",
        "dollar_math": "Dollar-style math delimiter (use \\(...\\) or \\[...\\])",
        "mathmode": "Math-only command used outside math mode (Missing $ inserted)",
        "latex_special": "Hand-escaped LaTeX special (\\%, \\&, \\$, \\#, \\{, \\}) in plain text — backslash + raw special breaks the build",
        "backslash_under": r"Backslash-underscore in plain text (exports as \backslash\textsubscript, not _)",
    }
    for kind in ("punctuation", "emphasis", "unicode", "dollar_math", "mathmode", "latex_special", "backslash_under"):
        items = by_kind.get(kind, [])
        if not items:
            continue
        print(f"\n## {labels[kind]}  [{len(items)}]")
        for ln, msg in items:
            print(f"  line {ln}: {msg}")

    if advisory is not None:
        advisory(text)

    if args.export_check:
        err = export_check(args.file)
        print("\n## Export check")
        print("  OK -- file exports to LaTeX without error" if err is None else f"  {err}")
        if err:
            problems.append((0, "export", err))

    if not problems:
        print(f"OK: {args.file} — no punctuation, emphasis-boundary, math-delimiter, unicode, or export problems found.")
        return 0
    print(f"\n{len(problems)} problem(s) found in {args.file}.")
    return 1


if __name__ == "__main__":
    sys.exit(main())
