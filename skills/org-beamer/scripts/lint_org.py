#!/usr/bin/env python3
"""Run shared Org lint and Beamer frame-density checks."""

from pathlib import Path
import re
import runpy
import sys


# --- Advisory frame-density thresholds (Beamer style, NOT correctness) --------
# These don't gate the build; they flag slides that are likely over-stuffed for a
# talk. A frame with :BEAMER_opt: allowframebreaks is exempt (it's deliberately
# long and will auto-split). Tune the numbers here.
MAX_BULLETS = 6        # list items in one frame before "too many bullets"
MAX_EQUATIONS = 2      # display equations in one frame before "too many equations"
MAX_RAWLATEX_LINES = 40  # content lines in one #+begin_export latex block

_LIST_ITEM = re.compile(r"^[ \t]*(?:[-+]|\d+[.)])[ \t]+\S")
_DISPLAY_MATH = re.compile(
    r"\\\[|\\begin\{(?:equation|align|gather|multline|eqnarray|alignat|flalign)\*?\}"
)


def check_structure(lines):
    """Advisory Beamer-density warnings, grouped per level-2 frame (H:2 house
    style: `** Title` is a frame). Returns (lineno, kind, msg) for:
      - too many bullets in one frame
      - too many display equations in one frame
      - an over-long single #+begin_export latex block (Org degenerating into a
        raw-LaTeX container)
    Frames carrying allowframebreaks are skipped for bullet/equation counts."""
    out = []

    # 1) per-frame bullet / equation density
    frames = []  # (start_lineno, [raw lines], allow_breaks)
    cur = None
    for i, raw in enumerate(lines, 1):
        if re.match(r"^\*\* ", raw):           # level-2 heading -> new frame
            cur = {"start": i, "body": [], "allow": False}
            frames.append(cur)
        elif re.match(r"^\* ", raw):            # level-1 section -> ends frame
            cur = None
        elif cur is not None:
            cur["body"].append(raw)
            if "allowframebreaks" in raw:
                cur["allow"] = True
    for fr in frames:
        if fr["allow"]:
            continue
        in_block = False
        bullets = equations = 0
        for raw in fr["body"]:
            low = raw.strip().lower()
            if low.startswith("#+begin_"):
                in_block = True
            elif low.startswith("#+end_"):
                in_block = False
            if _LIST_ITEM.match(raw) and not in_block:
                bullets += 1
            equations += len(_DISPLAY_MATH.findall(raw))
        if bullets > MAX_BULLETS:
            out.append((fr["start"], "density",
                        f"frame has {bullets} bullets (> {MAX_BULLETS}). One message "
                        f"per slide: split, cut, or fold detail into the spoken track. "
                        f"If it must stay long, add :BEAMER_opt: allowframebreaks."))
        if equations > MAX_EQUATIONS:
            out.append((fr["start"], "density",
                        f"frame has {equations} display equations (> {MAX_EQUATIONS}). "
                        f"A slide is not a derivation: keep the key line, move algebra "
                        f"to the spoken track or a backup slide."))

    # 2) over-long single raw-LaTeX export block (Org -> LaTeX container smell)
    start = None
    for i, raw in enumerate(lines, 1):
        low = raw.strip().lower()
        if start is None and re.match(r"^[ \t]*#\+begin_export\b.*\blatex\b", low):
            start = i
        elif start is not None and low.startswith("#+end_export"):
            n = i - start - 1
            if n > MAX_RAWLATEX_LINES:
                out.append((start, "rawlatex",
                            f"raw #+begin_export latex block is {n} lines (> "
                            f"{MAX_RAWLATEX_LINES}). Prefer Org-native constructs "
                            f"(#+ATTR_BEAMER, columns, tables) so the deck stays Org, "
                            f"not a LaTeX file in disguise."))
            start = None
    return out


def report_advisories(text):
    warnings = check_structure(text.splitlines())
    wlabels = {
        "density": "Frame too dense (advisory: one message per slide)",
        "rawlatex": "Over-long raw-LaTeX block (advisory: prefer Org-native)",
    }
    w_by_kind = {}
    for ln, kind, msg in warnings:
        w_by_kind.setdefault(kind, []).append((ln, msg))
    for kind in ("density", "rawlatex"):
        items = w_by_kind.get(kind, [])
        if not items:
            continue
        print(f"\n## [advisory] {wlabels[kind]}  [{len(items)}]")
        for ln, msg in items:
            print(f"  line {ln}: {msg}")


if __name__ == "__main__":
    shared = Path(__file__).resolve().parents[2] / "org" / "scripts" / "lint_org.py"
    if not shared.is_file():
        sys.exit(f"Install the org skill next to org-beamer: missing {shared}")
    main = runpy.run_path(str(shared))["main"]
    sys.exit(main(report_advisories, description=__doc__))
