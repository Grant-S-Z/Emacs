#!/usr/bin/env python3
"""Build a real Beamer PDF from an Org file and report frames whose body
OVERFLOWS the slide -- the classic silent Beamer failure: it compiles "fine"
but content runs off the bottom of the page.

Why a real build (not the linter's headless --export-check): overflow depends on
the actual fonts, theme, and CJK setup. The headless ``emacs --batch`` export uses
the default Org configuration rather than the project's classes and packages, so
its geometry is not representative. This script loads the configured Emacs setup
through an ISOLATED daemon, so it does not disturb an interactive Emacs session,
then parses the LaTeX ``.log`` for ``Overfull \\vbox`` (vertical overflow = body
pushed off the slide) and maps each one back to its frame title.

Usage:
    python build_pdf.py FILE.org [--server NAME] [--min-pt N]
                                 [--kill-daemon] [--keep-log]

  --server NAME   Emacs server/daemon socket to use (default: a private
                  "ob-build" daemon this script starts if absent; reused across
                  runs so iterating on layout is fast). Pass your interactive
                  server name to reuse it instead (note: export blocks that Emacs
                  for the ~20-60s build).
  --min-pt N      ignore overflows smaller than N pt (default 2.0; a few pt is
                  usually harmless slack, tens of pt means real spill-off).
  --kill-daemon   shut the private daemon down afterwards (default: leave it
                  running for the next build).
  --keep-log      keep the .log next to the PDF (default: removed on success).

Exit code 1 indicates content overflow. Exit code 2 indicates a build error or
a missing overflow log. Inspect rendered PDF pages separately.
"""
import argparse
import os
import re
import subprocess
import sys

DEFAULT_DAEMON = "ob-build"
OVERFULL_RE = re.compile(
    r"Overfull \\vbox \(([\d.]+)pt too high\)[^\n]*?detected at line (\d+)", re.S
)
FRAME_RE = re.compile(r"\\begin\{frame\}.*\{(.*)\}\s*$")


def _ec(server, *eval_args, timeout=300):
    """Run emacsclient against `server`; return CompletedProcess."""
    return subprocess.run(
        ["emacsclient", "-s", server, *eval_args],
        capture_output=True, text=True, timeout=timeout,
    )


def ensure_server(server: str, is_private: bool) -> str | None:
    """Make sure `server` is reachable. Start a private daemon if needed.
    Return None on success, else an error string."""
    if _ec(server, "--eval", "t", timeout=15).returncode == 0:
        return None
    if not is_private:
        return (f"Emacs server {server!r} not reachable. Start it (M-x server-start) "
                f"or omit --server to use the private '{DEFAULT_DAEMON}' daemon.")
    r = subprocess.run(["emacs", f"--daemon={server}"],
                       capture_output=True, text=True, timeout=120)
    if _ec(server, "--eval", "t", timeout=15).returncode != 0:
        return f"could not start Emacs daemon {server!r}:\n{r.stderr[-800:]}"
    return None


def export_pdf(server: str, org_abs: str) -> tuple[str | None, str]:
    """Export Org -> PDF through the configured Emacs environment."""
    # keep the .log so we can read Overfull warnings; revert in case the buffer
    # is stale from a previous build of the same file.
    el = (f'(let ((org-latex-remove-logfiles nil))'
          f'  (find-file "{org_abs}") (revert-buffer t t)'
          f'  (org-beamer-export-to-pdf))')
    r = _ec(server, "--eval", el)
    pdf = org_abs.rsplit(".", 1)[0] + ".pdf"
    out = (r.stdout or "") + (r.stderr or "")
    # org-beamer-export-to-pdf returns the PDF basename string on success and
    # signals an error (emacsclient exit != 0, "*ERROR*" on stderr) on failure.
    ok = (r.returncode == 0 and os.path.exists(pdf)
          and os.path.basename(pdf) in out and "*ERROR*" not in out)
    return (pdf, out) if ok else (None, out)


def parse_overflows(log_path: str, tex_path: str, min_pt: float):
    """Return list of (pt, tex_line, frame_title) for vbox overflows >= min_pt."""
    if not os.path.exists(log_path):
        return None
    with open(log_path, encoding="utf-8", errors="replace") as f:
        log = f.read()
    # frame starts: tex line number -> title
    frames = []
    if os.path.exists(tex_path):
        with open(tex_path, encoding="utf-8", errors="replace") as f:
            for i, line in enumerate(f, 1):
                if "\\begin{frame}" in line:
                    m = FRAME_RE.search(line.rstrip())
                    frames.append((i, m.group(1) if m else "(untitled frame)"))

    def title_for(line_no: int) -> str:
        best = None
        for fl, t in frames:
            if fl <= line_no:
                best = t
            else:
                break
        return best or "(outside any frame — title page / toc / structural)"

    out = []
    for m in OVERFULL_RE.finditer(log):
        pt = float(m.group(1))
        if pt < min_pt:
            continue
        ln = int(m.group(2))
        out.append((pt, ln, title_for(ln)))
    out.sort(key=lambda x: -x[0])
    return out


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("file")
    ap.add_argument("--server", default=DEFAULT_DAEMON)
    ap.add_argument("--min-pt", type=float, default=2.0)
    ap.add_argument("--kill-daemon", action="store_true")
    ap.add_argument("--keep-log", action="store_true")
    ap.add_argument("--show-all", action="store_true",
                    help="also list structural title/toc overflows (normally hidden)")
    args = ap.parse_args()

    org_abs = os.path.abspath(os.path.expanduser(args.file))
    if not os.path.exists(org_abs):
        print(f"no such file: {org_abs}")
        return 2
    is_private = args.server == DEFAULT_DAEMON

    err = ensure_server(args.server, is_private)
    if err:
        print(err)
        return 2

    pdf, raw = export_pdf(args.server, org_abs)
    base = org_abs.rsplit(".", 1)[0]
    log_path, tex_path = base + ".log", base + ".tex"

    if args.kill_daemon and is_private:
        _ec(args.server, "--eval", "(kill-emacs)", timeout=15)

    if pdf is None:
        print(f"BUILD FAILED for {args.file}")
        # surface the LaTeX error if the log captured one
        if os.path.exists(log_path):
            with open(log_path, encoding="utf-8", errors="replace") as f:
                errs = [l for l in f if l.startswith("!")]
            if errs:
                print("LaTeX errors:")
                for e in errs[:8]:
                    print("  " + e.rstrip())
        else:
            print(raw[-1000:])
        return 2

    overflows = parse_overflows(log_path, tex_path, args.min_pt)
    if not args.keep_log and os.path.exists(log_path):
        os.remove(log_path)

    print(f"Compiled PDF: {pdf}")
    print("Visual verification: pending rendered-page inspection.")
    if overflows is None:
        print("Overflow check unavailable: no LaTeX log found.")
        return 2
    # Separate real content frames from structural overflows (title page / toc /
    # section pages of themes like metropolis overflow by ~10-15pt on their own,
    # independent of your content — you can't fix those by editing slides).
    content = [o for o in overflows if not o[2].startswith("(outside")]
    structural = [o for o in overflows if o[2].startswith("(outside")]
    if structural:
        print(f"{len(structural)} overflow warning(s) outside content frames.")
        if args.show_all:
            for pt, ln, title in structural:
                print(f"  {pt:.0f}pt at tex line {ln}: {title}")
    if not content:
        print(f"No content-frame vertical overflow above {args.min_pt}pt in the LaTeX log.")
        return 0

    print(f"\n## {len(content)} content-frame vertical overflow warning(s)")
    for pt, ln, title in content:
        sev = "SEVERE" if pt >= 50 else ("moderate" if pt >= 10 else "slight")
        print(f"  [{sev}] {pt:.0f}pt too tall — frame “{title}” (tex line {ln})")
    print(
        "\nFix the overflowing frames by REDUCING content, not by shrinking text:\n"
        "  1. Split into two frames (best — one message per slide).\n"
        "  2. Cut bullets / shorten lines; move detail to the spoken track or a backup.\n"
        "  3. If a figure is the culprit, lower its :width (e.g. 0.75 -> 0.6).\n"
        "  4. Last resort only: :BEAMER_opt: shrink=NN (shrinks the font) or\n"
        "     allowframebreaks (auto-splits). Prefer real edits over these.\n"
        "Then rebuild and confirm the overflow is gone."
    )
    return 1


if __name__ == "__main__":
    sys.exit(main())
