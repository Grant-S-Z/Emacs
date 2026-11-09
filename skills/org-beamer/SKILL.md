---
name: org-beamer
description: >-
  Write, convert, or edit Org-mode Beamer slide decks, including Chinese and
  bilingual research talks. Use when the requested slide source is Org-mode.
  Uses the shared org lint rules and adds Beamer frame-density checks.
---

# Org Beamer authoring (CJK-aware)

Org → Beamer is a minefield of **silent** failures: the file compiles "fine" but
the output is wrong, so the eye misses it. This skill encodes portable authoring
conventions and ships a linter that catches the traps mechanically. Apply it for
any task whose deliverable is an `.org` Beamer deck.

## Workflow

1. **Match the current project before applying defaults.** Read
   `references/conventions.md` for the portable preamble, frame structure
   (`H:2`: level-1 = section, level-2 = frame), columns, tables, and math. When
   editing an existing project, inspect a nearby deck or project documentation
   and preserve its class, compiler, theme, fonts, author metadata, and export
   settings. For a new standalone deck, use the bundled generic template and
   replace its placeholders.

2. **Write or convert the content into frames — using Org-native constructs.**
   Org Beamer has native markup for nearly every Beamer element. **Always prefer
   the Org form:** level-3 headings are blocks (never `\begin{block}{}`), Org
   lists are itemize/enumerate (never `\begin{itemize}`), `:BMCOL:` tags are
   columns (never `\begin{columns}\column{..}`), `| a | b |` pipe tables are
   tables (never `\begin{tabular}` — add `#+attr_latex: :booktabs t` for
   booktabs styling), `*bold*` is bold (never `\textbf{}` in prose). Only reach
   for raw LaTeX `#+begin_export latex` when Org genuinely cannot express the
   thing — TikZ diagrams, multirow/multicolumn tables, the thank-you page
   overlay. See the mapping table in
   `references/conventions.md` § "Prefer Org-native constructs".

   One idea per level-2 frame, ~one screenful each. Prose from a report becomes
   bullets + equations, not walls of text. For blocks with columns, use the
   combined `:BMCOL:` + block pattern (the heading IS the block title — see
   `references/conventions.md` § "Blocks inside columns").

   For a **conference talk / 会议汇报**, start from the work-focused template:
   `references/talk-template.org` (Chinese) or
   `references/talk-template-en.org` (English) — pick by the talk's language.
   Both use the standard `beamer` class and the same skeleton — a thin
   Motivation / Background runway, then a **Body** section you organize freely,
   then Summary — with a tick-list frame menu (keep the frames this talk needs,
   delete the rest). Crucially, the Body is **not** a fixed "Method → Results"
   split: let its structure follow the source — a paper's section order when
   converting a paper, the work items / timeline for a routine progress report.
   The templates bake in the three rules the linter can't enforce: **conclusion-
   style titles** (the title states the takeaway, not the topic), **one message
   per slide**, and **figure-first**. Don't compress a report section by section
   into slides — transform it into claims + figures.

   The templates now include **Speaker Notes** (`:B_note:`) on every frame for
   talking points, a **Thank you** slide (using the `:B_ignoreheading:` pattern),
   citation support (`[cite:@key]` + `#+print_bibliography:`), and an
   in-slide TikZ diagram shape. See `references/conventions.md` for the full
   pattern catalog — optional `siunitx`/`mhchem` support, short-title and
   `\institute{}`, title-page logo overlays, `appendixnumberbeamer`, and `COMMENT`
   headlines for hiding work-in-progress frames.

3. **Lint the deck once.** Run
   `python3 "<this-skill-dir>/scripts/lint_org.py" FILE.org`. This uses the
   sibling `org` skill's common checks and adds non-blocking Beamer advisories
   for dense frames and long raw LaTeX blocks. Review diagnostics against the
   diff and fix issues introduced by the edit. Consult `org` for CJK and LaTeX
   source conventions.

4. **Compile and inspect the PDF as separate checks.** When delivering a PDF,
   use the project's configured Emacs and LaTeX toolchain. If appropriate,
   `scripts/build_pdf.py --show-all FILE.org` builds via Emacs and reports
   vertical overflow from the LaTeX log. Render and inspect the title page,
   table of contents if present, and changed frames (all frames for a new
   deck). Check clipping, glyphs, equations, figures, and tables. Report
   compilation, log diagnostics, and visual inspection separately. If the
   export environment is unavailable, deliver the Org source and say which
   PDF checks remain unverified.
