# Portable Org Beamer conventions

Use these defaults when writing or converting a standalone deck. When editing an
existing project, treat nearby decks and project documentation as the source of
truth: preserve their class, compiler, theme, fonts, metadata, and preamble unless
the user asks to change them.

## Class and language

- Use the standard `#+LATEX_CLASS: beamer` for portable new decks.
- Preserve a project's custom Beamer class when editing an existing deck.
- For CJK content, use the project's configured CJK package and fonts. If the
  project has no convention, add a suitable `ctex` or `xeCJK` setup only after
  confirming it is installed; never hard-code a machine-specific font name.

## CJK punctuation and emphasis

- Write Chinese Org prose with ASCII punctuation: `, . ; : ! ? () [] "" ''`.
  Use `...` for an ellipsis and `---` for an em-dash.
- Put an ASCII space between CJK text and `*bold*`, `/italic/`, `=verbatim=`, or
  `~code~`. For example, write `这是 *重点*.` and `这是 /斜体/.`.
- Write slash-separated text as `反射 / 扩张 / 收缩`, not
  `反射/扩张/收缩`, so Org does not interpret the middle segment as malformed
  italic markup.
- These rules apply to headlines, lists, tables, captions, and metadata as well
  as ordinary paragraphs. Raw LaTeX and source blocks follow their own syntax.

## Portable preamble

```org
#+OPTIONS: H:2 num:t toc:t \n:nil @:t ::t |:t ^:t -:t f:t *:t <:t
#+OPTIONS: TeX:t LaTeX:t skip:nil d:nil todo:t pri:nil tags:not-in-toc
#+LATEX_COMPILER: lualatex
#+LATEX_CLASS: beamer
#+LATEX_HEADER: \usepackage{booktabs}
#+LATEX_HEADER: \usepackage{appendixnumberbeamer}
#+LATEX_HEADER: \useoutertheme{infolines}
#+LATEX_HEADER: \useinnertheme{default}
#+LATEX_HEADER: \setbeamertemplate{blocks}[rounded][shadow=false]
#+LATEX_HEADER: \setbeamertemplate{items}[circle]
#+LATEX_HEADER: \setbeamertemplate{sections/subsections in toc}[sections numbered]
#+LATEX_HEADER: \setbeamertemplate{headline}[default]
#+LATEX_HEADER: %\setbeamertemplate{frame numbering}[none]
#+LATEX_HEADER: \usetikzlibrary{arrows.meta, positioning, angles, quotes}
#+TITLE: Talk title
#+BEAMER_HEADER: \AtBeginDocument{\title[Short title]{Talk title}}
#+SUBTITLE: Conference / occasion
#+AUTHOR: Presenter Name
#+DATE: \today
#+BEAMER_HEADER: \institute[Short affiliation]{Full affiliation \and \href{mailto:name@example.org}{\texttt{name@example.org}}}
```

- `#+LATEX_COMPILER: lualatex` is a portable Unicode-capable default. Preserve a
  project's existing compiler when editing a deck, and make sure it matches the
  selected CJK and font packages.
- `appendixnumberbeamer` — stops appendix slides from inflating the slide count
  / progress bar.
- `\AtBeginDocument{\title[Short]{Long}}` — short title for the infolines header;
  long title for the title page. The short form should fit in ~50 chars.
- `\institute[short]{long}` — same pattern for the affiliation footer.
- For single-author talks, drop `\and Coauthor Name` and simplify `\institute`.

### Title page logo overlay (conference / experiment talks)

For talks with organization or project logos, add a TikZ overlay on the title
page. Use project-relative assets and adjust paths and coordinates as needed:

```org
#+BEAMER_HEADER: \addtobeamertemplate{title page}{}{%
#+BEAMER_HEADER: \begin{tikzpicture}[remember picture,overlay]
#+BEAMER_HEADER: \node[anchor=north east, xshift=-0.3cm, yshift=-0.3cm] at (current page.north east)
#+BEAMER_HEADER: {\includegraphics[height=1.2cm]{./assets/institution-logo.pdf}\hspace{0.3cm}\includegraphics[height=1.2cm]{./assets/project-logo.png}};
#+BEAMER_HEADER: \node[anchor=south west, xshift=0.3cm, yshift=0.3cm, font=\footnotesize, text=black!50] at (current page.south west)
#+BEAMER_HEADER: {\href{https://arxiv.org/abs/XXXX.XXXXX}{\textcolor{black!50}{\texttt{arXiv:XXXX.XXXXX}}}};
#+BEAMER_HEADER: \end{tikzpicture}}
```

This is optional. Use it only when the talk benefits from visible organization,
project, or publication branding.

### Citation support (biblatex)

If the deck cites papers, add these lines after the theme setup:

```org
#+cite_export: biblatex
#+BEAMER_HEADER: \setbeamertemplate{bibliography item}[text]
#+LATEX_HEADER: \AtBeginDocument{\AtEveryBibitem{\clearfield{url}\clearfield{urldate}\clearfield{urlyear}\clearfield{urlmonth}\clearfield{urlday}}\renewbibmacro*{url+urldate}{}}
```

The `\AtBeginDocument{...}` block strips URL/date clutter from bibliography
entries — keep it for clean reference slides.

Cite in text with `[cite:@key]`:
```org
- Result matches prediction [cite:@Smith2024]
- Earlier work by Jones [cite:@Jones2023] showed ...
```

The bibliography file should be declared in the deck's header:
```org
#+bibliography: ./references.bib
```

### siunitx and mhchem (quantities and chemical formulas)

Include these optional packages only when the talk has quantities with units or
chemical formulas:

```org
#+LATEX_HEADER: \usepackage{siunitx}
#+LATEX_HEADER: \usepackage[version=4]{mhchem}
```

Usage:
- `\num{1.5e-5}` → formatted number
- `\qty{4}{MeV}` → quantity with unit
- `\ce{^{8}B}` → chemical formula with isotope
- `\ce{e^{+} e^{-}}` → particle notation

## Prefer Org-native constructs over raw LaTeX

Org Beamer has native markup for most Beamer elements. **Always prefer the Org
form** — it is shorter, easier to read, and benefits from the CJK-emphasis
linter. Only drop to raw LaTeX when Org genuinely can't express the layout
(TikZ diagrams, complex tables, the thank-you overlay).

| Instead of raw LaTeX …           | … write Org-native                       |
|---|---|
| `\begin{block}{Title}`            | `*** Title` (level-3 heading)            |
| `\begin{alertblock}{Title}`       | `*** Title :B_alertblock:` + `:BEAMER_env: alertblock` |
| `\begin{exampleblock}{Title}`     | `*** Title :B_exampleblock:` + `:BEAMER_env: exampleblock` |
| `\begin{columns}` + `\column{0.5}` | `*** Col :BMCOL:` + `:BEAMER_col: 0.5`   |
| `\begin{itemize}` / `\begin{enumerate}` | `-` / `1.` (Org lists)              |
| `\begin{tabular}...\end{tabular}` | `| a | b |` pipe table + `#+attr_latex: :booktabs t` |
| `\textbf{bold}` in prose          | `*bold*` (with CJK spacing — see `org` skill) |
| `\texttt{code}` in prose          | `~code~` or `=verbatim=`                 |
| `\begin{description}`             | `- term :: definition` (Org description list) |

Level-3 headings inside a frame (`H:2`) **are** blocks — you don't wrap
content in `\begin{block}{}` because the heading already IS the block title.
See the "Blocks" and "Two columns" sections below.

## Frame structure (because `#+OPTIONS: H:2`)

- Level-1 heading `* X` → a `\section{X}` (structural divider, not a slide).
- Level-2 heading `** Y` → a **frame**; the heading text becomes the frame title.
- Keep each frame to roughly one screenful. For an unavoidably long frame:

  ```org
  ** Long frame
  :PROPERTIES:
  :BEAMER_opt: allowframebreaks
  :END:
  ```

- To shrink an over-full frame (e.g. a wide table) instead of breaking it:
  `:BEAMER_opt: shrink=15`.

### COMMENT — hide frames during development

Prefix a headline with `COMMENT` to keep it in the source but exclude it from
export. Useful for work-in-progress slides:

```org
** COMMENT Draft result    # ← this frame won't appear in the PDF
...
```

### B_ignoreheading — slides without a frame title

Some slides (Thank you, References, Backup section opener) should not display a
frame title. Use the `:B_ignoreheading:` tag plus `:BEAMER_env: ignoreheading`:

```org
* Thank you :B_ignoreheading:
:PROPERTIES:
:BEAMER_env: ignoreheading
:END:
\appendix
** 
:PROPERTIES:
:BEAMER_opt: plain,c
:END:
#+begin_export latex
\centering
{\usebeamercolor[fg]{structure}\Huge\bfseries Thank you}
#+end_export
```

The `\appendix` command must appear before backup slides so they don't count
toward the total frame number (works with `appendixnumberbeamer`).

## Blocks (level-3 headings inside a frame)

Because `#+OPTIONS: H:2`, a level-3 heading `*** Title` inside a frame
automatically becomes a block with that title. **Never** write raw
`\begin{block}{Title}...\end{block}` — the heading IS the block.

```org
** Frame title
*** My block
- Content here becomes the block body.
- Org renders this as \begin{block}{My block}.
```

### Alert block

Use `:B_alertblock:` tag with the `BEAMER_env` property:

```org
** Frame title
... content above the alert ...

*** Key message :B_alertblock:
:PROPERTIES:
:BEAMER_env: alertblock
:END:
This paragraph gets the alertblock styling — colored background, prominent.
```

### Blocks inside columns

Column headings are ALSO block headings — you don't need an extra wrapping
`\begin{block}{}` inside a column. The level-3 heading with `:BMCOL:` tag
produces both the column AND the block:

```org
** Frame title
*** Left block :BMCOL:
:PROPERTIES:
:BEAMER_col: 0.47
:END:
- This heading becomes a block inside a 0.47-wide column.

*** Right block :BMCOL:
:PROPERTIES:
:BEAMER_col: 0.47
:END:
- Same here — heading = block title, content = block body.

*** Bottom alert :B_alertblock:
:PROPERTIES:
:BEAMER_env: alertblock
:END:
This is a full-width alert block below the columns.
```

This single pattern replaces all of `\begin{columns}\column{..}\begin{block}{}`
— one heading per column/block, no raw LaTeX needed.

## Two columns

Level-3 headings with the `:BMCOL:` tag and a `BEAMER_col` property — as shown
in the Blocks section above. **House preference: figure on the LEFT, text on the
RIGHT.** Keep the text column *uncrowded* — a few short points with breathing
room, not a dense wall.

Column widths should match the content, not blindly 0.5/0.5:
- Wide figure + narrow text → 0.58/0.42, 0.6/0.4, 0.65/0.35
- Figure + text balanced → 0.5/0.5, 0.55/0.45
- Two figures side by side → 0.5/0.5

```org
** Frame with two columns
*** L :BMCOL:
:PROPERTIES:
:BEAMER_col: 0.58
:END:
#+attr_latex: :width \linewidth
#+caption: caption (only if there's room — see Figures)
[[file:./figures/example.pdf]]

*** R :BMCOL:
:PROPERTIES:
:BEAMER_col: 0.42
:END:
- short point
- short point
```

If the text column fills up, split or cut — don't add more bullets.

## Speaker notes (`:B_note:`)

Every frame can have a speaker-note block. These appear only in the presenter
view (when `\setbeameroption{show notes on second screen}` is enabled) and are
invisible in the normal PDF. Use them to capture talking points, transitions,
and backup details:

```org
** Frame title
... frame content ...

*** Speaker Notes :B_note:
:PROPERTIES:
:BEAMER_env: note
:END:
- Key talking point for this slide
- Transition to next section: "..."
- Extra details to mention if asked
```

The notes are plain Org bullet lists. Keep them short — they're for the speaker,
not the audience.

To enable presenter view (laptop + projector), uncomment in the preamble:
```org
#+LATEX_HEADER: \setbeameroption{show notes on second screen}
```

## Math

- Inline: always use `\(...\)`; do not generate `$...$`.
- Unnumbered display: always use `\[...\]`; do not generate `$$...$$`.
- Use `\begin{equation}...\end{equation}` or
  `\begin{align}...\end{align}` only when numbering, labels, or multi-line
  alignment are required; add `\label{eq:...}` for cross-references.
- Use LaTeX commands for every symbol: `\Rightarrow \to \approx \le \times \sum
  \nabla` etc. Never paste the unicode glyph (⇒ → ≈ …) — see SKILL.md gotcha #2.
- For chemical formulas in math mode: `\ce{^{8}B}` (from mhchem).
- For units: `\qty{4}{MeV}`; for bare numbers: `\num{1e-5}` (from siunitx).
- For non-italic subscripts: `m_{\mathrm{eff}}`, `E_{\nu_e}`.

## Tables

**Prefer Org-native pipe tables.** They are shorter, easier to read, and benefit
from the CJK-emphasis linter. Use `#+attr_latex: :booktabs t` to get the same
toprule/midrule/bottomrule styling that raw-LaTeX `\toprule` tables produce:

```org
#+attr_latex: :booktabs t :align lll
| Header  | Header  | Header  |
|---------+---------+---------|
| ...     | ...     | ...     |
| ...     | ...     | ...     |
```

The separator line `|---+---+---|` after the header row tells Org which rows are
the header; the `+` marks column boundaries. Org renders this to a proper LaTeX
`\begin{tabular}` with `\toprule` / `\midrule` / `\bottomrule`.

For inline verbatim / code in cells, `~code~` and `=verbatim=` work fine and
render as `\texttt{}` — the CJK-emphasis trap does **not** apply inside pipe
tables. For bold in cells, `*bold*` also works (Org pipe tables are immune to
the CJK-emphasis trap; the markers are parsed correctly regardless).

Extra `#+attr_latex` keys when needed:
- `:center t` — center the table on the slide
- `:font \footnotesize` — shrink the table to fit

**Only drop to raw LaTeX** when Org genuinely can't express the layout —
multirow/multicolumn cells, `@{}` column spacing tweaks, or custom column
decorations that `:align` doesn't cover. In that case:

```org
#+begin_export latex
\centering
\begin{tabular}{@{}lll@{}}
\toprule
Header & Header & Header \\
\midrule
... & ... & ... \\
\bottomrule
\end{tabular}
#+end_export
```

Bold a cell with `\textbf{...}` inside the export block (it's plain LaTeX there).

## Figures

Caption a figure with **`#+caption:`** — never by hand-typing a centered line of
text under the image. `#+caption:` gives a proper "Figure N: ..." label that
Beamer styles and positions; a hand-written line is just floating text.

```org
#+attr_latex: :width 0.7\textwidth
#+caption: caption text
[[file:./figures/plot.pdf]]
```

When to caption:
- **Room to spare → add a `#+caption:`.** Tight frame → drop the caption entirely
  (no caption at all; do **not** replace it with a hand-typed line).
- **Be consistent within a deck:** if any figure is captioned, caption them all;
  if you're dropping captions for space, drop them everywhere. Don't mix.
- Caption text should be the **takeaway**, not a description. Bad: "Monthly
  results". Good: "Processing time fell by 35% after the workflow change".

## TikZ diagrams

The preamble already loads `\usetikzlibrary{arrows.meta, positioning, angles,
quotes}`. For inline diagrams such as workflows, architectures, or concept maps,
write a `#+begin_export latex` block with a `tikzpicture`:

```org
#+begin_export latex
\begin{center}
\begin{tikzpicture}[
    line/.style={draw, -stealth},
    step/.style={draw, rounded corners, fill=blue!8, minimum width=2.2cm, minimum height=0.9cm},
  ]
  \node[step] (input) {Input};
  \node[step, right=1.5cm of input] (process) {Process};
  \node[step, right=1.5cm of process] (result) {Result};
  \draw[line] (input) -- (process);
  \draw[line] (process) -- (result);
\end{tikzpicture}
\end{center}
#+end_export
```

Keep diagrams simple — a Beamer slide is not a textbook figure.

## Thank you page

End the talk with a dedicated thank-you slide. Place `\appendix` before it so it
doesn't inflate the frame count:

```org
* Thank you :B_ignoreheading:
:PROPERTIES:
:BEAMER_env: ignoreheading
:END:
\appendix
**
:PROPERTIES:
:BEAMER_opt: plain,c
:END:
#+begin_export latex
\begin{tikzpicture}[remember picture,overlay]
  \fill[black!5] (current page.south west) rectangle (current page.north east);
\end{tikzpicture}
\centering
{\usebeamercolor[fg]{structure}\Huge\bfseries Thank you}

\vspace{0.8cm}
{\Large Any questions?}
#+end_export
```

`black!5` is theme-independent; replace it with a project color only when that
color is defined by the selected theme.

## References page

After the Thank you, add a references section:

```org
* References :B_ignoreheading:
:PROPERTIES:
:BEAMER_env: ignoreheading
:END:
** Ref
:PROPERTIES:
:BEAMER_env: frame
:BEAMER_opt: allowframebreaks
:END:
#+print_bibliography: :heading none
```

The `:heading none` suppresses the default "References" heading since we already
have a section divider.

## Backup slides

After references, put extra material for Q&A:

```org
* Backup :B_ignoreheading:
:PROPERTIES:
:BEAMER_env: ignoreheading
:END:
** Backup: detailed derivation
- Extra detail to expand on if asked.
```

The `\appendix` command earlier (in the Thank you section) ensures backup slides
don't count toward the frame total.

## Inline code / verbatim

`~code~` (code) and `=verbatim=` render fine, but they are emphasis markers too,
so the CJK-spacing rule applies: write `调用 ~m.migrad()~ 即可` with spaces, not
`调用~m.migrad()~即可`.
