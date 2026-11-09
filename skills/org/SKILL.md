---
name: org
description: >-
  Author and edit Org-mode (.org) files safely, especially content exported to
  LaTeX/PDF or HTML and Chinese/CJK documents. Use when creating, converting, or
  making non-trivial edits to Org reports, notes, papers, READMEs, and slides.
  Includes source checks for common CJK and LaTeX export issues. For Org
  Beamer decks, use the org-beamer workflow.
---

# Editing Org-mode files safely

Org → LaTeX is full of **silent** failures: the file exports without an error (or
with a warning buried in thousands of log lines), but the rendered PDF is wrong —
a literal `*` where you wanted bold, a blank where a symbol should be. You cannot
catch these by reading the source, so this skill pairs a short set of conventions
with a linter that finds them mechanically.

## Workflow

1. **Write / edit the Org content** following the seven rules below.
2. **Lint new Org files and substantive edits.** Run
   `python3 "<this-skill-dir>/scripts/lint_org.py" FILE.org`. For Beamer decks,
   use the Beamer lint entry point instead. It runs these same checks.
   Review diagnostics against the diff. Fix issues introduced by the edit and
   preserve existing prose, punctuation, and formatting outside its scope.
3. **Verify exported deliverables in the project environment.** For a PDF,
   compile with the project's configured exporter, then inspect rendered pages
   for missing glyphs, malformed markup, and layout. Report source lint,
   compilation, and visual inspection separately. If the toolchain is
   unavailable, return the Org source and report which checks could run.

## The seven silent traps (and the *why*)

### 1. Use ASCII punctuation and isolate emphasis from CJK

Org only parses `*bold*`, `/italic/`, `=verbatim=`, and `~code~` when each marker
has a valid boundary. A Chinese character or full-width punctuation is not a
valid boundary, so the marker can remain literal instead of becoming emphasis.

Use ASCII punctuation throughout Chinese Org prose:

| Avoid | Use |
|---|---|
| `，。；：！？` | `, . ; : ! ?` |
| `（）【】` | `() []` |
| `“”‘’` | `"" ''` |
| `、` | `,` or `/` with surrounding spaces |
| `…` / `—` | `...` / `---` |

Also put an ASCII space between CJK text and every emphasis marker:

```
这是*重点*内容。         -> literal *重点* (no \textbf)
这是/斜体/内容。         -> literal /斜体/ (no \emph)
借助*确认*、/重试/。     -> markers touch CJK text and full-width punctuation
```

Write the same content with ASCII punctuation and spaces around the markup:

```
这是 *重点* 内容.          -> \textbf{重点}
这是 /斜体/ 内容.          -> \emph{斜体}
借助 *确认* 和 /重试/.     -> both spans export correctly
```

For slash-separated prose, write `反射 / 扩张 / 收缩`, not
`反射/扩张/收缩`; this prevents the middle segment from looking like a broken
`/italic/` span. The linter checks `* / = ~`. It still skips `_` and `+` because
they collide heavily with identifiers, subscripts, and ordinary notation.

### 2. Unicode symbols missing from the body font → blank output or build failure

**Symbols/dings** — dingbats (✓ ✗), arrows (→ ⇒), math operators (≈ ≤ ∑ × √)
typed as **literal text** have no glyph in a Latin body font (Times New Roman,
Latin Modern, …). The build emits `Missing character: There is no ✓ (U+2713)...`
and renders nothing. Use a LaTeX command, or a plain word:

| Don't (literal unicode) | Do |
|---|---|
| `误差 ✓ / ✗`            | `误差 有 / 无` (or `\(\checkmark\)` (amssymb) / `\(\times\)`) |
| `a → b`, `p ⇒ q`        | `a \(\to\) b`, `p \(\Rightarrow\) q` |
| `≈ ≤ ∑ × √`             | `\(\approx\)` `\(\le\)` `\(\sum\)` `\(\times\)` `\(\sqrt{}\)` |

**Mathematical Alphanumeric Symbols** — characters like 𝑡 (U+1D461, "mathematical
italic small T") and 𝐀 (U+1D400, "mathematical bold capital A") look like
italic/bold letters but are **separate Unicode code points** with NO glyph in any
standard LaTeX font. They commonly sneak in when copying from arXiv abstracts
(which use MathJax), PDFs, Word documents, or AI-generated text. Replace them
with a math-mode equivalent:

| Don't (Unicode math letter) | Do |
|---|---|
| `变量 𝑡 和 𝑎`            | `变量 \(t\) 和 \(a\)` |
| `矩阵 𝐀`                 | `矩阵 \(\mathbf{A}\)` |
| `集合 ℕ`                 | `集合 \(\mathbb{N}\)` |

CJK letters are fine. In Org prose, still write punctuation with ASCII forms as
required by rule 1, even when the selected font contains the Unicode glyph.

### 3. En-dash for ranges/connections → write `--`

A literal en-dash `–` (U+2013) for a range should be `--` in the source so LaTeX
renders the right dash; inside math use `\text{--}`:

```
10–100              →  10--100      (text)
\(10^6–10^9\)       →  \(10^6\text{--}10^9\)   (math)
```

For an aside, use the LaTeX-style ASCII em-dash source `---` rather than a
literal Unicode em-dash.

### 4. Use `\(...\)` and `\[...\]`, not dollar delimiters

Use Org's recommended LaTeX fragment delimiters consistently:

| Purpose | Avoid | Use |
|---|---|---|
| Inline math | `$E = mc^2$` | `\(E = mc^2\)` |
| Unnumbered display math | `$$E = mc^2$$` | `\[E = mc^2\]` |

Use `equation`, `align`, or related environments only when numbering, labels, or
multi-line alignment are required. The linter rejects `$...$` and `$$...$$` in
Org prose, while raw LaTeX and source blocks remain exempt.

### 5. Math-only command in running text → "Missing $ inserted"

Trap #2's twin: once you correctly pick a LaTeX command over a unicode glyph, it
must still be in **math mode**. `\to`, `\Rightarrow`, `\alpha`, `\frac{}{}`,
`\le`, `\sum` … are math-only and error in plain text. Wrap them:

```
延迟 \to 选 TCP        →  延迟 \(\to\) 选 TCP
参数 \alpha 控制       →  参数 \(\alpha\) 控制
```

### 6. Hand-escaped LaTeX specials in plain text → backslash + broken output

Org escapes LaTeX specials in plain text **itself**: `50%` exports as `50\%`,
`Tom & Jerry` exports as `Tom \& Jerry`. Writing `\%`, `\&`, `\$`, `\#`,
`\{`, `\}` by hand "to be safe" backfires — each one exports as
`$\backslash$` followed by the raw special character, which then has its
normal LaTeX effect:

| Hand-escaped | Exports as              | Result                                     |
|-------------|-------------------------|--------------------------------------------|
| `80\%`      | `$\backslash$%`         | Backslash visible; `%` comments out the rest of the line |
| `Tom \& Jerry` | `$\backslash$&`      | `&` becomes a tabular alignment tab → "Misplaced alignment tab character &" error |
| `\$100`     | `$\backslash$$`         | `$` starts math mode; everything after looks like math |
| `\#1`       | `$\backslash$#`         | `#` is a macro parameter char → "Illegal parameter number" error |
| `\{key\}`   | `$\backslash${`…`$\backslash$}` | `{`/`}` become unbalanced → "Too many }'s" or missing braces |

The rule is the same as for `%`: **write the character as-is in Org plain text.**
Org escapes it correctly. The backslash form belongs only inside math fragments
(`\(...\)`, `\[...\]`, `\begin{equation}`...) and raw `#+begin_export latex` /
`#+begin_src` blocks where content passes through verbatim.

### 7. Backslash-underscore in plain text → garbled identifiers

`\_` in Org **prose** is not a literal underscore — org-latex exports it as
`$\backslash$\textsubscript{...}`, which garbles identifiers in the PDF:

```
pywa\_iminuit         →  pywa$\backslash$\textsubscript{iminuit}   (broken!)
MMC\_C\_2p2MeV        →  MMC$\backslash$\textsubscript{C}$\backslash$\textsubscript{2p2MeV}
```

Fix: **wrap the whole identifier in `~verbatim~`** so Org exports it as
`\texttt{name}` without touching the underscores:

```
~pywa_iminuit~        →  \texttt{pywa\_iminuit}   (correct)
~MMC_C_2p2MeV~        →  \texttt{MMC\_C\_2p2MeV}
```

`\_` is safe inside `~verbatim~`, `=verbatim=`, and math `\(...\)`/`\[...\]` —
the linter only flags it in plain prose.

## A few general Org-editing reminders

- **Headlines** are `* `, `** `, … with a literal space after the stars. A line
  starting with `*nospace` is *not* a headline — it's an emphasis attempt.
- **Preserve structure when editing**: keep `#+PROPERTY`/`:PROPERTIES:` drawers,
  `#+OPTIONS`, and the existing heading depth; many export settings live there.
- **Use ASCII punctuation in Chinese Org prose** and keep ASCII spaces between
  CJK text and `* / = ~` emphasis markers.
- **Tables**: native `| a | b |` pipe tables are fine and immune to the emphasis
  trap; for full control over alignment/rules write raw LaTeX in a
  `#+begin_export latex ... #+end_export` block (also immune — it's plain LaTeX,
  so use `\textbf{}` there, not Org `*...*`).
- **Match the current project's style** (preamble, theme, author) when adding to
  a project that has other `.org` files — inspect a sibling and preserve its
  relevant header settings.

