---
name: org-literature-agentero
description: Read, analyze, and compare papers selected through Org-mode literature files and Citar citation keys, using papers.json and Agentero CLI for local PDF parsing and source localization.
---

# Org Literature Agentero

## Resynchronize on every invocation

At the start of every skill invocation, including repeated use in the same conversation, reread the current `literature.org` and refresh `papers.json` through the existing Emacs command `grant/literature-export` for that file. Reload the resulting manifest before resolving papers; do not rely on citation mappings, paths, or paper selections remembered from an earlier invocation. Use the configured running Emacs when available, and do not save or revert the user's buffers as a side effect. If Emacs or the export command is unavailable, report that the manifest could not be refreshed, reread the available files, and clearly state any unverified or missing mappings rather than claiming synchronization succeeded.

For every paper to be read, verify its current PDF path, accessibility, and content hash against the Agentero cache on each invocation. Import and parse new or changed PDFs, and update their cache mapping before answering. Reuse unchanged parsed content only after these checks; synchronization does not require reparsing an unchanged PDF. Do not use removed or missing sources as if they were still available, and preserve unrelated cache entries.

## Select sources

Start from the paper(s), citation keys, or Org heading explicitly selected by the user for the current question. Use only the relevant entries in `literature.org` and the Emacs-generated `papers.json` to resolve those papers; the manifest maps each key to a list of PDF paths, and `[]` means no PDF was found. Report missing or inaccessible PDFs. If a key has multiple PDFs, inspect their identities before selecting one. The manifest is a lookup table, not permission to read every listed paper. If the selected paper cannot be identified from the conversation or supplied context, ask for clarification instead of choosing unrelated sources.

On every invocation, also check for a `ref/` directory inside the current literature project. If present, inspect its current contents and read the reference material relevant to the question, including local papers and explanatory documents even when they are not listed in `papers.json`. Treat `ref/` as explicitly authorized supplementary context, and distinguish its notes or interpretations from the original papers' claims. Verify the original source before attributing a note's statement to a paper. Do not follow unrelated links or symlinks outside the authorized scope.

Keep local reading limited to the selected papers, the project's `ref/` reference material, their parsed content, and the minimal manifest/cache metadata needed to access them. Do not explore repository source code, project implementations, tests, logs, unrelated notes, other PDFs, or files outside this scope to answer a paper question. Do not run project-wide searches, directory scans, or code-graph queries; searches within `ref/` and the selected source material are allowed. A paper's code link or reference list does not itself authorize reading that code or expanding the local reading scope. Read code or other local material only when the user explicitly requests it.

## Handle missing information

Answer from the selected papers first. When they do not contain the requested information, say so explicitly and identify what is missing. Do not invent details or present general knowledge, deductions, repository behavior, or external findings as statements made by the authors. Distinguish information absent from a paper from information that could not be verified because the PDF is missing, inaccessible, or poorly extracted; try available direct PDF tools before declaring that the paper lacks it.

When the selected papers and relevant `ref/` material leave a relevant gap, search the web for related scholarly literature addressing that specific gap, unless the user explicitly forbids external search. This is permission to supplement the answer with relevant literature, not to browse unrelated local files or source code. Prefer original papers and authoritative publication or preprint pages, inspect the relevant passages when accessible, and cite the source and verified section/page/equation where available. Label external findings separately from the supplied paper's content and your own reasoning. If only an abstract is accessible, or search is unavailable or inconclusive, state that limitation honestly. Do not silently add retrieved papers to `literature.org`, `papers.json`, Zotero, or the user's notes.

## Prepare or reuse Agentero content

Agentero is a CLI tool called by the agent in agent-shell. Keep the existing Citar and Appine workflow.

1. Check `agentero --version`; if Emacs has not inherited PATH, try `~/.local/bin/agentero`. Inspect `--help` or `agentero describe <op> --json` when flags are uncertain. Report missing dependencies rather than silently installing them or launching a desktop app.
2. Reuse a user-specified Vault and citation mapping when available. Otherwise use a project-specific cache under `~/.cache/org-literature-agentero/<project-id>/`, where project-id is the first 16 hex characters of SHA256 of the resolved absolute manifest path. Keep `vault/`, `citation-map.json`, and CLI configuration there. Pass `--vault` explicitly; when creating the Vault, set `XDG_CONFIG_HOME` to the cache's `config/` directory so global Vault registration is not changed.
3. In `citation-map.json`, map each citation key to its resolved `source_pdf`, `source_sha256`, and Vault-relative `paper_ref`. Verify the PDF hash and the cached paper's existence before reuse. A changed source needs a fresh cached paper and updated mapping; do not reuse stale parsed text. Preserve other mapping entries. Existing mappings without a hash must be verified against the cached PDF before adding one.
4. For a cache miss, copy only the selected PDF into the cache, using a collision-resistant filename derived from its content hash. Import the copy with `agentero --vault <vault> import pdf <copy> --no-recognize --json`; obtain `paper_ref` from the successful response, not a guessed directory name. Parse with `agentero --vault <vault> paper parse <paper_ref> --json`. Check the JSON success/error and quality fields before reading the resulting `PAPER.md`. The import's generated metadata and note scaffold belong only to the cache, not the user's research notes.

Local import with `--no-recognize` avoids unnecessary metadata-recognition requests. Authorization to read a local PDF does not by itself grant new permission to transmit it to a different external service; honor the user's existing authorization for their configured agent.

## Read and verify

Use `paper get` and `PAPER.md` for focused reading. Query `layout list` / `layout get` for section, paragraph, formula, or figure anchors only when the required layout files exist. Text parsing alone does not generate them. On `layout_raw_missing` or `layout_index_missing`, report the limitation and use original-PDF page verification; do not invent anchors. Installing or starting Agentero desktop layout analysis is a separate step to explain to the user.

For quote localization, use an existing `mark list` result where possible. `mark add --quote ... --page ...` writes a sidecar annotation: use it only inside the independent cache when needed for the task, never against the original library. Its success verifies a text location, not the scientific claim.

Treat low-quality extraction, equations, tables, figures, and ambiguous passages as reasons to inspect the relevant original PDF pages directly. If Agentero parsing fails, continue with available direct PDF tools and state which capabilities were unavailable. Distinguish physical PDF page numbers from printed page labels; `pageIndex` is zero-based.

Distinguish the authors' results and assumptions from your reasoning. Support paper-specific claims with the original Citar keys and verified page, section, equation, or figure references. Keep qualifications about imperfect separation, effective fitted components, and model-dependent interpretations.

## Output and preservation

End every answer with a "参考文献与对应位置" list or table of the sources actually used. For each source, give its title and original citation key (or a DOI/URL for external literature), the question or claim it supports, and the verified page, section, equation, figure, or table location. For `ref/` documents without a citation key, give their project-relative path and a verified heading or line location. Label PDF page numbers versus printed page labels explicitly when they differ. Distinguish user-supplied papers, `ref/` supplementary material, and externally retrieved literature. If an exact location cannot be verified, say so and give only the location actually checked (for example, the abstract); never invent page numbers or anchors. If no supporting source was found, state that explicitly instead of listing unused references.

Answer in the conversation by default. Create or edit research notes only when explicitly requested; use Org-mode, LaTeX math, and existing citation keys. Leave original PDFs, Zotero data, `literature.org`, and `ref/` material untouched. Refresh generated `papers.json` only through the existing Emacs export workflow described above; do not hand-edit mappings or add external search results. Do not migrate the library, mark papers as read, or replace the existing reading tools as a side effect of reading.
