# Visual design for the explainer page

The design system for the HTML this skill produces. It exists so the page
reads as a deliberate technical document rather than a generated one, and so
its diagrams carry the explanation instead of decorating it.

Everything here is self-contained: no font CDN, no icon library, no image
host, no script tag pointing outward. That is not a stylistic preference —
Step 5 greps for `href="http"` and fails the file.

Adapted from the ideas in `Nutlope/hallmark` (anti-slop gates) and
`cathrynlavery/diagram-design` (editorial diagram system), both MIT. Neither
is a dependency; nothing is fetched at run time.

## Contents

- The stylesheet — paste it verbatim; do not re-derive it
- Tokens — the color custom properties, and why there is only one accent
- Typography — three system-font families and the job each one holds
- Geometry — the 4px grid, hairlines, and the ban on shadows
- Layout — one column, one width, one containment layer
- Diagram families — the seven, and how a change routes to one
- Diagram construction — the measure, the type scale, arrowheads, and what
  SVG text is allowed to hold
- Code blocks — one span per line, and why that structure is load-bearing
- Interaction — the quiz, the inline/split diff toggle, and the motion rules
- Gates — the reject list. Read this section even if you skim the rest

## The stylesheet

Paste the block below into the page's `<style>` element verbatim, before
writing a single section. It is not a starting point to adapt. Every rule in
it was added to fix a specific defect observed in a page this skill
produced, and re-deriving CSS per run is what produced those defects:
a document whose prose, code and diagrams each ended up at a different
width, and diff rows separated by blank lines.

Add to it only when a section genuinely needs a class that isn't here (a new
diagram family's container, say). Never restate a rule that is already
below with a different value.

```css
:root {
  --paper:       #faf8f5;              /* page background, default node fill */
  --paper-2:     #f1ede7;              /* secondary fill, code panel, callout */
  --ink:         #26221e;              /* body text, primary stroke */
  --muted:       #6b625a;              /* secondary text, default arrow stroke */
  --soft:        #948a80;              /* eyebrow labels, footer */
  --rule:        rgba(38,34,30,0.12);  /* hairline borders */
  --rule-solid:  #d9d2c8;              /* stronger borders, baselines */
  --accent:      #c2410c;              /* focal only, 1-2 per diagram */
  --accent-tint: rgba(194,65,12,0.10); /* fill behind an accent border */
  --add:         #3f6212;              /* diff: added line / new path */
  --add-tint:    rgba(63,98,18,0.16);
  --del:         #9f1239;              /* diff: removed line / old path */
  --del-tint:    rgba(159,18,57,0.14);

  --measure:     48em;                 /* the one width; see Layout */

  --font-title: ui-serif, Georgia, "Hiragino Mincho ProN", serif;
  --font-body:  system-ui, -apple-system, "Hiragino Sans", "Noto Sans JP", sans-serif;
  --font-mono:  ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;
}
* { box-sizing: border-box; }
body {
  margin: 0;
  padding: 48px 24px 96px;
  background: var(--paper);
  color: var(--ink);
  font-family: var(--font-body);
  font-size: 16px;
  line-height: 1.8;
}
/* One measure for the whole document. Prose, code, diagrams and tables all
   share this single edge; a second width is what makes the page look ragged. */
.wrap { max-width: var(--measure); margin: 0 auto; }
h1 {
  font-family: var(--font-title);
  font-size: 1.75rem;
  font-weight: 400;
  line-height: 1.4;
  margin: 0 0 8px;
}
.subtitle { color: var(--muted); font-size: 14px; line-height: 1.6; margin: 0 0 4px; }
.subtitle a { color: var(--muted); }
h2 {
  font-family: var(--font-title);
  font-size: 1.25rem;
  font-weight: 400;
  margin: 56px 0 16px;
  padding-top: 16px;
  border-top: 1px solid var(--rule-solid);
}
h3 {
  font-family: var(--font-body);
  font-size: 15px;
  font-weight: 600;
  margin: 32px 0 8px;
}
p { margin: 0 0 18px; }
ul, ol { padding-left: 1.4em; }
li { margin-bottom: 6px; }
code {
  font-family: var(--font-mono);
  font-size: 13px;
  background: var(--paper-2);
  padding: 1px 4px;
  border-radius: 4px;
  /* A long identifier is one unbreakable token; without this it pushes the
     whole page into horizontal scroll on a narrow window. */
  overflow-wrap: anywhere;
}
a { color: var(--accent); }

nav.toc {
  border: 1px solid var(--rule-solid);
  border-radius: 8px;
  padding: 16px 24px;
  margin: 32px 0 8px;
  background: var(--paper-2);
}
nav.toc p { font-size: 12px; color: var(--soft); margin: 0 0 8px; font-family: var(--font-mono); }
nav.toc ol { margin: 0; }
nav.toc li { margin-bottom: 2px; }

.callout {
  border-left: 2px solid var(--accent);
  background: var(--accent-tint);
  padding: 14px 20px;
  margin: 28px 0;
  border-radius: 0 6px 6px 0;
}
.callout p { margin: 0; }
.callout p + p { margin-top: 8px; }
.callout .label {
  font-family: var(--font-mono);
  font-size: 10px;
  letter-spacing: 0.08em;
  color: var(--accent);
  display: block;
  margin-bottom: 4px;
}

/* A code block is one panel: label lid, framed body, four sides. */
.codeblock { margin: 28px 0; }
.codeblock .filelabel {
  font-family: var(--font-mono);
  font-size: 11px;
  line-height: 1.5;
  color: var(--muted);
  background: var(--paper-2);
  border: 1px solid var(--rule-solid);
  border-bottom: 0;
  border-radius: 8px 8px 0 0;
  padding: 8px 16px;
  display: block;
  overflow-wrap: anywhere;
}
pre {
  white-space: normal;
  overflow-x: auto;
  font-family: var(--font-mono);
  font-size: 13px;
  line-height: 1.7;
  margin: 0;
  padding: 12px 0;
  background: var(--paper-2);
  border: 1px solid var(--rule-solid);
  border-radius: 0 0 8px 8px;
}
/* Every line is its own block span. The pre collapses the newlines between
   them (which would otherwise render as blank rows) and each span restores
   pre semantics for its own text; min-width keeps a row's tint reaching the
   right edge once the block scrolls horizontally. */
pre > span {
  display: block;
  white-space: pre;
  padding: 0 16px 0 14px;
  min-width: max-content;
  border-left: 2px solid transparent;
}
pre > span.add { color: var(--add); background: var(--add-tint); border-left-color: var(--add); }
pre > span.del { color: var(--del); background: var(--del-tint); border-left-color: var(--del); }
pre .cm { color: var(--muted); }

/* Diff view toggle. The authored code block is the only copy of the code;
   the split view is derived from it at run time. The control sits on each
   block but switches every block at once — a reader picks one reading mode
   for the page, not one per block. */
.codeblock .filelabel { display: flex; align-items: baseline; justify-content: space-between; gap: 16px; }
.codeblock .filelabel .path { min-width: 0; overflow-wrap: anywhere; }
.viewtoggle { flex: none; display: flex; gap: 10px; }
.viewtoggle button {
  font-family: var(--font-mono);
  font-size: 11px;
  line-height: 1.4;
  color: var(--soft);
  background: none;
  border: 0;
  border-bottom: 1px solid transparent;
  padding: 0;
  cursor: pointer;
  transition: color 0.12s ease, border-color 0.12s ease;
}
.viewtoggle button:hover { color: var(--muted); }
.viewtoggle button[aria-pressed="true"] { color: var(--accent); border-bottom-color: var(--accent); }
.viewtoggle button:focus-visible { outline: 2px solid var(--accent); outline-offset: 2px; }

/* Each side takes half the measure and scrolls on its own; the script keeps
   the two scroll positions equal. Sizing the tracks to their content instead
   would push the right column past the right edge of the page, which is the
   one thing a side-by-side view cannot afford. */
.split-view {
  display: grid;
  grid-template-columns: 1fr 1fr;
  background: var(--paper-2);
  border: 1px solid var(--rule-solid);
  border-radius: 0 0 8px 8px;
}
.split-view[hidden] { display: none; }
.split-view > pre { min-width: 0; border: 0; border-radius: 0; }
.split-view > pre + pre { border-left: 1px solid var(--rule-solid); }
pre > span.pad { background: var(--paper); }

figure { margin: 36px 0; }
figcaption {
  font-size: 12px;
  line-height: 1.7;
  color: var(--muted);
  margin-top: 10px;
}
.scroller { overflow-x: auto; }

/* Diagram type scale lives here, not in per-element attributes, so every
   diagram in the page shares one set of sizes. */
svg { display: block; width: 100%; height: auto; }
svg text { font-family: var(--font-body); fill: var(--ink); }
svg text.mono { font-family: var(--font-mono); }
svg .hdr { font-size: 13px; font-weight: 600; }
svg .lbl { font-size: 14px; font-weight: 600; }
svg .sub { font-size: 12px; fill: var(--muted); }

table {
  border-collapse: collapse;
  font-size: 13px;
  width: 100%;
  font-variant-numeric: tabular-nums;
}
th, td {
  text-align: left;
  padding: 9px 12px;
  border-bottom: 1px solid var(--rule);
  vertical-align: top;
  line-height: 1.6;
}
th {
  font-size: 11px;
  font-family: var(--font-mono);
  color: var(--muted);
  font-weight: 400;
  border-bottom: 1px solid var(--rule-solid);
}
td.mono { font-family: var(--font-mono); font-size: 12px; }
tr.is-new td { background: var(--accent-tint); }

/* The Code section's file map. Group rows carry the reading order, so the
   table needs no priority column. */
.filemap tr.grp th {
  font-family: var(--font-body);
  font-size: 12px;
  font-weight: 600;
  color: var(--ink);
  padding-top: 20px;
  border-bottom: 1px solid var(--rule-solid);
}
.filemap td.path { font-family: var(--font-mono); font-size: 11px; overflow-wrap: anywhere; width: 42%; }
.filemap td.stat, .filemap td.sec { font-family: var(--font-mono); font-size: 11px; white-space: nowrap; }
.filemap td.stat { width: 9%; }
.filemap td.sec { width: 5%; }
/* An identifier that has to break mid-word looks broken inside a tinted box. */
.filemap td code { background: none; padding: 0; }

.quiz { margin-top: 24px; }
.q { margin: 0 0 40px; }
.q .qtext { font-weight: 600; margin-bottom: 12px; }
.q .qnum { font-family: var(--font-mono); font-size: 11px; color: var(--soft); display: block; font-weight: 400; }
.opt {
  display: block;
  width: 100%;
  text-align: left;
  font-family: var(--font-body);
  font-size: 14px;
  line-height: 1.7;
  color: var(--ink);
  background: var(--paper);
  border: 1px solid var(--rule-solid);
  border-radius: 6px;
  padding: 10px 14px;
  margin-bottom: 8px;
  cursor: pointer;
  transition: background-color 0.12s ease, border-color 0.12s ease;
}
.opt:hover { background: var(--paper-2); }
.opt:focus-visible { outline: 2px solid var(--accent); outline-offset: 2px; }
.opt.correct { border-color: var(--add); border-width: 2px; background: var(--add-tint); }
.opt.wrong { border-color: var(--del); background: var(--del-tint); }
.opt .mark { font-family: var(--font-mono); margin-right: 8px; }
.opt .why { display: block; font-size: 12px; color: var(--muted); margin-top: 6px; }
footer {
  margin-top: 64px;
  padding-top: 16px;
  border-top: 1px solid var(--rule-solid);
  font-size: 12px;
  color: var(--soft);
  font-family: var(--font-mono);
}
```

## Tokens

Every color and font in the file goes through a CSS custom property. Needing
a value that has no token means adding the token first, not inlining a hex.
Mid-render improvisation is how a page ends up with eight colors: by the
third edit pass the restraint that made it readable is gone.

Paper is warm-neutral, not pure white; ink is warm near-black, not `#000`.
Pure black on pure white is the sterile default every generator lands on.

`--accent` is the only decorative hue. `--add` and `--del` are semantic:
they mean "this line/path was added or removed", nothing else. Do not reach
for them to color an unrelated node, and do not introduce a fourth hue.

The tint alphas are set for reading on `--paper-2`, which is where the diff
rows sit. Lowering them produces a wash that barely registers — the first
version of this file had them at 0.07 and the diff coloring was invisible.

Light only. The page is a disposable local file opened once; a second
palette buys nothing.

## Typography

Three families, three jobs. The contrast between them is load-bearing —
it is what lets a reader tell a concept name from a symbol name at a glance.

| Role | Family | Size / weight |
|---|---|---|
| Page title, section headings | `--font-title` | 1.75rem / 1.25rem, 400 |
| Body prose | `--font-body` | 16px, 400, line-height 1.8 |
| Diagram panel header (`.hdr`) | `--font-body` | 13px, 600 |
| Diagram node name (`.lbl`) | `--font-body` | 14px, 600 |
| Diagram sublabel, arrow label (`.sub`) | `--font-body` | 12px |
| Code, identifiers, paths | `--font-mono` | 13px |
| Editorial callout | `--font-title` italic | 15px |

Mono is for things the machine reads: identifiers, paths, ports, commands,
field types, literal values. Human-readable names go in the sans. A page
that sets every technical-feeling word in mono has thrown away the signal.

**In a diagram, prose goes in the sans even when it is short.** Setting a
Japanese arrow label in `--font-mono` at 10px — which an earlier version of
this file recommended — renders it in a fallback face at a size 40% below
the body copy sitting two centimetres away, and that mismatch is most of
why a diagram reads as cramped. Mono inside a diagram is for the literal
token only (`FOR UPDATE`, `batch upsert`), via `class="mono"`.

## Geometry

- Every coordinate, size, and gap in a diagram is divisible by 4.
- Borders are 1px hairlines (`--rule`, or `--rule-solid` for a baseline).
- `border-radius` never exceeds 10px: 4 for tags, 6 for nodes, 8 for containers.
- SVG stroke widths: 1 for a lifeline or panel hairline, 1.2 for an
  emphasised node border, 1.5 for a connector or message arrow.
- No `box-shadow`, anywhere. Depth comes from a background tint plus a
  hairline, not from a blur.

## Layout

- One column, **one width**. `.wrap` is capped at `--measure` and nothing
  inside it sets a second maximum: prose, code blocks, figures and tables
  all end on the same right edge. A page whose paragraphs stop at one column
  width while its code and diagrams run to another looks broken even when
  every individual element is well made, and that is the single most visible
  defect this file exists to prevent.
- `--measure` is `48em`, sized by the **code**, not the prose. Kotlin and
  TypeScript lines are the binding constraint; a measure chosen for
  comfortable prose puts every code block into horizontal scroll. Do not use
  `ch` for this: `ch` is the advance of `0`, so `72ch` on a Japanese page is
  about 40 full-width characters, not 72 — a unit that silently means
  something other than what it says is how the widths drifted apart in the
  first place.
- Text is left-aligned. Centering everything is the fastest way to look templated.
- The table of contents is a plain anchor list at the top. No sticky sidebar,
  no progress bar, no floating chrome.
- One containment layer. A bordered card holding bordered cards is the
  card-in-card tell; pick the layer that carries meaning and drop the other.

## Diagram families

Seven families cover what a code change needs to show. Pick by the shape of
the change, not by variety — reusing one family three times is better than
inventing three visuals, because the reader learns the visual grammar once.

Mark each diagram with an HTML comment naming its family
(`<!-- diagram: before-after -->`), so reuse is visible when re-reading the draft.

1. **before-after** — two panels with identical geometry, side by side; only
   the changed node differs and carries the accent. The default choice for a
   diff, and the one family a reader can always parse without a legend.
2. **flow** — boxes and arrows carrying concrete toy values (`orderId=42`,
   `status=DRAFT`), not type names. A flow labelled with types explains nothing
   the signature didn't already say.
3. **state** — a state machine for lifecycle or status changes. Transitions the
   diff adds are accent; pre-existing ones are muted.
4. **modules** — nested boxes for a refactor or a move: what lives where, and
   which boundary the change crosses.
5. **matrix** — an HTML table for condition x outcome logic (permissions,
   feature flags, branch conditions). Often clearer than any drawing. Not
   every table is one: the Code section's diff map is navigation, so it
   carries no family tag.
6. **sequence** — vertical lifelines when ordering across services or async
   steps is the point.
7. **ui** — a simplified mockup, boxes and real label text, for a user-facing
   change. Never draw browser chrome, a window titlebar, or a phone frame
   around it.

## Diagram construction

- Build in inline SVG, or in CSS grid/flex boxes. No ASCII art, no images.
- **`viewBox="0 0 768 H"`.** 768 is `--measure` at the default 16px root, so
  the diagram renders at 1:1 and its text lands at the size the type scale
  says. A wider viewBox is scaled down by `width: 100%` and every label
  shrinks with it — a 840-wide diagram renders its 12px labels at 11px.
  Height is whatever the content needs; set no `width`/`height` attributes.
  An SVG needs no `.scroller` wrapper — at the measure it already fits, and on
  a narrow window it scales down rather than scrolling. Its labels get small
  there; that is the accepted trade for a file opened on a desktop. Tables do
  need the wrapper, since their columns cannot shrink past their content.
- **A two-panel comparison is two 368-wide columns at x=0 and x=400.** Both
  panels keep the same node width so the eye can diff them by row.
- **SVG `<text>` holds labels, never sentences.** A node name, a sublabel, an
  arrow label — up to about 24 characters. Anything longer is a conclusion,
  and a conclusion belongs in the `figcaption` or the paragraph beside the
  figure, where it wraps, scales with the reader's font, and is selectable.
  Paragraphs typeset as SVG text is the second most common reason a diagram
  on this page reads badly.
- **Every text uses a scale class** (`hdr` / `lbl` / `sub`), not a
  `font-size` attribute. Add `class="mono"` for a literal token.
- **Annotate below, not far right.** A cost or condition note goes on a
  second line inside its node as a `.sub`, 21px under the label. Right-
  aligning it against the node's far edge leaves a lake of empty space
  between a name and the thing that qualifies it.
- **Connectors carry arrowheads.** Define one `<marker>` per stroke color in
  a `<defs>` at the top of each SVG, with ids unique across the whole
  document (`ah-a`, `ah-m`, `ah-d`, …) — duplicate ids in one HTML file are
  invalid and the second definition is ignored:

  ```svg
  <marker id="ah-a" viewBox="0 0 10 10" refX="9" refY="5"
          markerWidth="5" markerHeight="5" orient="auto">
    <path d="M0,0 L10,5 L0,10 z" fill="var(--muted)"/>
  </marker>
  ```

  A stack of boxes joined by plain hairlines reads as unrelated cards.
- **A sequence lane label is centred on its lifeline** (`text-anchor="middle"`
  at the lifeline's x), except the leftmost, which starts at x=8 so it isn't
  clipped.
- One or two focal elements per diagram, in `--accent`. Everything else is
  ink or muted. Three focal points means no focal point.
- Every node earns its place. The highest-quality edit to a diagram is
  usually a deletion.
- A diagram must carry information the adjacent prose and code block do not
  already state. One that restates the paragraph above it is decoration —
  cut it, and keep the paragraph.
- Never encode meaning in color alone. Pair the accent with a label, a
  dashed stroke, or a position difference, so the diagram survives a
  grayscale print and a red-green colorblind reader.
- Give each SVG `role="img"` and a `<title>` naming what it shows.

## Code blocks

A `<pre>` framed as a panel: a mono file-and-line label as the lid, the code
in a bordered body, four sides closed. Do not draw a terminal window,
traffic-light dots, or a tab bar around it. The reader already has a real
editor; a redrawn one reads as invention.

**Every line inside the `<pre>` is its own `<span>`** — changed lines take
`class="add"` / `class="del"`, unchanged lines `class="ln"` — and each
changed line also carries a leading `+` / `-` so the meaning does not live
in the color:

```html
<div class="codeblock">
<span class="filelabel">path/to/File.kt:161-207 (抜粋)</span>
<pre><span class="add">+ val requestIds = distinctItems.map { it.requestId }</span>
<span class="ln">  requestRepository.requestsBelongsToOrganization(orgId, requestIds)</span>
<span class="del">- requestRepository.findLatestRequestRevisionByRequestId(id)</span>
<span class="ln">  <span class="cm">// ... 残りは省略</span></span>
</pre>
</div>
```

Wrapping *every* line, including unchanged ones, is not tidiness. The
stylesheet sets `white-space: normal` on the `pre` and restores
`white-space: pre` on the spans, which is what makes the newlines *between*
the spans collapse. Leave one line as a bare text node and it loses its
indentation and wraps; use the older markup where only changed lines are
spans and each `display: block` span is followed by a rendered newline, so
the diff comes out double-spaced.

Nested spans (`.cm` for a comment) sit *inside* a line span and stay inline —
the block rule is `pre > span`, direct children only.

### The diff map

The table that closes the Code section (Step 4 of `SKILL.md` says what belongs
in it, and why it goes last) is `<table class="filemap">` with four columns — file, diffstat, role,
and the walkthrough subsection that covers it. A group is a full-width row
inside the `<tbody>`, not a second table:

```html
<tr class="grp"><th colspan="4" scope="colgroup">核心 (4 ファイル) — …</th></tr>
<tr>
  <td class="path">shared/usecase/NursingConfirmableRequestUseCase.kt</td>
  <td class="stat">+70 −22</td>
  <td>一括受領と一括差し戻しの本体</td>
  <td class="sec">1・2</td>
</tr>
```

Paths are shortened to a common prefix stated once in the `figcaption`; the
column set has to stay this narrow because 25 full paths at the measure do
not fit otherwise. A file the walkthrough did not open individually gets
`—` in the last column, which is what makes the map an honest coverage
statement rather than a decorated file list.

## Interaction

Two interactive elements, and no others: the quiz, and the inline/split
toggle on diff code blocks. Both are described below; anything beyond them
is decoration on a page meant to be read once.

### The quiz

Clicking an option changes its border and background instantly and reveals
the one-line explanation.

- The quiz container's id must not collide with the section heading's
  anchor id. `<h2 id="quiz">` followed by `<div id="quiz">` makes
  `getElementById` return the heading, and all five questions get appended
  inside an `<h2>` — where they inherit the serif title font, and the page
  ships with buttons nested in a heading. Name them `quiz` and `quiz-body`.
- Transition named properties only (`background-color`, `border-color`);
  never `transition: all`.
- No hover scale, no bounce or elastic easing, no scroll-triggered fade-up.
- Correct and incorrect must differ by more than hue: add a `✓` / `✗` glyph
  or a border-weight change alongside the color.
- Keyboard-focusable options with an instant, non-animated focus ring.

### The inline/split toggle

A diff reads better one way or the other depending on the change, so a code
block holding **both** removals and additions gets a control to switch
between the inline view (one column of `+`/`-` rows) and a split view
(before on the left, after on the right). A block that is pure additions
does not get one: its split view is an empty column beside a full one.

The authored markup does not change. The `<pre>` in the file stays the only
copy of the code; the script below derives the split view from it and builds
the control. Append it to the page's existing `<script>` block, and
translate `DIFF_VIEW_LABELS` and the group's `aria-label` into the
document's language.

- The control is text with an underlined active state, not two boxed tabs.
  Boxed tabs over a code panel read as the redrawn editor chrome the gates
  ban.
- Clicking it switches **every** diff block on the page. A reader picks one
  reading mode for the document, not one per block; putting the control on
  each block is for discoverability, not for independent state.
- Each split pane scrolls horizontally and the script keeps the two scroll
  positions equal. Sizing the columns to their content instead —
  `minmax(max-content, 1fr)` — pushes the right-hand column off the page,
  which is the one thing a side-by-side view cannot afford.
- Removals pair row-for-row with the additions that follow them, and the
  longer run continues against blank rows carrying `--paper` so the absence
  is visible. That pairing is why removals go before their replacements
  inside a change run.

```js
const DIFF_VIEW_LABELS = { inline: "インライン", split: "分割" };

const diffBlocks = [...document.querySelectorAll(".codeblock")].filter(
  (b) => b.querySelector("pre > span.add") && b.querySelector("pre > span.del")
);

function splitRows(pre) {
  const lines = [...pre.children];
  const rows = [];
  let i = 0;
  while (i < lines.length) {
    const cls = lines[i].classList;
    if (!cls.contains("add") && !cls.contains("del")) {
      rows.push([lines[i], lines[i]]);
      i++;
      continue;
    }
    // A run of removals pairs row-for-row with the run of additions that
    // follows it; the longer side keeps going against blank rows.
    const dels = [];
    const adds = [];
    while (i < lines.length && lines[i].classList.contains("del")) dels.push(lines[i++]);
    while (i < lines.length && lines[i].classList.contains("add")) adds.push(lines[i++]);
    for (let k = 0; k < Math.max(dels.length, adds.length); k++) rows.push([dels[k], adds[k]]);
  }
  return rows;
}

function diffCell(line) {
  if (line) return line.cloneNode(true);
  const pad = document.createElement("span");
  pad.className = "ln pad";
  pad.textContent = "\u00a0";
  return pad;
}

function setDiffView(mode) {
  diffBlocks.forEach((b) => {
    b.querySelector("pre.inline-view").hidden = mode === "split";
    b.querySelector(".split-view").hidden = mode !== "split";
    b.querySelectorAll(".viewtoggle button").forEach((btn) =>
      btn.setAttribute("aria-pressed", String(btn.dataset.mode === mode))
    );
  });
}

diffBlocks.forEach((block) => {
  const pre = block.querySelector("pre");
  pre.classList.add("inline-view");

  const split = document.createElement("div");
  split.className = "split-view";
  const left = document.createElement("pre");
  const right = document.createElement("pre");
  splitRows(pre).forEach(([l, r]) => {
    left.appendChild(diffCell(l));
    right.appendChild(diffCell(r));
  });
  split.append(left, right);
  pre.after(split);

  // Keep the two sides on the same column so a row still reads across.
  [left, right].forEach((pane, i, panes) => {
    const other = panes[1 - i];
    pane.addEventListener("scroll", () => {
      if (other.scrollLeft !== pane.scrollLeft) other.scrollLeft = pane.scrollLeft;
    });
  });

  const label = block.querySelector(".filelabel");
  const path = document.createElement("span");
  path.className = "path";
  while (label.firstChild) path.appendChild(label.firstChild);
  label.appendChild(path);

  const group = document.createElement("span");
  group.className = "viewtoggle";
  group.setAttribute("role", "group");
  group.setAttribute("aria-label", "差分の表示");
  Object.entries(DIFF_VIEW_LABELS).forEach(([mode, text]) => {
    const btn = document.createElement("button");
    btn.type = "button";
    btn.textContent = text;
    btn.dataset.mode = mode;
    btn.addEventListener("click", () => setDiffView(mode));
    group.appendChild(btn);
  });
  label.appendChild(group);
});

setDiffView("inline");
```

## Gates

Reject the draft on any of these. Step 5 checks the mechanical ones by grep;
the rest belong to the fact-check pass.

- A second width: any `max-width` other than `.wrap`'s, or an SVG whose
  viewBox is not 768 wide.
- A `<pre>` line that is not wrapped in a `<span>`.
- A sentence typeset as SVG `<text>`.
- A duplicate `id` anywhere in the document.
- Gradient background, gradient headline text, blurred color blob, floating orb.
- Any `box-shadow`.
- Emoji standing in for an icon.
- A card inside a card.
- A three-column grid of icon-topped feature cards.
- Redrawn browser, terminal, or device chrome — including boxed tabs over a
  code panel. The inline/split control is text with an underline, for this
  reason.
- `transition: all`, or a universal hover transform.
- A color or font written inline instead of through `var(--…)`.
- A column of numbers without `font-variant-numeric: tabular-nums`.
- A fabricated number. A number-shaped hole labelled "unverified" is honest;
  an invented statistic makes every other claim on the page unreadable.
