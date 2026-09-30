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
- Diagram families — the common set, and the routing table that decides which ones a change needs
- Diagram construction — the measure, the type scale, arrowheads, what SVG text is allowed to hold, and the notation for class, ER and use case diagrams
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
/* Diff marks on labels. A fill attribute on <text> loses to the rules
   above, so a mark set as an attribute never renders; these classes win. */
svg text.add { fill: var(--add); }
svg text.del { fill: var(--del); }
svg text.chg { fill: var(--accent); }

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
td.add, th.add { background: var(--add-tint); }
td.del, th.del { background: var(--del-tint); }
td.chg, th.chg { background: var(--accent-tint); }

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
.opt .why[hidden] { display: none; }
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

A family is an established notation a reviewer may already read: UML, ER, DFD, C4 and the like. The family is decided by what the change *is*, through the routing table below, never by variety or by habit. A page that draws every change as two panels of boxes has skipped the routing: a schema change shown without its tables, or a new call path shown without its order, leaves the reader to reconstruct from code the one structure a picture carries better than prose.

The list below is the common set, not a closed one. When an aspect of the change is better shown by another established notation — a deployment diagram for where a process now runs, a data flow diagram for a pipeline, a timing diagram for a timeout or retry schedule, an object diagram for one concrete instance graph, a swimlane activity for work handed between people, a C4 context diagram for a new external system — use it, and name it by its common name in kebab-case (`deployment`, `dataflow`, `timing`). What is not allowed is an invented visual with no notation behind it: the reader has to learn its grammar from scratch, and it usually turns out to be boxes and arrows that mean nothing in particular.

Mark each diagram with an HTML comment naming its family (`<!-- diagram: er -->`).

1. **er** — entities with their columns (`PK` / `FK` marked) and crow's-foot cardinality between them, for schema changes.
2. **class** — UML-style class boxes: name, the members the change touches, and inheritance / implementation / dependency edges between types.
3. **sequence** — vertical lifelines when ordering across layers, services or async steps is the point. Messages carry toy values.
4. **usecase** — actors outside a system boundary, use cases as ovals inside it: who can now do what, and what they can no longer do.
5. **state** — a state machine for lifecycle or status changes.
6. **flow** — an activity diagram: boxes, decisions and arrows carrying concrete toy values (`orderId=42`, `status=DRAFT`), not type names. A flow labelled with types explains nothing the signature didn't already say.
7. **modules** — nested boxes for a refactor or a move: what lives where, and which dependency crosses which boundary.
8. **matrix** — an HTML table for condition x outcome logic (permissions, feature flags, branch conditions). Not every table is one: the Code section's diff map is navigation, so it carries no family tag.
9. **ui** — a simplified mockup, boxes and real label text, for a user-facing change. Never draw browser chrome, a window titlebar, or a phone frame around it.
10. **before-after** — two panels with identical geometry, side by side; only the changed part differs. The fallback when no notation fits, typically a behavior or performance change whose shape is not a schema, a type graph, an ordering or a lifecycle. When a structural diagram would overlap old and new in one drawing — a responsibility moving between classes — draw that notation in two panels instead and tag it with the notation (`class`), not `before-after`.

### Routing

Before writing, read the diff's core against this table. Every row whose signal is present gets its family, and a change that touches a schema, a call order and a type hierarchy gets all three diagrams: several families on one page is the normal case, not a smell. A signal that appears only in test fixtures or mechanical changes (a generated file, a rename) does not count.

A row fires when the diff changes the thing the row names, not when that thing merely appears in it. A new enum whose values gain no new transitions is a type, so it goes in `class`, not `state`; a new DNS name or a new field on an existing query gives no actor a new operation, so it is not `usecase`. The test: would this diagram's diff marks land on the element the row is about? A `state` diagram where every transition is pre-existing, or a `usecase` diagram whose only new oval restates a hostname, fails it — drop the row rather than draw it to be safe.

| Signal in the diff | Family | What the diagram must show |
|---|---|---|
| Migration, DDL, ORM entity or table mapping, schema file | `er` | The touched tables, added / removed columns, keys, and cardinality between them |
| New or changed class, interface, sealed hierarchy, DTO, DI wiring | `class` | The types, the members the change touches, and inheritance / implementation / dependency |
| New or reordered calls across layers or services, async job, event, transaction boundary, external API | `sequence` | Participants, message order with toy values, and the step the change inserts or removes |
| An actor (user role, client application, external system) can perform an operation it could not before, or loses one: new mutation or endpoint, screen action, batch, role or permission scope | `usecase` | Actors, the system boundary, and the added / removed use cases |
| A state, transition or guard of a lifecycle or workflow is added, removed or changed | `state` | States and transitions, with the guard on each changed one |
| Branching, validation, retry, or an algorithm | `flow` | The decision path, followed with one concrete toy value |
| Permission, role, feature flag, or condition-dependent config | `matrix` | Condition x outcome, with the changed cells marked |
| File or package move, new module, dependency direction | `modules` | The boundaries and which dependency crosses them |
| Visible UI | `ui` | The screen region that changed, with its real labels |
| Infrastructure, deploy target, container or queue topology | `deployment` | Nodes, what runs on each, and the connection the change adds or moves |
| Data moving through a pipeline, ETL, sync, or export | `dataflow` | Sources, processes, stores, and which flow the change adds or reroutes |
| Timeout, retry, backoff, TTL, schedule, or rate limit | `timing` | A time axis with the events the change moves, using real durations |
| Anything else with an established notation | its notation | Whatever that notation exists to show |
| None of the above | `before-after` | The observable difference in behavior |

The rows are the common signals, not an exhaustive list, and the last two are in that order on purpose: reach for another established notation before falling back to two panels of boxes.

The routed diagrams go in the Diagrams section, ordered from the outside in: who is affected (`usecase`, `ui`) first, then structure (`er`, `class`, `modules`), then behavior (`sequence`, `state`, `flow`). Background, Intuition and Code may add a diagram that zooms into one part or follows a toy value through it.

One piece of logic can match two rows — a feature-flag branch is both a condition (`matrix`) and a branch (`flow`). Draw the `matrix` when the question is which input gives which outcome; add the `flow` only when the order or placement of the checks matters to the reader (an early return that skips a query, a check that runs once per batch rather than per key). Two figures answering the same question is one too many.

An element that is new as a whole — a new class, table or participant — carries the mark on the element: its outline and its name. Its members need no marks of their own, since all of them are new.

Record every family used anywhere on the page once, near the top of the `<body>`, as a space-separated list: `<!-- diagram-plan: usecase er sequence -->`. Step 5 checks that every planned family is drawn and that nothing outside the plan is, and the fact check compares the plan against the diff.

### Diff marks

Any family marks its own diff, so a structural diagram usually needs no second panel:

- Added element: stroke `var(--add)` on its shape, `class="add"` on its label, and a `+` prefix on the label (or a `.sub` reading `NEW`).
- Removed element: stroke `var(--del)`, `class="del"` on its label, and a `−` prefix. Add `stroke-dasharray="4 4"` only where the notation gives dashes no meaning of its own; in a sequence diagram (replies), a class diagram (dependency, implementation) or a deployment diagram (dependency), a dashed removal reads as that relationship, so keep the notation's line style and let the color and the `−` carry the removal.
- Changed element: stroke `var(--accent)` on its shape, `class="chg"` on its label. In a diagram `--accent` means changed and nothing else: the focal point of a diff diagram is the element the change touched, so do not also spend the accent on an unchanged node to draw the eye.
- In a `matrix` table, the same three classes go on the `<td>` / `<th>` that changed. When a whole column is new, mark its header and say so in the caption rather than tinting every cell.

Color labels through these classes, never a `fill` attribute on `<text>`: the stylesheet's `svg text` and `.sub` rules outrank a presentation attribute, so an attribute-colored label renders in ink or muted and the mark silently disappears. Shape strokes are not styled by the sheet, so a `stroke` attribute on them works.

The prefix is what keeps the meaning off color alone. `--add` and `--del` marks do not count against the one-or-two accent limit below; an ER diagram with five new columns is five `--add` rows and still has one focal point.

When old and new share no elements to mark against — every participant of a call path replaced, say — draw the notation twice, as two panels, and tag it with the notation. Do not borrow a construct that means something else at run time, such as a sequence `alt` fragment guarded by "before" and "after".

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
- **A two-panel comparison is two 368-wide columns at x=0 and x=400.** Both panels keep the same node width so the eye can diff them by row. When a panel cannot fit 368 — a sequence with four participants, or 40-character message labels — stack the panels instead: before above after in one SVG, full width, with each shared lifeline or node at the same x in both, so the eye diffs them by column.
- **SVG `<text>` holds labels, never sentences.** A node name, a sublabel, an arrow label — up to 24 characters. A `class="mono"` literal (a class member, a column definition, a message with its toy value) may run to 40, since identifiers are long and cannot be reworded. Anything longer is a conclusion, and a conclusion belongs in the `figcaption` or the paragraph beside the figure, where it wraps, scales with the reader's font, and is selectable. Paragraphs typeset as SVG text is the second most common reason a diagram on this page reads badly.
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

  A stack of boxes joined by plain hairlines reads as unrelated cards. The exceptions are the notations whose lines are not directed: an ER relationship ends in cardinality marks, a use case association is a plain line, and so is a communication path between deployment nodes (a dependency between them keeps its dashed arrow).
- **A sequence lane label is centred on its lifeline** (`text-anchor="middle"`
  at the lifeline's x), except the leftmost, which starts at x=8 so it isn't
  clipped.
- One or two focal elements per diagram, in `--accent`. Everything else is ink or muted, apart from the diff marks. Three focal points means no focal point.
- Every node earns its place: draw the tables, types and participants the change touches plus the neighbours needed to read them, not the whole system.
- A diagram must carry information the adjacent prose and code block do not already state — cardinality, call order, ownership, which transition is new. One that restates the paragraph above it is redrawn to carry that, not deleted; a diagram the routing plan calls for is not optional.
- Label with real names. A node reading 「処理」「データ」「システム」 or an arrow reading 「改善」 fits every PR and so explains none; if a box has no better name, it probably should not be in the diagram.
- Never encode meaning in color alone. Pair the accent with a label, a
  dashed stroke, or a position difference, so the diagram survives a
  grayscale print and a red-green colorblind reader.
- Give each SVG `role="img"` and a `<title>` naming what it shows.

### Notation for the structural families

Hand-drawn SVG is enough for these; keep the standard shapes so a reader who knows the notation reads it without a legend.

- **class** — a box with a name compartment (`lbl mono`, since a class name is an identifier and often passes 24 characters, with a `.sub` stereotype such as `«interface»` above it when relevant), a hairline, then one member per line as `.sub mono`. Leave out UML visibility markers, so a leading `+` / `−` can only mean the diff mark. Show only the members the change adds, removes or calls; signatures drop parameter lists when they would pass 40 characters. Inheritance is a solid line to a hollow triangle, implementation the same line dashed, dependency a dashed line to an open arrowhead.
- **er** — a box per table: the table name as `lbl mono`, a hairline, then one column per line as `.sub mono` with `PK` / `FK` in front. Relationships are plain lines ending in crow's-foot marks on each side.
- **usecase** — a boundary rectangle with the system name as `.hdr` at its top left, use cases as ellipses inside it with `.lbl` names, actors outside as a minimal stick figure (a circle and four strokes, `stroke="var(--ink)"`) or a box labelled `«actor»` for a non-human one, with the role name below. Emoji are not an actor glyph.

The markers these need, ids unique across the document as before. The hollow triangle's fill is a token, not white, so it passes the hex gate:

```svg
<marker id="inh" viewBox="0 0 12 12" refX="12" refY="6"
        markerWidth="12" markerHeight="12" markerUnits="userSpaceOnUse" orient="auto">
  <path d="M0,0 L12,6 L0,12 z" fill="var(--paper)" stroke="var(--ink)" stroke-width="1"/>
</marker>
<marker id="cf-many" viewBox="0 0 12 12" refX="12" refY="6"
        markerWidth="12" markerHeight="12" markerUnits="userSpaceOnUse" orient="auto-start-reverse">
  <path d="M0,6 L12,0 M0,6 L12,6 M0,6 L12,12" fill="none" stroke="var(--muted)" stroke-width="1.2"/>
</marker>
<marker id="cf-one" viewBox="0 0 12 12" refX="12" refY="6"
        markerWidth="12" markerHeight="12" markerUnits="userSpaceOnUse" orient="auto-start-reverse">
  <path d="M0,6 L12,6 M8,0 L8,12" fill="none" stroke="var(--muted)" stroke-width="1.2"/>
</marker>
```

`orient="auto-start-reverse"` lets one crow's-foot marker serve as both `marker-start` and `marker-end`.

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
<tr class="grp"><th colspan="4" scope="colgroup">核心の 4 ファイル：…</th></tr>
<tr>
  <td class="path">shared/usecase/NursingConfirmableRequestUseCase.kt</td>
  <td class="stat">+70 −22</td>
  <td>一括受領と一括差し戻しの本体</td>
  <td class="sec">1、2</td>
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
- A routing signal present in the diff's core with no diagram of its family, or a drawn family missing from the `diagram-plan`.
- A diagram with no established notation behind it, or one labelled with words that would fit any PR (「処理」「改善」「システム」).
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
