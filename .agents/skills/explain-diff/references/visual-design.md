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

- Tokens — the color custom properties, and why there is only one accent
- Typography — three system-font families and the job each one holds
- Geometry — the 4px grid, hairlines, and the ban on shadows
- Layout — one column, one containment layer
- Diagram families — the seven, and how a change routes to one
- Code blocks — `<pre>` framed typographically, never as fake window chrome
- Interaction — the quiz, and the motion rules that apply to it
- Gates — the reject list. Read this section even if you skim the rest

## Tokens

Every color and font in the file goes through a CSS custom property. Needing
a value that has no token means adding the token first, not inlining a hex.
Mid-render improvisation is how a page ends up with eight colors: by the
third edit pass the restraint that made it readable is gone.

```css
:root {
  --paper:       #faf8f5;              /* page background, default node fill */
  --paper-2:     #f1ede7;              /* secondary fill, callout background */
  --ink:         #26221e;              /* body text, primary stroke */
  --muted:       #6b625a;              /* secondary text, default arrow stroke */
  --soft:        #948a80;              /* sublabels, axis labels */
  --rule:        rgba(38,34,30,0.12);  /* hairline borders */
  --rule-solid:  #d9d2c8;              /* stronger borders, baselines */
  --accent:      #c2410c;              /* focal only, 1-2 per diagram */
  --accent-tint: rgba(194,65,12,0.08); /* fill behind an accent border */
  --add:         #3f6212;              /* diff: added line / new path */
  --add-tint:    rgba(63,98,18,0.08);
  --del:         #9f1239;              /* diff: removed line / old path */
  --del-tint:    rgba(159,18,57,0.07);

  --font-title: ui-serif, Georgia, "Hiragino Mincho ProN", serif;
  --font-body:  system-ui, -apple-system, "Hiragino Sans", "Noto Sans JP", sans-serif;
  --font-mono:  ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;
}
```

Paper is warm-neutral, not pure white; ink is warm near-black, not `#000`.
Pure black on pure white is the sterile default every generator lands on.

`--accent` is the only decorative hue. `--add` and `--del` are semantic:
they mean "this line/path was added or removed", nothing else. Do not reach
for them to color an unrelated node, and do not introduce a fourth hue.

Light only. The page is a disposable local file opened once; a second
palette buys nothing.

## Typography

Three families, three jobs. The contrast between them is load-bearing —
it is what lets a reader tell a concept name from a symbol name at a glance.

| Role | Family | Size / weight |
|---|---|---|
| Page title, section headings | `--font-title` | 1.75rem / 1.25rem, 400 |
| Body prose | `--font-body` | 16px, 400, line-height 1.75 |
| Diagram node name | `--font-body` | 12px, 600 |
| Diagram sublabel, arrow label | `--font-mono` | 9–10px, 400 |
| Code, identifiers, paths | `--font-mono` | 13px |
| Editorial callout | `--font-title` italic | 15px |

Mono is for things the machine reads: identifiers, paths, ports, commands,
field types, literal values. Human-readable names go in the sans. A page
that sets every technical-feeling word in mono has thrown away the signal.

## Geometry

- Every coordinate, size, and gap in a diagram is divisible by 4.
- Borders are 1px hairlines (`--rule`, or `--rule-solid` for a baseline).
- `border-radius` never exceeds 10px: 4 for tags, 6 for nodes, 8 for containers.
- SVG stroke widths: 0.8 leaf/tag, 1 default, 1.2 emphasis.
- No `box-shadow`, anywhere. Depth comes from a background tint plus a
  hairline, not from a blur.

## Layout

- One column. Prose caps at ~72ch; diagrams and code may run wider.
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
   feature flags, branch conditions). Often clearer than any drawing.
6. **sequence** — vertical lifelines when ordering across services or async
   steps is the point.
7. **ui** — a simplified mockup, boxes and real label text, for a user-facing
   change. Never draw browser chrome, a window titlebar, or a phone frame
   around it.

Construction rules:

- Build in inline SVG, or in CSS grid/flex boxes. No ASCII art, no images.
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

Plain `<pre>` tags, framed typographically: a top hairline, a mono label
naming the file and line range, the code, a bottom hairline. Per-line color
goes on `<span>`s nested inside the `<pre>`, using `--add` / `--del`, and
each changed line also carries a leading `+` / `-` so the meaning does not
live in the color.

Do not draw a terminal window, traffic-light dots, or a tab bar around it.
The reader already has a real editor; a redrawn one reads as invention.

## Interaction

The only interactive element is the quiz. Clicking an option changes its
border and background instantly and reveals the one-line explanation.

- Transition named properties only (`background-color`, `border-color`);
  never `transition: all`.
- No hover scale, no bounce or elastic easing, no scroll-triggered fade-up.
- Correct and incorrect must differ by more than hue: add a `✓` / `✗` glyph
  or a border-weight change alongside the color.
- Keyboard-focusable options with an instant, non-animated focus ring.

## Gates

Reject the draft on any of these. Step 5 checks the mechanical ones by grep;
the rest belong to the fact-check pass.

- Gradient background, gradient headline text, blurred color blob, floating orb.
- Any `box-shadow`.
- Emoji standing in for an icon.
- A card inside a card.
- A three-column grid of icon-topped feature cards.
- Redrawn browser, terminal, or device chrome.
- `transition: all`, or a universal hover transform.
- A color or font written inline instead of through `var(--…)`.
- A column of numbers without `font-variant-numeric: tabular-nums`.
- A fabricated number. A number-shaped hole labelled "unverified" is honest;
  an invented statistic makes every other claim on the page unreadable.
