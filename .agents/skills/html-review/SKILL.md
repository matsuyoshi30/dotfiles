---
name: html-review
description: Use when the user asks to publish something for a human to review in the browser ("html-review で出して", "レビュー用に公開して", "ブラウザで見られる形にして"), or to pick up the comments they left there ("コメント見て", "inbox 確認して", "レビューの指摘取り込んで"). Wraps the `html-review` CLI, which must already be on PATH.
---

# html-review

A round trip between you and a human reviewer: render what you wrote as HTML, publish it to the
review app, then collect the comments they left in the browser and act on them.

Announce at start: "Using html-review skill to publish this for review." (or "…to pick up the
review comments")

## Prerequisites

`publish`, `list`, `inbox`, and `complete` talk to the server and need all three of these environment
variables. Missing even one makes the CLI exit 1.

- `HTML_REVIEW_BASE_URL`
- `CF_ACCESS_CLIENT_ID`
- `CF_ACCESS_CLIENT_SECRET`

`build` runs locally and needs none of them — never hold up a build over missing env vars. If they
turn out to be unset once you reach `publish`, say so and stop. Don't guess values or substitute
your own; setup lives in the html-review repo's `docs/setup.md`.

## Quick reference

| Step | Command |
| --- | --- |
| Build HTML from blocks.json | `html-review build blocks.json -o outdir/` |
| Publish, first time | `html-review publish outdir/` |
| Publish an update | `html-review publish outdir/ --id <item-id>` |
| Look up an item's last `version=` before republishing | `html-review list` |
| List unresolved comments | `html-review inbox` |
| Close comments you addressed | `html-review complete <id>... --note "what you did"` |

## Publishing

### 1. Write blocks.json

```json
{
  "title": "Document title",
  "blocks": [
    { "type": "heading", "level": 2, "text": "Heading" },
    { "type": "paragraph", "text": "Body text" },
    { "type": "image", "src": "assets/figure.png", "alt": "What the figure shows" },
    { "type": "mermaid", "code": "graph TD; A-->B;" },
    { "type": "graphviz", "code": "digraph { a -> b }" }
  ]
}
```

These five `type`s are the whole vocabulary, and `heading` `level` is 1, 2, or 3.

An `image` `src` resolves relative to the directory holding blocks.json, not the process's working
directory — keep the JSON and its images together. Extensions are limited to `.png`, `.jpg`,
`.jpeg`, `.gif`, `.webp`, `.svg`.

`mermaid` and `graphviz` render to SVG at build time. Graphviz shells out to `dot`, so on
`dot: command not found`, tell the user to run `brew install graphviz`.

### 2. Build

Build deletes the output directory before writing to it, so point `-o` at a fresh directory that
holds nothing you need. It refuses outright when the target is the working directory, an ancestor of
it, or `$HOME`, which is why `-o .` fails by design. Omitting `-o` writes to `dist-site`.

### 3. Publish and hand over the URL

The first publish takes no `--id`. Record the `id=` from the output and give the human the `url=`.
Content is deleted 30 days after the item is created, comments included; mention that if the review
is unlikely to be prompt.

Every later publish of the same document needs `--id <item-id>` — it becomes a new version at the
same URL. Then check the output: `version=` must be higher than last time. A `version=1` back means
you created a second item, and the human is still looking at the stale URL. If you don't already
have last time's `version=` on hand — a fresh session, a long gap since the last publish — run
`html-review list` first to look it up instead of guessing.

## Collecting comments

### 4. Read the inbox

`html-review inbox` lists unresolved comments across every published item, oldest first — not only
the one you published last. Each entry gives the comment text and what it targets (a block, a
selected passage, a diagram), the item id, the block id, the review URL, and the comment id.

### 5. Fix first, then close

```bash
html-review complete <comment-id> <comment-id> --note "what you changed"
```

Close only comments you have actually addressed, and only after addressing them. Addressed means
republished, not just edited locally: a fix in `blocks.json` doesn't count until you've re-run
`publish --id <item-id>` (step 3) and the human can see it there. Close before republishing and the
comment vanishes from `inbox` while the human is still looking at the old content — neither of you
gets a signal anything's wrong. A closed comment drops out of `inbox`, so anything closed early is
silently lost. Re-run `inbox` afterwards and confirm the ids you closed are gone.

## Revising a document that still has open comments

Block ids are assigned positionally at build time: `b1`, `b2`, … in `blocks` array order. They are
not content hashes. Insert blocks in the middle and every id after the insertion point shifts by
however many blocks you inserted — a heading-plus-paragraph section shifts everything after it by
two, not one. Since a human's comments remember their position by id, existing comments start
pointing at the wrong block.

While unresolved comments exist, append to the end rather than inserting in the middle, or clear the
inbox before restructuring. If you genuinely must insert mid-document, tell the human which comments
will end up pointing at the wrong block and wait for their go-ahead before you publish that version —
don't just notify and proceed. Building the edit locally first is fine; publishing is the point it
becomes real for them.
