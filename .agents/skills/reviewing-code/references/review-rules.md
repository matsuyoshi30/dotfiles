# Review rules

Shared by the orchestrator and every perspective reviewer. The orchestrator assigns the final disposition; a perspective reviewer proposes one using the same rules.

## Substantiating a finding

Spotting something suspicious is not enough to make it a finding. Only write that something breaks when you can construct the path to the breakage yourself.

- Attack the claim "this change works correctly". If you can break it, write the breaking steps as a numbered list
  - Include what input, what ordering of operations, what interleaved transaction, what flag state, or what mid-migration data gets you there
- A candidate you could not write steps for is not a finding: turn it into a question for the author, or drop it
  - Do not ship "this might be a problem" as a finding
- Argue against your own steps once. If they collapse because callers are limited, a guard exists upstream, that data cannot exist, or it only applies during a migration window, drop the finding
- Do not raise a finding merely because a shape matches a convention. If you cannot say what that shape breaks in this context, or which reader it misleads and how, do not raise it
- Do not silently drop areas you could not substantiate but remain uneasy about — keep them as "Unverified"
  - Do not write them up as if they were confirmed

## Dispositions

Every finding must carry one of the following. Do not ship a finding you cannot assign one to.

- Blocker — a defect you wrote breaking steps for. Fix before merge
- Needs decision — whether it breaks depends on a spec, operational, or rollout-ordering decision. Requires a human call before merge
- Follow-up — worth fixing, but not a reason to hold this change
- Nits — taste and readability. No need to fix
- Question — a doubt you cannot call a defect. Depending on the answer it may become a Blocker

Two boundaries come up in most reviews.

- Blocker or Needs decision — ask whether the correct behaviour is already settled. If it is, and the code does not do it, that is a Blocker; an author who described the intent and then implemented half of it has written a defect, not raised a question. Reach for Needs decision only when the correct behaviour is genuinely still open
- Needs decision or Question — a Needs decision carries breaking steps for the branch that breaks. If you cannot write those steps for either branch, what you have is a Question

A repository's severity words do not map onto these one for one: theirs rank how much the team cares, yours rank what you could substantiate. Let their severity direct your attention, not set your disposition. A rule they mark must-fix still needs breaking steps from you before it is a Blocker; without them it is a Follow-up, and their label belongs in the Reason. Do not carry their vocabulary or a mapping table into the output — name the criterion you applied and let the disposition say the rest.

## Anchoring a finding

Every finding carries a repository-root-relative path and line number.
Good: `server/orders/query/OrderQuery.kt:36-62`
Bad: `OrderQuery.kt:36-62`

Line numbers are the ones the author will see once the change lands.

- For a line the diff shows, take the number the annotated diff prints beside it (`+N` and `ctxN` are after-image numbers). Do not derive numbers from hunk headers
- For a deleted line (`-N`, a before-image number), anchor at the neighbouring `+` or `ctx` line where it used to be
- For an unchanged line the diff does not show, read it from the working directory. For a local target that number is already the after-image. For a PR, in a file the PR modifies, that number is pre-change; anchor on a line the diff shows instead when you can
- If you cannot pin a single line, give the range you did verify rather than a number you guessed
- Anchor at the line this change touched. When the reason lives elsewhere — a caller, a sibling implementation — cite that path in the body, not in the anchor
- A finding may sit on a file the change does not touch only when the change breaks that file. Say in the body that the file is unchanged, so the author knows the line is not theirs to look for in the diff. An untouched sibling the change merely leaves unaligned is not broken by it: anchor at the changed line, as in the bullet above
