# Design Doc Template

> **How to use this**: Don't try to fill in everything. The **Core** section alone is a valid doc.
> For each **Optional** section, read the 〈When you need it〉 note — if it doesn't apply, delete the whole section.
> One test governs every decision: **what's the penalty for being wrong?** If it's fixable in a few hours, it doesn't belong here.
>
> Delete this block and all the `>` guidance under each heading once you've finished writing.

---

## Core (write almost always)

### Title
<!-- The project's name. Short, distinctive, and evocative of what it does. Avoid cryptic codenames. -->

### Metadata
- **Author**: (name / email)
- **Status**: Draft / In review / Approved
- **Created**: YYYY-MM-DD
- **URL**: (canonical link or shortlink, if any)

### Objective
<!-- One sentence. Plain language any stakeholder understands. Put it on page one. -->

### Background
<!-- Why now / what problem this solves / whether there were prior attempts.
     Test: can someone reading this cold, with no verbal explanation from you, understand it? -->

### Goals
<!-- State them as impact on users, the team, or the company. Not as implementation means (e.g. "add Kubernetes"). -->
-

### Non-goals
<!-- Things readers might wrongly assume are in scope. Put them explicitly out of scope. -->
-

## Optional (keep only what applies)

### Related documents
> 〈When you need it〉 A test plan, functional spec, or related/prior design doc exists.
-

### Scenarios
> 〈When you need it〉 The goals alone don't make the finished thing easy to picture. Walk through real usage as numbered steps.
1.

### Diagrams
> 〈When you need it〉 Data flow, how components fit, or relationships with dependencies/clients are hard to convey in prose.
> Don't just paste an image — always link to the **editable source / code** (a photo becomes a dead end).

### Glossary
> 〈When you need it〉 You use internal tool names or acronyms that out-of-team or new readers won't know.
> Better options first: use self-explanatory terms, or define them inline. This is the last resort.
- **Term**: definition

### Constraints
> 〈When you need it〉 Budget, clients, infrastructure, or dependencies constrain the design.
-

### Service level objectives (SLOs)
> 〈When you need it〉 A vague requirement like "performant" needs to become a measurable number.
- Availability:
- Latency (e.g. p50 ≤ ◯ms):
- Scale:

### Monitoring / alerting
> 〈When you need it〉 You've defined SLOs. How will you measure them in production and detect failures?
- Alert trigger conditions:

### Timeline
> 〈When you need it〉 Multi-person coordination or deadlines. Cut each milestone so it yields a *usable artifact*
> (e.g. start with a dummy-data UI → surface misunderstood requirements early).
- **Milestone 1 (YYYY-MM-DD)**:
- **Milestone 2 (YYYY-MM-DD)**:

### Interfaces
> 〈When you need it〉 The contact points with people or other systems are central. UI (rough sketches are fine) / API · CLI / file format.
> For changed endpoints, note backward compatibility and error handling.

### Data model / migration
> 〈When you need it〉 You change a schema, storage layout, or persisted data.
> Focus on what's hard to reverse — backfill, rollback, and whether it ships without downtime.
- Schema / data changes:
- Backfill:
- Rollback:
- Downtime (can it ship without downtime?):

### Dependencies / infrastructure
> 〈When you need it〉 You're choosing language, libraries, storage, or runtime.
> Concentrate on what's **hard to change later** (language, storage); don't over-think easily swappable pieces.
- Language:
- Runtime / where it runs:
- Where persistent data lives:
- Key third-party packages:

### Security
> 〈When you need it〉 You process potentially malicious data, or cross a privilege boundary. If you think there's no threat, write **why**.
- Threats considered:
- Attack surface:
- Trust boundaries:

### Privacy
> 〈When you need it〉 You handle sensitive data.
- Sensitive data handled:
- Retention period:
- Who has access:
- How it's protected (encryption at rest / in transit, etc.):

### Legal considerations
> 〈When you need it〉 Regulated domain, contractual limits, OSS license choice, or ways things could break the law if they go wrong.

### Logging
> 〈When you need it〉 You're planning for future bug / performance / incident investigation.
- Events logged:
- Log levels / storage / retention / access:
- Sensitive data to keep out of logs:

---

## Appendix

### Open issues
> For each entry: what the problem is / the options / the **immediate next step**.
> **Open issue: (title)**
> Problem:
> Options:
> Proposed solution:
> Next step:

### Resolved issues
> When resolved, move it here with the **decision** up front. Keep the original discussion for posterity.
> **Resolved: (title)**
> Decision:
> (original discussion below)

### Alternatives considered
> Preempt the "why didn't you do X?" question. Strong alternatives and why they failed, **a few lines each**. Don't catalog every rejected idea.
- **Option**: why rejected
