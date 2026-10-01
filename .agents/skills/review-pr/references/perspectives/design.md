# Design

Does the change fit the shape of the code around it?

- Identifiers and types — whether identifiers and category values are passed around as raw strings or UUIDs. Whether the representable states are minimal (if there is no need to distinguish absent from empty, collapse them into one)
- Reuse — whether the same logic already exists somewhere. Whether it is consistent with sibling implementations (other kinds, other screens, sibling modules), and if not, whether there is a reason
- Ripple — whether the same mistake, or the same rewrite, is also needed in other files or for other kinds
- Coupling — what the change makes one part know about another: internal details, shared mutable state, control flags that steer the callee, a whole structure passed where a few fields are used
- Cohesion — whether what changes together lives together. Whether the change scatters one concept across modules, or packs unrelated responsibilities into one unit

Judge coupling as a balance, not against a fixed target. Do not push every dependency toward the weakest form (passing plain data) by default. Weigh how strong the coupling is against how far apart the two parts live (same module, across modules, across services or teams) and how often they change:

- Strong coupling between parts that live close together and change together is fine, and often clearer than an indirection added to avoid it
- The same strength across a module or service boundary, or toward a part that changes often, is what makes changes ripple
- Loosening coupling has costs too: extra mapping layers, duplicated models, and logic split away from the data it needs. Splitting things that always change together is as much a cohesion problem as lumping together things that do not

Raise a coupling or cohesion finding only when you can name the likely change that would now have to touch both sides, and how far it has to travel. "This could be more decoupled" without that change is not a finding.
