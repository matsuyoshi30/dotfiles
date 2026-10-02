# Frontend (TypeScript/React) Review Guardrails

Guardrails for TypeScript/React idioms and pitfalls (observable models, component state, form bindings, GraphQL-backed views) — generic review concerns (naming, DRY, etc.) live in `../perspectives/`.

After writing code, self-check against these rules. When reviewing, apply the same checks to the diff.

## Contents

- Don't recompute domain logic at the call site
- Pass initialization-required values through the constructor, not a post-construction apply
- Read cross-cutting globals (feature flags, config) at the point of use
- Before adding a fetch, check whether existing state already holds the model
- Don't pipe form-owned state (canSubmit / errors) through external props
- Name booleans after their subject; split composite conditions into separate getters
- Rename stale identifiers and comments the moment their semantics change
- Keep getters and setters semantically symmetric
- Dispatch enum / union types with switch + exhaustive never, not if chains
- Leave a one-line "why" on every non-obvious escape hatch

## Don't recompute domain logic at the call site

**Rule**: If a model (aggregate) already exposes a derived property or method, don't re-implement the same logic in State / ViewModel / Component. Call the model's method instead.

**Why**: Duplicated logic drifts — one site gets a new condition added, the other doesn't, and the two judgments silently diverge. A typical result: the model reports a value as valid while the state that re-derives it rejects it.

```ts
// ❌
class CartState {
  get hasExpiredItem(): boolean {
    return this.cart.items.some(
      (i) => i.expiresAt < Date.now() || i.isDiscontinued,
    );
  }
}

// ✅
class CartState {
  get hasExpiredItem(): boolean {
    return this.cart.hasExpiredItem; // already defined on the model
  }
}
```

## Pass initialization-required values through the constructor, not a post-construction apply

**Rule**: Never design a class so that "after `Model.create(...)` you must also call `applyXxx(...)` before it's in a valid state." Take initialization-required values as constructor / factory arguments. `optional` is fine.

**Why**: When a new `create` call site is added later, the `applyXxx` call is forgotten, and the object silently operates in an invalid state. The type system can't enforce the post-hoc call, so TypeScript gives you no protection.

```ts
// ❌
const editor = DocumentEditor.create(document);
editor.applyInitialCursor(initialCursor);
// A future caller forgets the apply call and the editor starts at the wrong position

// ✅
const editor = DocumentEditor.create(document, { initialCursor });
```

## Read cross-cutting globals (feature flags, config) at the point of use

**Rule**: Don't propagate a value that isn't owned by the parent — feature flags, environment config, i18n — through a parent → child chain of reactions / props. Read it directly where you need it.

**Why**: Piping non-owned data through a parent bloats the parent's responsibility and forces edits along the whole propagation path every time a new flag is added. A flag relayed through parent model → child model → view can be read directly by the child instead.

```ts
// ❌
class ParentModel {
  constructor(private readonly featureFlag: FeatureFlag) {}
  toChildModel() {
    return new ChildModel(this.data, this.featureFlag);
  }
}

// ✅
class ChildModel {
  get isNewFlowEnabled(): boolean {
    return featureFlagStore.isEnabled("newFlow"); // read at point of use
  }
}
```

## Before adding a fetch, check whether existing state already holds the model

**Rule**: Before adding a new query / API call, check whether the current screen's State / Store already holds the same model. If it does, receive it as a function argument instead of refetching.

**Why**: Duplicate fetches don't just cost network — they create cache consistency problems (two versions of the same entity coexisting). A common case is a detail action that queries again for data the containing list state already holds.

```tsx
// ❌
function DetailButton({ itemId }: { itemId: string }) {
  const { data } = useQuery(GetItemForDetail, { variables: { itemId } });
  return <button onClick={() => openDetail(data)}>Open</button>;
}

// ✅
function DetailButton({ item }: { item: ItemModel }) {
  return <button onClick={() => openDetail(item)}>Open</button>;
}
```

## Don't pipe form-owned state (canSubmit / errors) through external props

**Rule**: State that belongs to a form object — `canSave`, `errors`, `isDirty` — must not be passed from a parent component to children as props. Look for the binding the form library or the codebase already provides and use it.

**Why**: Externalizing form state duplicates the "is submit enabled?" logic and falls out of sync with the form's own updates. A hand-rolled `canSave` lags behind the form's validation, and the submit button flickers between enabled and disabled.

```tsx
// ❌
<SubmitButton
  canSave={form.canSave}
  errorMessage={form.errors[0]}
  onClick={() => form.submit()}
/>

// ✅
<SubmitButton binding={submitButtonBindingOf(form)} />
```

## Name booleans after their subject; split composite conditions into separate getters

**Rule**: Every `is...` boolean should make clear *what* it's true about. When a value combines multiple conditions (OR / AND), keep the atomic getters separate and expose the composite as its own, additional getter.

**Why**: A getter named `isLocked` gives no hint whether the lock comes from an admin action or an automatic policy. Callers misread it, and adding a new cause later silently changes the meaning for every existing call site. One name, one meaning.

```ts
// ❌
class Account {
  get isLocked(): boolean {
    return this.lockedByAdmin || this.lockedByPolicy; // two meanings in one name
  }
}

// ✅
class Account {
  get isLockedByAdmin(): boolean { return this.lockedByAdmin; }
  get isLockedByPolicy(): boolean { return this.lockedByPolicy; }
  get isLocked(): boolean {
    return this.isLockedByAdmin || this.isLockedByPolicy;
  }
}
```

## Rename stale identifiers and comments the moment their semantics change

**Rule**: When a function's return value or a class's responsibility changes, rename the identifier (variable, method, JSDoc) in the same change. Don't defer it.

**Why**: A getter called `adminLockLabel` that also returns a policy-lock label makes every later reader ask what it actually returns. Stale identifiers are worse than missing comments — they actively mislead.

```ts
// ❌
/** Label shown when an admin locks the account */
get adminLockLabel(): string { // now also returns the policy-lock label
  return this.lockedByPolicy ? "Policy locked" : this.lockedByAdmin ? "Admin locked" : "";
}

// ✅
/** Label for any lock state (admin action or automatic policy) */
get lockLabel(): string {
  return this.lockedByPolicy ? "Policy locked" : this.lockedByAdmin ? "Admin locked" : "";
}
```

## Keep getters and setters semantically symmetric

**Rule**: If a getter is expanded to look at an additional field X, the corresponding setter / mutator must account for X as well (or neither should). Never expand only one side.

**Why**: When the getter for `isLocked` starts considering a new field but the setter still touches only the original boolean, reads and writes disagree, and a UI toggle stops reflecting immediately — a classic read/write skew bug.

```ts
// ❌
get isLocked(): boolean {
  return this.lockedByAdmin || this.lockedByPolicy; // new condition added
}
set isLocked(v: boolean) {
  this.lockedByAdmin = v; // lockedByPolicy is ignored → read/write skew
}

// ✅
get isLockedByAdmin(): boolean { return this.lockedByAdmin; }
set isLockedByAdmin(v: boolean) { this.lockedByAdmin = v; }
// Expose the composite as a read-only getter (see the booleans rule above)
```

## Dispatch enum / union types with switch + exhaustive never, not if chains

**Rule**: For enum or discriminated union dispatch, use `switch` instead of chained `if`. Add a `default` branch with `const _: never = value` so the compiler catches missing cases.

**Why**: `if` chains give you zero compiler help when a new enum case is added. When a case is added later, the existing `if` chain silently falls through its trailing `return false`, and the bug surfaces only in production.

```ts
// ❌
if (status === Status.Active) {
  return handleActive();
} else if (status === Status.Inactive) {
  return handleInactive();
}
return false; // adding a new enum case produces no warning

// ✅
switch (status) {
  case Status.Active:   return handleActive();
  case Status.Inactive: return handleInactive();
  case Status.Pending:  return false;
  default: {
    const _exhaustive: never = status;
    return _exhaustive;
  }
}
```

## Leave a one-line "why" on every non-obvious escape hatch

**Rule**: Any `min-width: 0`, `as unknown as T`, `// eslint-disable-next-line`, `!important`, `any`, or `@ts-expect-error` must carry a one-line comment explaining why it's necessary.

**Why**: Without a note, every reader asks what the `min-width: 0` is for. Worse, a later maintainer who sees no reason for it deletes it, and the layout bug it was suppressing comes back.

```tsx
// ❌
<div css={{ minWidth: 0, display: "flex" }}>
  <span>{label}</span>
</div>

// ✅
{/* min-width:0 is required so a flex child doesn't expand the parent past its intended width */}
<div css={{ minWidth: 0, display: "flex" }}>
  <span>{label}</span>
</div>
```
