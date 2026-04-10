# Kotlin Review Guardrails

Guardrails for patterns Kotlin PR reviews. Scoped to Kotlin-specific idioms and pitfalls — generic review concerns (naming, DRY, etc.) live in the main `code-reviewer` SKILL.md.

After writing code, self-check against these rules. When reviewing, apply the same checks to the diff.

## Contents

- Don't add `= null` default arguments casually
- Don't add meaningless `?:` fallbacks
- Avoid overusing `takeIf` and scope functions
- Use `listOfNotNull` and collection APIs for nullable aggregation
- Express predicates as extension functions using `in`
- Order parameters so trailing-lambda syntax works
- Put validation in `init` / factory
- Exposed (ORM): no `selectAll()`, always limit single-row reads
- Avoid N+1 queries — fetch in bulk

## Don't add `= null` default arguments casually

**Rule**: Do not add `= null` defaults to parameters of domain models, factories, DTOs, or mutation inputs. If data "must always exist," express it as non-null.

**Why**: LLM code generators (Claude/Copilot included) tend to add `= null` for call-site convenience. The result: data that *should* always exist becomes nullable, and downstream code grows defensive null-checks and fallbacks that should never fire. This has been flagged repeatedly in review:

**Self-check after generation**: run `rg '= null' <edited file>`.

```kotlin
// ❌
data class CreateUserInput(
    val email: String,
    val displayName: String? = null,   // Added because "caller might want to omit it"
    val profile: Profile? = null,      // Can it really be absent?
)

// ✅
data class CreateUserInput(
    val email: String,
    val displayName: String?,   // Nullable if absence has meaning — but no default
    val profile: Profile,       // Non-null if it must always exist
)
```

## Don't add meaningless `?:` fallbacks

**Rule**: Do not add `value ?: someDefault` unless you can justify why that specific fallback value is correct. "Better than crashing" is not a justification.

**Why**: A fallback added "just in case" silently mixes out-of-spec data into the happy path and hides bugs. It's almost always better to fail loudly or branch explicitly. Real review feedback:

```kotlin
// ❌
val user = repository.find(id) ?: User.empty()   // What does empty() mean semantically?

// ✅ Guarantee via the type if it can't be missing
val user: User = repository.find(id)   // Let it throw if not found

// ✅ Branch explicitly at the call site if it can be missing
val user = repository.find(id)
    ?: return Result.NotFound
```

## Avoid overusing `takeIf` and scope functions

**Rule**: For simple branching, use a plain `if` instead of chaining `takeIf` / `let` / `run`. Use scope functions only when (a) the result type changes, or (b) you want to eliminate a temporary variable.

**Why**: Chained scope functions impose reading cost for no real gain. Review feedback has explicitly rejected this pattern:

```kotlin
// ❌
val profile = user.takeIf { it.isActive }?.let { fetchProfile(it.id) }

// ✅
val profile = if (user.isActive) fetchProfile(user.id) else null
```

## Use `listOfNotNull` and collection APIs for nullable aggregation

**Rule**: When collecting values that might be null, use `listOfNotNull` / `mapNotNull` instead of manual filtering. Use `maxOf` / `maxOrNull` / `minOrNull` for min/max.

**Why**: Hand-written null-skipping loops are noisy and error-prone. Real review feedback:

```kotlin
// ❌
val values = mutableListOf<Int>()
if (a != null) values.add(a)
if (b != null) values.add(b)
val largest = values.maxOrNull()

// ✅
val largest = listOfNotNull(a, b).maxOrNull()
```

## Express predicates as extension functions using `in`

**Rule**: When a predicate on an enum / value class is checked in multiple places, extract it into an extension function backed by a `Set`, and use `in`. Don't leak the check logic to call sites.

**Why**: When new cases are added later, scattered `==` comparisons are easy to miss. Review feedback suggested exactly this rewrite:

```kotlin
// ❌ The check logic leaks to every call site
if (status == Status.CANCELLED || status == Status.FAILED) { ... }

// ✅ The predicate lives with the type
private val terminalStatuses = setOf(Status.CANCELLED, Status.FAILED, Status.DONE)
fun Status.isTerminal() = this in terminalStatuses

if (status.isTerminal()) { ... }
```

## Order parameters so trailing-lambda syntax works

**Rule**: When defining a higher-order function, put the lambda parameter last. If you can't, redesign the signature.

**Why**: Call sites become awkward without trailing-lambda syntax. Review feedback:

```kotlin
// ❌
fun <T> retry(block: () -> T, times: Int): T { ... }
retry({ fetch() }, 3)

// ✅
fun <T> retry(times: Int, block: () -> T): T { ... }
retry(3) {
    fetch()
}
```

## Put validation in `init` / factory

**Rule**: Enforce model invariants in an `init` block or a companion factory using `require` / `check`. Do not scatter validation across service, resolver, or presenter layers.

**Why**: If invariants aren't enforced at construction time, every caller ends up writing defensive checks. Kotlin's `init` block makes this concise — use it. Review feedback:

```kotlin
// ❌ Validation in the service layer
class OrderService {
    fun create(input: OrderInput) {
        if (input.quantity <= 0) error("quantity must be positive")
        // ...
    }
}

// ✅ Validation in the model's init block
data class OrderInput(val quantity: Int, /* ... */) {
    init {
        require(quantity > 0) { "quantity must be positive" }
    }
}
```

## Exposed (ORM): no `selectAll()`, always limit single-row reads

**Rule**: When using JetBrains Exposed or a similar Kotlin DSL ORM:
- Select only the columns you need with `select(col1, col2, ...)` — do not call `selectAll()`
- For single-row reads, use `orderBy(...).limit(1).firstOrNull()` — do not call `firstOrNull()` on an unbounded query
- For existence checks, use `.exists()` / `.any()` instead of fetching a row and checking `!= null`

**Why**: `selectAll()` pulls unneeded columns and degrades query plans as the schema grows. A bare `firstOrNull()` on some DSLs fetches the entire result set before returning the first row, which explodes under real data volume. Existence checks should drop to SQL `EXISTS` rather than materializing rows. Review feedback:

```kotlin
// ❌
val row = UserTable.selectAll().where { UserTable.email eq email }.firstOrNull()
val found = repo.find(id) != null

// ✅
val row = UserTable
    .select(UserTable.id, UserTable.name)
    .where { UserTable.email eq email }
    .orderBy(UserTable.createdAt to SortOrder.DESC)
    .limit(1)
    .firstOrNull()

val found = UserTable.select(UserTable.id).where { UserTable.email eq email }.any()
```

## Avoid N+1 queries — fetch in bulk

**Rule**: Do not issue a query inside a loop over an already-fetched collection. When you have a list of parent rows and need related data, fetch all related rows in a single query (`WHERE parent_id IN (...)`) and group them in memory.

**Why**: Calling a repository inside `map` / `forEach` turns one request into N+1 queries. It looks fine in unit tests with 2-3 rows and melts under production data volume. This is one of the most common review rejections on repository/service code.

**Red flags to grep for**: `.map { repo.` / `.forEach { ... repo.` / `.associateWith { repo.` / any repository call inside a collection transform.

```kotlin
// ❌ N+1: one query per user
val usersWithOrders = users.map { user ->
    user to orderRepository.findByUserId(user.id)
}

// ✅ One query, grouped in memory
val ordersByUserId = orderRepository
    .findByUserIds(users.map { it.id })
    .groupBy { it.userId }

val usersWithOrders = users.map { user ->
    user to (ordersByUserId[user.id].orEmpty())
}
```

If the relation goes through GraphQL resolvers (DataLoader pattern) or a batching layer, prefer that over a manual `IN` query — but never leave the naive per-item repository call in place.
