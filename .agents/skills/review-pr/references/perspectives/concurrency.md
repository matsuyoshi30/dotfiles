# Concurrency and consistency

What happens when state is shared or interleaved?

- Concurrency — what happens when another transaction, request, async task, or UI event interleaves between read, decide, and write. Whether a conditional UPDATE silently succeeds matching zero rows and execution proceeds as if the update landed
- Lock ordering — whether acquisition order depends on the ordering or grouping of the input. When a bulk operation groups by kind, ordering that flips depending on the request can deadlock
- Transaction boundaries — whether the boundary sits where existing conventions put it. Whether nesting has nullified an inner isolation-level setting. When a boundary is split, or an external side effect is moved outside it, what users see when only one side succeeds
- Caching — whether some ordering of invalidation and read lets a pre-update value be returned. This includes client-side caches
