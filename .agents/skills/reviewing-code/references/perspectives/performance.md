# Performance

Does the change stay fast and bounded at production data volumes and traffic?

A performance finding needs a scale in its breaking steps: how many rows, items, requests, or renders it takes before the cost shows, and why production reaches that scale. Without one it is Unverified, not a finding.

- Fetch scope — whether everything is fetched and then filtered or counted in application code. Whether a query is issued inside a loop. Whether the same data is fetched twice through different paths
- Query shape — whether a new filter, join, or sort can use an existing index, or will scan a table that grows. Whether a list endpoint is unbounded where siblings paginate
- Growth — whether computation or memory grows faster than the input on a hot path: nested loops over collections, repeated linear lookups, whole result sets held in memory
- Blocking work — whether synchronous I/O, external calls, or heavy computation now sit on a request path or the UI thread, and whether independent calls run in series when they could run in parallel
- Rendering — whether a change causes repeated re-renders, recomputation on every render, or large lists rendered without virtualization, and whether it pulls a heavy dependency into the initial bundle
