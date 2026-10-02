# Behavior

Does the changed code do the right thing for every input and state it can meet?

- Correctness — logic errors, null dereferences, inverted conditions, off-by-one
- Edge cases and failure paths — empty collections, division by zero, negative values, unhandled branches, error handling
- Functional impact — whether the change contradicts existing use cases, state transitions, or configuration
- Silent fallbacks — whether `?: default`, catch-to-null, or return-empty-on-error turns out-of-spec data into a plausible value. Each fallback value needs a specific justification, not "better than crashing"
- Swallowed exceptions — whether the swallowing site retains what a later investigation needs (the exception, the target identifier). Whether the error reaches the user, or the screen just goes blank without telling them
- Intent — whether the code does what the stated intent says it does, completely, and nothing outside it. An unmentioned change belongs in another change or in the description
- Spec gaps — whether an automated process can later overwrite a manual operation. Whether a delete or undo is needed to match a create or update. Whether a no-op guard is needed in certain states
