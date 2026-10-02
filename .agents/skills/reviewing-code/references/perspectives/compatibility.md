# Compatibility and rollout

What breaks for existing callers, existing data, or running code while this change is being deployed?

- Backward compatibility — changes to public interfaces, schema changes, impact on existing callers
- Old/new dual-path migration — whether an entry point that ignores the switch flag remains. Whether the old path can still be written to after the switch. How data created before the switch is handled (and if it is not migrated, whether that errors)
- Existing data under new rules — when a change adds or tightens a validation or invariant, or routes an existing operation through code that re-validates every field (a constructor, a factory, a decode step), whether rows already stored can violate it. If they can: whether reading them now fails, whether an unrelated edit can no longer be saved, whether re-saving drops elements in the old shape or triggers side effects retroactively, and whether cancel, delete, or discard paths can still move data out of the bad state. Whether the author checked how many existing rows violate the rule, and chose explicitly between validating when loading from storage and validating only on write; if only on write, whether new instances can still be created without passing that validation
- Schema change rollout order — the effect of a new column's default on existing rows. Whether running work breaks during the window where a migrated database coexists with un-updated old code
- Operational accidents — whether operations that are dangerous to run against production have a safety catch
