# Compatibility and rollout

What breaks for existing callers, existing data, or running code while this change is being deployed?

- Backward compatibility — changes to public interfaces, schema changes, impact on existing callers
- Old/new dual-path migration — whether an entry point that ignores the switch flag remains. Whether the old path can still be written to after the switch. How data created before the switch is handled (and if it is not migrated, whether that errors)
- Schema change rollout order — the effect of a new column's default on existing rows. Whether running work breaks during the window where a migrated database coexists with un-updated old code
- Operational accidents — whether operations that are dangerous to run against production have a safety catch
