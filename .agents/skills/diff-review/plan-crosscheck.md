# Plan cross-check (Step 4) classification reference

For each finding in the blind-review `review.json`, cross-check it against the plan,
apply one of the classifications below, and add a `plan_note` to the finding. If
there is no plan, skip this step entirely, set `plan_checked: false` /
`plan_path: null`, and pass findings through unchanged.

`severity` has three levels, `critical` > `warning` > `info` (`info` is the floor).
The five classifications:

- unrelated: an implementation concern unrelated to the plan. Keep it as-is. Do not
  add a `plan_note`
- resolved: the plan makes the concern go away (it is intended / expected and needs
  no action). If it is worth surfacing as known behavior, keep it and lower
  `severity` by at least one level (if already `info`, leave it at `info`) and write
  the reason in `plan_note`. If it leaves no trace in the implementation and has no
  value to surface, drop the finding (when in doubt, keep it)
- plan_sanctioned_but_flagged: the plan explicitly says it is intentional, but the
  implementation quality still has room to improve and the action is worth keeping.
  Leave `severity` unchanged and write in `plan_note` that "the plan intends this,
  but improvement is requested as an implementation." Boundary vs resolved: if no
  action is needed use resolved; if an action should remain, use this
- plan_confirmed: the plan backs up the finding, showing it is a definite spec
  violation (e.g. the implementation fails to do something the plan marks as MUST).
  Leave `severity` unchanged (raise it if confidence increases). Write the plan's
  supporting basis in `plan_note`
- new addition: a discrepancy first noticed by cross-checking the plan against the
  diff (e.g. the plan requires something the implementation lacks). Create a new
  finding, tag it `source: "plan_crosscheck"`, and place it inside some group's
  `findings[]`. Set `severity` by the real harm of the discrepancy (a failure to
  meet a plan MUST / required behavior is `critical`~`warning`; a minor deviation is
  `info`)

Finally add `plan_checked: true` and `plan_path` to review.json.
