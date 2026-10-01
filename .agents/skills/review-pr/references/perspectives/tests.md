# Tests

Would the tests catch this change breaking, and do the added tests earn their place?

- Test sufficiency — whether the added or changed behavior has tests, and whether branches and failure paths are covered
- Whether the tests actually verify something — whether expected and actual collapse onto the same constant so the test always passes, and whether the test actually exercises the branch this PR changed
- Whether each added test is meaningful — ask whether it would fail if this PR's change were reverted or the bug it fixes came back. A test that passes either way protects nothing. Typical shapes:
  - Assertions too weak to tell right from wrong: only "not null", "no exception", or a length, where the value is what matters
  - Verifying the implementation instead of the behavior: asserting that a mock was called with certain arguments while never checking the observable result, so a correct refactor breaks it and a wrong result does not
  - Mocking the unit under test, or the collaborator whose behavior is the point of the change
  - Restating existing coverage: the same path and the same assertions as a test that already exists
  - A name or description that promises a case the body does not exercise

A missing or meaningless test is a Blocker only when you can name the defect in this PR that it lets through. Otherwise it is a Follow-up.
