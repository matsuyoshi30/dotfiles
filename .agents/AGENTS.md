## Code Quality

- Add clear comments explaining complicated business logic
- Generate comprehensive documentation
  - Write WHY not WHAT
- Create examples in documentation
- Auto-fix all linting/formatting issues
- Write tests for new features and bug fixes
  - Don't test trivial functions like just call other utilities

## Hypothesis Testing

- Test only one hypothesis per change
- Explain the hypothesis and change details beforehand
- Validate the hypothesis through testing
- Revert the change if testing shows no effect

## Agent Guidelines

- Always prefer simplicity over pathological correctness
- YAGNI, KISS, DRY
- No backward compatibility shims or fallback paths unless they come free without adding cyclomatic complexity
