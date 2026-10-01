# Security

Who can reach what, and what leaks?

- Authorization and tenant isolation — whether scope such as the owning organization is part of the fetch and update conditions. Whether knowing an identifier is enough to reach another tenant's rows
- Untrusted input — whether input reaches a query, shell command, file path, URL, or rendered HTML without parameterization or escaping
- Exposure — whether secrets, tokens, or personal data end up in logs, error messages, analytics events, or API responses that did not carry them before
