---
name: Bug Report
about: Report a bug in Enso.
title: ''
labels: 'Type: Bug'
assignees: ''

---

<!--
Please ensure that you are running the latest version of Enso before reporting 
the bug! It may have been fixed since.
-->

### General Summary
<!--
- Please include a high-level description of your bug here.
-->

### Steps to Reproduce
<!--
Please list the reproduction steps for your bug. For example:

1. Launch the enso interpreter in server mode `enso --server --socket:8080`.
2. Send it a message as follows, where `path/to/project` doesn't exist.

```json
{
    message-type: "load-project",
    load-project: {
        path: "path/to/project"
    }
    ...
}
```
3. Observe that the compiler crashes.
-->

### Expected Result
<!--
- A description of the results you expected from the reproduction steps.
-->

### Actual Result
<!--
- A description of what actually happens when you run these steps.
- Please include any error output if relevant.
-->

### Luna Version
<!--
- Please include the output of `enso --version`.
-->
