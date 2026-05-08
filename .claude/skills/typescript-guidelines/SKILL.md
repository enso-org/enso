---
name: typescript-guidelines
description: Typescript coding guidelines and conventions. Use this skill whenever working on Typescript source files (.ts or .tsx) including Vue framework (.vue) — editing, creating, reviewing, or refactoring Typescript code. This includes any task involving Typescript: writing new modules, fixing bugs, adding features, code review, or answering questions about Typescript code in the project.
---

## General rules

- Do not add typecasts if not necessary. First make sure the cast is needed for
  proper typechecking. Then consider adding assertions or guards ensuring the
  value's type is correct.
- The type aliases should be used only when the type is really complex (multiple
  nested generic arguments, for example), or if the aliast name would provide
  useful information besides the variable's/parameter's name.

## Documentation Comments

- The docstrings should be created for exported, or non-obvious private
  entities.
- They goal is to understand what given entity _is_. Do not focus on how it is
  used, unless it is useful as an example to understand the entity itself. Do
  not mention details which are easily deduced from the name or code itself.
- They should not focus on implementation details, unless it may be somehow
  important to the dev using the entity (like time complexity).
- @file documentation should be a very short summary of module. The more
  detailed description should be on the main function/class defined in module.
