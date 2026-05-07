---
name: project-structure
description:
  Mandatory practices for reading, understanding, and maintaining project
  documentation (CLAUDE.md, ARCHITECTURE.md). This skill MUST be used for ALL
  coding tasks, ALL conversations, ALL projects, regardless of language or
  context. It defines non-negotiable rules for how to orient yourself in any
  codebase before doing any work. Trigger on every single interaction — any file
  edit, code review, bug fix, feature addition, refactoring, question about
  code, or any task that involves a project directory. There are no exceptions.
  Even if the user asks a simple question, these rules apply.
---

## Reading Project Documentation

- Before doing ANY work in a project, read all CLAUDE.md files in the hierarchy
  — from the current working directory up through every parent directory to the
  project root. These files tell you where you are, what the project is, and how
  to work on the code. They contain critical guidance, conventions, and rules
  specific to that project and its modules.
- NEVER skip reading these files. No matter what. No matter how small the task
  seems. No matter if you think you already know the answer. Read them every
  time.

### ARCHITECTURE.md

- If there is an ARCHITECTURE.md file next to the closest (most specific)
  CLAUDE.md, always read it too. It contains system design, module boundaries,
  data flows, and key decisions that you need to understand.
- You don't need to read ARCHITECTURE.md files next to parent CLAUDE.md files
  unless you feel the task requires that broader architectural context.

## Maintaining Project Documentation

- Update CLAUDE.md IMMEDIATELY when you learn new things about the project.
  NEVER ask for permission — just do it. This includes things you learn from:
  - Reading code and discovering patterns or conventions
  - User corrections or preferences during conversation
  - Build/test commands and workflows
  - Module responsibilities and boundaries
  - Gotchas, edge cases, or non-obvious behaviors
- Apply common sense about where to put what:
  - If the learning is specific to a library or crate, update or create the
    CLAUDE.md at that library/crate root.
  - If the learning is generic and applies to the whole project, update the root
    CLAUDE.md.

### Ensuring CLAUDE.md Coverage

- Every Cargo.toml and package.json must have a sibling CLAUDE.md. If one
  doesn't exist, create it. It's fine to start with an empty file and fill it as
  you learn about that package. This applies to every crate and package in a
  workspace — the root and every member.
