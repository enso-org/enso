---
layout: developer-doc
title: Commit Messages
category: development
tags: [development, commit, git]
order: 1
---

# Commit Messages

Commit messages are an incredibly important part of development, as they provide
a summary of the changes that are included in a commit. This is instrumental in
producing a clear history for a given project.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Formatting Your Commit Messages](#formatting-your-commit-messages)
  - [Summary Line](#summary-line)
  - [Detailed Explanation](#detailed-explanation)
  - [Issue Number](#issue-number)
  - [General Formatting](#general-formatting)
- [Where these Guidelines Apply](#where-these-guidelines-apply)

<!-- /MarkdownTOC -->

## Formatting Your Commit Messages

Your commit messages should be formatted as follows:

```plain
[Type] A brief summary line

1.  A numbered list.
2.  That describes the changes made.

Related #issue-no
```

All elements are mandatory for all commits. Please note that the blank line
between the summary and the numbered list, and the line between the numbered
list and the `Resolves` are crucial to prevent certain tools from getting
confused.

An example of a good commit message is as follows:

```plain
[Fix, Style] Fix a crash in the frobnicator

1.  This commit fixes a bug in the method resolver that had arisen due
    to an issue with the frobnicator. Tweaking the algorithm used by
    the frobnicator from super-fancy-algo to even-fancier-algo resolved
    the issue.
2.  It also fixes a minor style issue with the naming of builder
    methods for the Truffle nodes.

Resolves #211
Resolves #283
```

### Summary Line

The summary line is a short descriptor of the commit. It must meet the following
requirements:

1.  It is written as an imperative "Create a new API for
    $foo", not "Creates a new API for $foo".
2.  It is less than 50 characters long.
3.  It begins with a capital letter.
4.  It _does not_ end with a full stop (`.`).
5.  It should specify the commit type(s) from the list of
    [commit types](#commit-types) in square brackets (`[]`) at the start.

For example:

```plain
[Tool] Bump scalafmt to 2.6.2
```

#### Commit Types

The following commit types are allowable in commit messages:

- `Build`: Related to the project build system.
- `Doc`: Changes to documentation.
- `Feat`: A new feature.
- `Fix`: A fix to a bug.
- `Ref`: A refactor.
- `Style`: Changes related to code style in code otherwise untouched.
- `Test`: Related to testing _only_. Do not use this tag if another fits.
- `Tool`: Related to tooling and CI.

### Detailed Explanation

This section contains more detailed explanatory text, expanding upon the changes
that have taken place as part of the commit. It must meet the following
requirements:

1.  Each numbered point deals with a different aspect of the commit.
2.  The numbered point should explain what was done and why, containing
    pertinent details where necessary.
3.  They should be written in full sentences, in grammatically correct English.
4.  They should be wrapped at 72 characters.
5.  Do not assume that the person reading the commit message understands the
    nature of the problem. Make sure to explain it.

For example:

```plain
1.  This commit fixes a bug in the method resolver that had arisen due
    to an issue with the frobnicator. Tweaking the algorithm used by
    the frobnicator from super-fancy-algo to even-fancier-algo resolved
    the issue.
2.  It also fixes a minor style issue with the naming of builder
    methods for the Truffle nodes.
```

### Issue Number

If the commit relates to an issue, please put the issue numbers at the bottom.
You may use more than one of the following if there are multiple issue numbers:

- `Resolves`: For when the commit resolves the issue in question.
- `Related`: For when the commit is related to an open issue, but doesn't
  completely resolve it.

For example:

```plain
Resolves #232
Related #271
```

### General Formatting

Your commit messages should obey the following formatting guidelines:

- There should be no trailing whitespace in the commit message.
- All unnecessary punctuation should be removed.
- Use markdown syntax for formatting in the body, but not in the subject line.

## Where these Guidelines Apply

These guidelines apply to _all branches_ in the Enso repository, except:

- Those beginning with `wip/*`.
