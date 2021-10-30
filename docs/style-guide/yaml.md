---
layout: style-guide
title: YAML Style Guide
category: style-guide
tags: [style-guide]
order: 6
---

# YAML Style Guide

In order to make our YAML configuration easy to read, we use an automated
formatter to ensure that it's up to scratch.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Automated Formatting](#automated-formatting)

<!-- /MarkdownTOC -->

## Automated Formatting

The bulk of the heavy lifting for formatting our YAML is done by the formatter
[prettier](https://prettier.io). This formatter should be run on the entire
repository using `npx prettier --write .` before every pull request.

For instructions on how to install it, please see our
[contributing guidelines](../CONTRIBUTING.md#getting-set-up-documentation).

If you notice files in generated code being formatted by prettier, please add
them to the [`.prettierignore`](../../.prettierignore) file.

When bumping the version of prettier, please commit the resultant configuration
formatting changes along with the bump as a separate PR from any functional code
changes.
