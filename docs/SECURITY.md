---
layout: developer-doc
title: Security Policy
category: summary
tags: [summary, security, vulnerability, report]
order: 4
---

# Security Policy
This document outlines the security policy for Enso and its libraries.

> **If you believe that you have found a vulnerability in Enso or one of its
> libraries, please see the section on 
> [reporting a vulnerability](#reporting-a-vulnerability) below.**

<!-- MarkdownTOC levels="2" autolink="true" -->

- [Supported Versions](#supported-versions)
- [Reporting a Vulnerability](#reporting-a-vulnerability)

<!-- /MarkdownTOC -->

## Supported Versions
Security updates for Enso are provided for the versions shown below with a
:white_check_mark: next to them. No other versions have security updates
provided.

| Version       | Supported          |
|---------------|--------------------|
| `master@HEAD` | :white_check_mark: |
| `wip/*`       | :x:                |

## Reporting a Vulnerability
If you believe that you've found a security vulnerability in the Enso codebase
or one of the libraries maintained in this repository, please contact
[security@enso.org](mailto:security@enso.org) and provide details of the bug.

You can expect an update on a reported vulnerability within one business day,
and the timeline works as follows:

1. We analyse your report to determine the risk posed by the vulnerability, and
   our further steps forward. This may involve asking for more information.
2. We will email the submitter with our verdict as to whether it is, or isn't a
   vulnerability, as well as the severity if it is.
3. We plan and outline any steps necessary to fixing the bug, including the
   timeline for fixing the vulnerability within 90 days.
4. We will communicate the planned fix with the person who submitted the
   vulnerability report.
5. We will fix the bug and communicate with the submitter when the fix has
   landed on `master`, and when it has been backported to the above supported
   versions.
6. The submitted may then disclose the bug publicly.

All communication will take place via email with a member of our team.
