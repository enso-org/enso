---
layout: developer-doc
title: RFC Template
category: rfcs
tags: [rfcs, specification]
order: 1 # Should match the RFC Number
---

> - **Feature Name:** A unique name for the feature
> - **Start Date:** Today's date (YYYY-MM-DD)
> - **Change Type:** Breaking / Non-Breaking (delete as appropriate)
> - **RFC Dependencies:** List any RFCs that this one depends on.
> - **RFC PR:** Leave Empty
> - **Enso Issue:** Leave Empty
> - **Implemented:** Leave blank, this will be filled with the first version of
>   the relevant tool where it is implemented.

# Summary

A one-paragraph, high-level summary of the proposal.

# Motivation

Why should we make this change? What use-cases does it support? What benefits
does it bring to the ecosystem? Explain why the status quo is insufficient or
not ideal.

# Guide-Level Explanation

Explain the proposal as if teaching the feature(s) to a newcomer to Enso. This
should usually include:

- Introduction of any new named concepts.
- Explaining the feature using motivating examples.
- Explaining how Enso programmers should _think_ about the feature, and how it
  should impact the way they use the language. This should aim to make the
  impact of the feature as concrete as possible.
- If applicable, provide sample error messages, deprecation warnings, migration
  guidance, etc.
- If applicable, describe the differences in teaching this to new Enso
  programmers and experienced Enso programmers.

For implementation-oriented RFCs, this section should focus on how contributors
to the project should think about the change, and give examples of its concrete
impact. For policy-level RFCs, this section should provide an example-driven
introduction to the policy, and explain its impact in concrete terms.

# Specification-Level Explanation

This is the technical portion of the RFC and should be written as a
specification of the new feature(s). It should provide a syntax-agnostic
description of the planned feature, and include sufficient detail to address the
following points:

- Impact on existing feature(s) or functions.
- Interactions with other features.
- A clear description of how it should be implemented, though this need not
  contain precise references to internals where relevant.
- An explanation of all corner cases for the feature, with examples.

This section should be written in comprehensive and concise language, and may
include where relevant:

- The grammar and semantics of any new constructs.
- The types and semantics of any new library interfaces.

# Drawbacks

A description of why we _should not_ do this. Write this section as if you are
picking apart your proposal.

# Rationale and Alternatives

A few paragraphs addressing the rationale for why this design is the best
possible design for this feature in its design space. It should address how the
proposed design resolves the motivating factors.

A few paragraphs addressing alternative designs for the feature, and the reasons
for not choosing them.

A paragraph or two addressing the impact of not including this feature.

# Unresolved Questions

This section should address any unresolved questions you have with the RFC at
the current time. Some examples include:

- What parts of the design will require further elaboration before the RFC is
  complete?
- Which portions of the design will need resolution during the implementation
  process before the feature is made stable.
- Are there any issues related to this RFC but outside its scope that could be
  addressed in future independently of this RFC? If so, what are they?
