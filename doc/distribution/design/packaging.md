# Library Packaging Design
This document deals with the design for the packaging of libraries in Enso. We
want to strike a balance between providing an excellent user experience and
allowing power users to get the most out of the platform.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Build Reproducibility](#build-reproducibility)

<!-- /MarkdownTOC -->

## Build Reproducibility
It is crucial for any good development environment to provide reproducible
builds, such that it is impossible for it to go wrong by mismatching library
versions.

> The actionables for this section are:
> - Decide on the strategies of ensuring consistent library resolution. This
>   may include hashing the downloaded versions of libraries and publishing
>   stack-style resolvers for sets of libraries that are proven to work well
>   together.
