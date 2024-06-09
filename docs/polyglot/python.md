---
layout: developer-doc
title: Polyglot Python
category: polyglot
tags: [polyglot, python]
order: 4
---

# Polyglot Python

This document provides practical example showing polyglot interoperability with
Python in the runtime. Please familiarise yourself with the general operation of
[polyglot bindings](./polyglot-bindings.md).

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Install Graal Python standalone distribution](#install-graal-python-standalone-distribution)
- [Polyglot Library System](#polyglot-library-system)
- [Using Python Libraries](#using-python-libraries)

<!-- /MarkdownTOC -->

## Install Graal Python standalone distribution

Before continuing this tutorial, you need to install Graal Python standalone
distribution. Follow the instructions from
https://www.graalvm.org/latest/reference-manual/python/#installing-graalpy. Note
that for the basic Python usage, it is not necessary to download the GraalPy
standalone distribution, we just need it for this tutorial because we initialize
the _virtual environment_ and install `numpy` package with it.

In the rest of the document, `graalpy` points to the `bin/graalpy` binary from
the downloaded Graal Python standalone distribution.

## Polyglot Library System

There is a support for using any Python library from Enso. Steps to include
`numpy` in a new Enso project follows:

```bash
$ enso-engine*/bin/enso --new numenso
$ find numenso/
numenso/
numenso/src
numenso/src/Main.enso
numenso/package.yaml
$ mkdir numenso/polyglot
$ graalpy -m venv numenso/polyglot/python
$ ./numenso/polyglot/python/bin/graalpy -m pip install numpy
Successfully installed numpy-1.23.5
```

The above steps instruct Enso to create a new project in `numenso` directory.
Then they create Python virtual environment in `numenso/polyglot/python/` dir -
e.g. in the
[standard location for polyglot](../distribution/packaging.md#the-polyglot-directory)
components of an Enso project. As a last step we activate the virtual
environment and use `pip` manager to install `numpy` library.

## Using Python Libraries

As soon as a library is installed into the
[polyglot directory](#polyglot-library-system) it can be used via the
[embedded syntax](polyglot-bindings.md#embedded-syntax):

```ruby
foreign python random_array s = """
    import numpy
    return numpy.random.normal(size=s)

main = random_array 10
```

Let's modify the `numenso/src/Main.enso` to use `numpy.random.normal` as shown
above. Then we can simply execute the project and obtain a `numpy` array as a
result:

```bash
$ enso-engine*/bin/enso --run numenso
array([-0.51884419, -0.23670113, -1.20493508, -0.86008709,  0.59403118,
       -0.171484  , -1.19455596, -0.30096434, -0.69762239, -0.11411331])
```

The same steps can be applied to any Graal Python supported library.
