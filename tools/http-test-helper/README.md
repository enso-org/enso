# HTTP Test Helper

A simple HTTP Request/Response clone of [httpbin](http://httpbin.org) for
testing purposes, extended with additional functionality allowing for testing
Enso Cloud features.

## Usage

It can be compiled like any other SBT project i.e.

```
sbt> http-test-helper/compile
```

To run, simply invoke the `main` method with the appropriate hostname and port:

```
sbt> http-test-helper/run localhost 8080
```
