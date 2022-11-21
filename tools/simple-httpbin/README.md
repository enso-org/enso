# Simple HTTPBin

A simple HTTP Request/Response clone of [httpbin](http://httpbin.org) for
testing purposes.

## Usage

Simple HTTPBin can be compiled like any other SBT project i.e.

```
sbt> simple-httpbin/compile
```

To run, simply invoke the `main` method with the appropriate hostname and port:

```
sbt> simple-httpbin/run localhost 8080
```
