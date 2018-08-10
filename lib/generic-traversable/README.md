# Generic traversable

`generic-traversable` is a Haskell library for writing generic traversal
functions. It is similar to `syb-with-class` and `traverse-with-class`, 
but has many improvements, including:

- It is possible to use this library to generate GTraversable instances from
  withing other TH splices.
- All low-level utilities are marked INLINE for better performance guarantees.


The library started as a fork from `traverse-with-class`.