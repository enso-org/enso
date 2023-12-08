# Data structures

This folder contains:

- classes defining data structures
  - `vec2.ts` defines a `Vec2` type representing an arbitrary two-dimensional quantity
    - this includes both coordinates, and 2D sizes
  - `rect.ts` defines a `Rect` type representing a rectangle with both a position, and a size
- branded types (newtypes) for specific data types
  - `urlString.ts` defines URL strings and a type predicate for producing URL strings
- utility functions for data structures
  - `array.ts` contains functions for the `Array<T>` type
  - `iterable.ts` contains functions for the `Iterable<T>` type
  - `observable.ts` contains functions for the `ObservableV2<Events>` type in `lib0`
