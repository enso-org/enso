# Standard.Base

The foundational Enso standard library: core data types, I/O, networking, error
handling, and runtime utilities. Every other standard-library module imports
from `Standard.Base`. Most types here are imported automatically by
`from Standard.Base import all`.

## Main entry points

**Data types:** `Vector`, `Array`, `List`, `Map`, `Dictionary`, `Hashset`,
`Pair`, `Text`, `Numbers` (`Integer`, `Float`, `Decimal`), `Boolean`, `Nothing`,
`Date`, `Time_Of_Day`, `Date_Time`, `Duration`, `Range`, `Interval`.

**Collections & utilities:** `Ordering`, `Sort_Direction`, `Index_Sub_Range`,
`Locale`, `Encoding`, `Regex`.

**JSON & serialization:** `Json`, `JS_Object`, `parse_json`, XML.

**I/O & file system:** `File`, `File_Format` (`JSON`, `Delimited`/CSV,
`Plain_Text`, …), `System.File`, `System.Process`, `System.Environment`.

**Networking:** `HTTP`, `URI`, `Email`, `Data.fetch`, `Data.post`.

**Error handling:** `Error`, `Panic`, common errors (`Missing_Argument`,
`Index_Out_Of_Bounds`, `Type_Error`, `Illegal_Argument`, …), `Problem_Behavior`.

**Runtime & reflection:** `Meta`, `Runtime`, `Random`, `Polyglot`, `Any`.

**Cloud:** `Enso_File`, `Enso_Secret`, `Enso_User`.

## Common usage

```
v = [1, 2, 3, 4, 5]
filtered = v.filter (x -> x > 2)
mapped = v.map (x -> x * 2)
total = v.fold 0 (+)

text = "foo, bar, baz"
parts = text.split ", "
trimmed = text.trim
ok = text.contains "bar"

table = Data.read "data.csv"

parsed = '[{"name":"Alice","age":30}]'.parse_json
first = parsed.first

result = "not a number".parse_integer.catch _-> 0
```

## Layout

- `src/Data/` — foundational types: `Vector`, `Array`, `Text`, `Numbers`,
  `Boolean`, `Date`/`Time`, `Map`, `Pair`, `Dictionary`, `Range`, `Interval`,
  `Json`, `XML`, `Regex`.
- `src/System/` — file I/O, paths, environment, processes, file formats.
- `src/Errors/` — error type definitions, `Problem_Behavior`.
- `src/Network/` — HTTP, URI, Email.
- `src/Enso_Cloud/` — `Enso_File`, `Enso_Secret`, `Enso_User`.
- `src/Runtime/` — runtime utilities, contexts, debugging.
- `src/Meta/` — runtime type introspection.

## Things to avoid in generated code

- `advanced: true` entities — these are escape hatches the IDE searcher hides.
  Use the regular API.
- Direct `polyglot java import` of internal classes — only the stdlib should
  reach across the polyglot boundary.

## Where to read more

- `src/Main.enso` — full public API list.
- `src/Data/Vector.enso` — vector methods (`map`, `filter`, `fold`, `zip`, …).
- `src/Data/Text.enso` — string operations.
- `src/Data/Numbers.enso` — numeric types and methods.
- `src/System/File.enso` — file operations.
- `test/Base_Tests/src/` — extensive working examples.
