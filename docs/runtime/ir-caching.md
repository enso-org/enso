---
layout: developer-doc
title: IR Caching in the Enso Compiler
category: runtime
tags: [runtime, caching]
order: 10
---

# IR Caching in the Enso Compiler

One of the largest pain points for users of Enso at the moment is the fact that
it has to precompile the entire standard library on every project load. This is,
in essence, due to the fact that the current parser is abysmally slow, and
incredibly demanding. The obvious solution to improve this is to take the parser
out of the equation in its entirety, by serializing the parser's output.

To that end, we want to serialize the Enso IR to a format that can later be read
back in, bypassing the parser entirely. Furthermore, we can move the boundary at
which this serialization takes place to the end of the compiler pipeline,
thereby bypassing doing most of the compilation work, and further improving
startup performance.

<!-- MarkdownTOC levels="2,3" autolink="true" -->

- [Serializing the IR](#serializing-the-ir)
  - [Breaking Links](#breaking-links)
- [Storing the IR](#storing-the-ir)
  - [Metadata Format](#metadata-format)
  - [Portability and Versioning](#portability-and-versioning)
- [Loading the IR](#loading-the-ir)
  - [Integrity Checking](#integrity-checking)
  - [Error Handling](#error-handling)
  - [Imports](#imports)
- [Testing the Serialization](#testing-the-serialization)
- [Future Directions](#future-directions)

<!-- /MarkdownTOC -->

## Serializing the IR

Using classical Java Serialization turned out to be unsuitably slow. Rather than
switching to other serialization framework that does the same, but faster we
desided in [PR-8207](https://github.com/enso-org/enso/pull/8207) to create _own
persistance framework_ that radically changes the way we can read the caches.
Rather than loading all the megabytes of stored data, it reads them _lazily on
demand_.

Use following command to generate the Javadoc for the `org.enso.persist`
package:

```bash
enso$ find lib/java/persistance/src/main/java/ | grep java$ | xargs ~/bin/graalvm-21/bin/javadoc -d target/javadoc/ --snippet-path lib/java/persistance/src/test/java/
enso$ links target/javadoc/index.html
```

In order to maximize the benefits of this process, we want to serialize the IR
as _late_ in the compiler pipeline as possible. This means serializing it just
before the code generation step that generates Truffle nodes (before the
`RuntimeStubsGenerator` and `IrToTruffle` run).

This serialization should take place in an _offloaded thread_ so that it doesn't
block the compiler from continuing.

### Breaking Links

Doing this naïvely, however, means that we can inadvertently end up serializing
the entire module graph. This is due to the `BindingsMap`, which contains a
reference to the associated `runtime.Module`, from which there is a reference to
the `ModuleScope`. The `ModuleScope` may then reference other `runtime.Module`s
which all contain `IR.Module`s. Therefore, done in a silly fashion, we end up
serializing the entire reachable module graph. This is not what we want.

The `Persistance.write` method contains additional `writeReplace` function which
our cache system uses to perform following modification just before
`ProcessingPass.Metadata` are stored down:

- modify `BindingsMap` and its child types to be able to contain an unlinked
  module pointer `case class ModulePointer(qualifiedName: List[String])` in
  place of a `Module`.
- As the `MetadataStorage` type that holds the `BindingsMap` is mutable it might
  be tempting to update it in place, but relying on `writeReplace` mechanism is
  safer as it only changes the format of object being written down, rather than
  modifying objects of live `IR` - potentially shared with other parts of the
  system.

Having done this, we have broken any links that the IR may hold between modules,
and can serialize each module individually.

It _may_ be safer to `duplicate` the IR before handing it to serialization, but
it shouldn't be necessary if the `writeReplace` function is written correctly.

## Storing the IR

The serialized IR needs to be stored in a location that is tied to the library
that it serializes. Despite this, we _also_ want to be able to ship cached IR
with libraries. This leads to a two pronged solution where we check two
locations for the cache.

1. **With the Library:** As libraries can have a hidden `.enso` directory, we
   can use a path within that for caching. This should be
   `$package/.enso/cache/ir/enso-$version/`, and can be accessed by extending
   the `pkg` library to be aware of the cache directories.
2. **Globally:** As some library locations may not be writeable, we need to have
   a global out-of-line cache that is used if the first one is not writeable.
   This is located under `$ENSO_DATA` (whose location can be obtained from the
   `RuntimeDistributionManager`), and is located under the path
   `$ENSO_DATA/cache/ir/$hash/enso-$version/`, where `$hash` is the `SHA3-224`
   hash of the tuple `(namespace, library_name, version)`, where
   `version = SemVer | "local"`. This hash is computed by concatenating the
   string representations of these fields.

In each location, the IR is stored with the following assumptions:

- The IR file is located in a directory modelled after its module path, followed
  by a file named after the module itself with the extension `.ir` (e.g. the IR
  for `Standard.Base.Data.Vector` is stored in `Standard/Base/Data/Vector.ir`).
- The [metadata](#metadata-format) file is located in a directory modelled after
  its module path, followed by a file named after the module itself with the
  extension `.meta` (e.g. the metadata for `Standard.Base.Data.Vector.enso` is
  stored in `Standard/Base/Data/Vector.meta`). This is right next to the
  corresponding `.ir` file.

Storage of the IR only takes place iff the intended location for that IR is
_empty_.

### Metadata Format

The metadata is used for integrity checking of the cached IR to prevent loading
corrupted or out of date data from the cache. Due to the fact that engines can
only load IR created by their versions, and cached IR is located in a directory
named after the engine version, this format need not be forward compatible.

It is a JSON file as follows:

```typescript
{
  sourceHash: String; // The hash of the corresponding source file.
  blobHash: String; // The hash of the blob.
  compilationStage: String; // The compilation stage of the IR.
}
```

All hashes are encoded in SHA1 format, for performance reasons. The engine
version is encoded in the cache path, and hence does not need to be explicitly
specified in the metadata.

### Portability and Versioning

These are two static methods in `Persistance` class to help creating a `byte[]`
from a single object and then read it back. The array is identified with
following header:

- 4 bytes fixed header
- 4 bytes describing the version
- 4 bytes to locate the beginning of the object (the objects aren't written
  linearly)

E.g. 12 bytes overhead before the actual data start. Following versioning is
recommended when making a change:

- when you change something really core in the `Persitance` implementation -
  change the builtin header first four bytes
- when you add or remove a Persistance implementation the version changes (as it
  is computed from all the IDs present in the system)
- when you change format of some `Persitance.writeObject` method - change its ID

That way the same version of Enso will recognize its `.ir` files. Different
versions of Enso will realize that the files aren't in suitable form.

Every `Persistance` class has a unique identifier. In order to keep definitions
consistent one should not attempt to use smaller `id`s than previously assigned.
One should also not delete any `Persistance` classes.

## Loading the IR

Loading the IR is a multi-stage process that involves performing integrity
checking on the loaded cache. It works as follows.

1. **Find the Cache:** Look in the global cache directory under `$ENSO_DATA`. If
   there is no cached IR here that is valid for the current configuration, check
   the ibrary's `.enso/cache` folder. This should be hooked into in
   `Compiler::parseModule`.
2. **Check Integrity:** Check the module's [metadata](#metadata-format) for
   validity according to the [integrity rules](#integrity-checking).
3. **Load:** If the cache passes the integrity check, load the `.ir` file. If
   deserialization fails in any way, immediately fall back to parsing the source
   file.
4. **Re-Link:** Relinking is part of **Load**. When using `Persistance.read`
   provide own `readResolve` function. Such a function gets a chance to change
   and replace each object read-in with appropriate variant respecting the whole
   compiler environment.

The main subtlety here is handling the dependencies between modules. We need to
ensure that, when loading multiple cached libraries, we properly handle them
one-by-one. Doing this is as simple as hooking into `Compiler::parseModule` and
setting `AFTER_STATIC_PASSES` as the compilation state after loading the module.
This will tie into the current `ImportsResolver` and `ExportsResolver` which are
run in an un-gated fashion in `Compiler::run`.

Unlike classical Java deserialization nly registered `Persistance` subclasses
may participate in deserialization making it much safer and less vulnerable.

### Integrity Checking

For a cache to be usable, the following properties need to be satisfied:

1. The `sourceHash` must match the hash of the corresponding source file.
2. The `blobHash` must match the hash of the corresponding `.ir` file.

If any of these fail, the cache file should be deleted where possible, or
ignored if it is in a read-only location.

### Error Handling

It is important, as part of this, that we fail under all circumstances into a
working state. This means that:

- If serialization fails, we report a low-priority error message and continue.
- If deserialization fails, we fall back to loading and parsing the original
  source file.

At no point should this mechanism be exposed to the user in any visible way,
other than the fact that they may be seeing the actual files on disk.

### Imports

Integrity Checking does not check the situation when the cached module imports a
module which cache has been invalidated. For example, module `A` uses a method
`foo` from module `B` and a successful compilation resulted in IR cache for both
`A` and `B`. Later, someone modified module `B` by renaming method `foo` to
`bar`. If we only compared source hashes, `B`'s IR would be re-generated while
`A`'s would be loaded from cache, thus failing to notice method name change,
until a complete cache invalidation was forced.

Therefore, the compiler performs an additional check by invalidating module's
cache if any of its imported modules have been invalidated.

## Testing the Serialization

There are two main elements that need to be tested as part of this feature.

- `persistance` project comes with its own unit tests
- `runtime-parser` project adds tests of various core classes used during `IR`
  serialization - like Scala `List` or checks of the _laziness_ of Scala `Seq`
- We need to test the serialization and deserialization process, including the
  rewrite of `BindingsMap` to work properly.
- We also need to test the discovery of cache locations on the filesystem and
  cache eviction strategies. The best way to do this is to set `$ENSO_DATA` to a
  temporary directory and then directly interact with the filesystem. Caching
  should be disabled for existing tests. This will require adding additional
  runtime options for debugging, but also constructing the `DistributionManager`
  on context creation (removing `RuntimeDistributionManager`).

### Import/Export caching of bindings

Import and export resolution is one of the more expensive elements in the
initial pipeline. It is also the element which does not change for the releases
library components as we do not expect users to modify them. During the initial
compilation stage we iteratively parse/load cached ir, do import resolution on
the module, followed by export resolution, and repeat the process with any
dependent modules discovered in the process. Calculating such transitive closure
is an expensive and repeatable process. By caching bindings per library we are
able to skip that process completely and discover all necessary modules of the
library in a single pass.

The bindings are serialized along with the library caches in a file with a
`.bindings` suffix.

Further more the storage of `.ir` files contains usage of _lazy_ `Seq`
references to separate the general part of the `IR` tree from elements
representing method bodies. As such the compiler can process the structure of
`.ir` files, but avoid loading in `IR` for methods that aren't being executed.

## Future Directions

The `Persistance` framework gives us _laziness_ opportunities and we should use
them more:

- have a single _blob_ with all `IR`s per a library and read only the parts that
  are needed

- experiement with GC - being able to release parts of unused `IR` once they
  were used (for code generation or co.)

- make the `.ir` files smaller where possible

The use of `Persistance` has already sped up the execution time of simple
`IO.println "Hello!"` by 16% - let's use it to speed things up even more.
