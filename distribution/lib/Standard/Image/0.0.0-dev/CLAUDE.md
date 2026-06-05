# Standard.Image

Image manipulation backed by an OpenCV polyglot import. Read/write image files
in common formats; arithmetic on images and matrices; histograms.

This is a small, real Enso library — a good reference for idiomatic Enso syntax
(constructor patterns, doc blocks, polyglot Java interop, conversion methods,
`private: true`).

## Main entry points

- `Image.read location flags=[]` — load an image from a file (PNG, JPEG, …).
  `flags` accepts a single `Read_Flag` or a vector.
- `Image.from_vector values rows=1 channels=1` — construct an image from
  normalized `[0.0..1.0]` pixel values.
- `image.write path flags=[]` — save an image, controlled by `Write_Flag`s
  (`..PNG_Compression n`, `..JPEG_Quality n`, `..JPEG_Progressive`, …).
- `image.rows`, `.columns`, `.channels` — dimensions.
- `image.get row column` — pixel at coordinate.
- `image + value`, `-`, `*`, `/` — element-wise arithmetic against a number,
  vector, or `Matrix` of equal dimensions.
- `image.to_vector`, `image.to_matrix` — conversions.
- `image.histogram channel` — per-channel `Histogram`.
- `Matrix.zeros`, `Matrix.ones`, `Matrix.from_vector` — matrix constructors.

## Common usage

```
img = Image.read "photo.png"
gray = Image.read "photo.png" Read_Flag.Grayscale
alpha = Image.read "photo.png" Read_Flag.Alpha_Channel

brightened = img + 0.1

img.write "out.png" [Write_Flag.PNG_Compression 9]
img.write "out.jpg" [Write_Flag.JPEG_Quality 80, Write_Flag.JPEG_Progressive]

mat = Matrix.zeros rows=img.rows columns=img.columns channels=img.channels
diff = img - mat
```

## Layout

- `src/Image.enso` — `Image` type, arithmetic operators, conversions.
- `src/Matrix.enso` — `Matrix` type with `zeros`, `ones`, `identity`,
  `from_vector`, arithmetic operators.
- `src/Matrix_Error.enso` — `Matrix_Error` type (e.g. `Dimensions_Not_Equal`,
  `Index_Out_Of_Bounds`).
- `src/Histogram.enso` — `Histogram` type returned by `image.histogram`.
- `src/Read_Flag.enso`, `src/Write_Flag.enso` — flag enums for read/write.
- `src/Image_File_Format.enso` — file format SPI for `Data.read` / `Data.write`.
- `src/Image_Services.enso` — internal SPI implementation.

## Things to avoid in generated code

- Internal helper functions in `src/Image.enso` marked `private: true`
  (`core_op`, `core_op_handler`, `Image_Comparator`, `Image.Value` constructor —
  use `Image.read` / `Image.from_vector` instead).
- Direct use of `polyglot java import org.opencv.…` types — these are marshalled
  through the public API, not exposed as Enso types.
- Mutating an `Image` in place — operators return new `Image` values; the
  underlying OpenCV `Mat` is not safe to mutate from Enso.
- Mismatched dimensions in arithmetic against a `Matrix`: triggers
  `Matrix_Error.Dimensions_Not_Equal`.

## Where to read more

- `src/Image.enso` — every method has a `## ---` doc block with arguments and
  runnable `## Examples` snippets.
- `src/Matrix.enso` — matrix operators and conversions.
- `test/Image_Tests/src/` — read/write/arithmetic test specs.
