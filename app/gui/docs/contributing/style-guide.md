---
layout: style-guide title: Rust Style Guide category: style-guide tags: [style-guide,contributing]
---

# Rust Style Guide

We are using `rustfmt` for the basic formatting of our rust code, which is also
checked on CI. We have some additional guidelines for imports, naming, sections
within files and naming which are documented here.

## Styling rules

### Imports

Imports should be divided into 4 groups separated by blank lines. Items in the
groups should be sorted alphabetically.

```rust
// Group 1: sub-module definitions.
// Group 2: prelude-like imports.
// Group 3: local-crate imports.
// Group 4: external imports.
```

For example:

```rust
pub mod display_object;

use crate::prelude::*;

use crate::closure;
use crate::data::opt_vec::OptVec;
use crate::data::dirty;
use crate::system::web::group;

use nalgebra::Matrix4;
use nalgebra::Vector3;
```

### Sections

Source files should be divided into sections. Section headers should be placed
before each new " concept" defined in a file. By "concept" we normally mean a
structure with related implementations. In case related implementations use some
helper structs with very small implementations, these helper structs may be
defined in the same section. Moreover, the code in each section should be
divided into sub-sections, grouping related definitions. At least one section
should be defined in a file (if there is at least one struct definition as
well). For example:

```rust
// =================
// === AxisOrder ===
// =================

/// Defines the order in which particular axis coordinates are processed. Used
/// for example to define the rotation order in `DisplayObject`.
pub enum AxisOrder { XYZ, XZY, YXZ, YZX, ZXY, ZYX }

impl Default for AxisOrder {
    fn default() -> Self { Self::XYZ }
}


// =================
// === Transform ===
// =================

/// Defines the order in which transformations (scale, rotate, translate) are
/// applied to a particular object.
pub enum TransformOrder {
    ScaleRotateTranslate,
    ScaleTranslateRotate,
    RotateScaleTranslate,
    RotateTranslateScale,
    TranslateRotateScale,
    TranslateScaleRotate
}

impl Default for TransformOrder {
    fn default() -> Self { Self::ScaleRotateTranslate }
}


// =============================
// === HierarchicalTransform ===
// =============================

pub struct HierarchicalTransform<OnChange> {
    transform: Transform,
    transform_matrix: Matrix4<f32>,
    origin: Matrix4<f32>,
    matrix: Matrix4<f32>,
    pub dirty: dirty::SharedBool<OnChange>,
    pub logger: Logger,
}

impl<OnChange> HierarchicalTransform<OnChange> {
    pub fn new(logger: Logger, on_change: OnChange) -> Self {
        let logger_dirty = logger.sub("dirty");
        let transform = default();
        let transform_matrix = Matrix4::identity();
        let origin = Matrix4::identity();
        let matrix = Matrix4::identity();
        let dirty = dirty::SharedBool::new(logger_dirty, on_change);
        Self { transform, transform_matrix, origin, matrix, dirty, logger }
    }
}


// === Getters ===

impl<OnChange> HierarchicalTransform<OnChange> {
    pub fn position(&self) -> &Vector3<f32> {
        &self.transform.position
    }

    pub fn rotation(&self) -> &Vector3<f32> {
        &self.transform.rotation
    }

    ...
}


// === Setters ===

impl<OnChange: Callback0> HierarchicalTransform<OnChange> {
    pub fn position_mut(&mut self) -> &mut Vector3<f32> {
        self.dirty.set();
        &mut self.transform.position
    }

    pub fn rotation_mut(&mut self) -> &mut Vector3<f32> {
        self.dirty.set();
        &mut self.transform.rotation
    }

    ...
}
```

### Multiline Expressions

Most (preferably all) expressions should be single line. Multiline expressions
are hard to read and introduce noise in the code. Often, it is also an indicator
of code that is not properly refactored. Try to refactor parts of multiline
expressions to well-named variables, and divide them to several single-line
expressions.

Example of poorly formatted code:

```rust
pub fn new() -> Self {
    let shape_dirty = ShapeDirty::new(logger.sub("shape_dirty"),
                                      on_dirty.clone());
    let dirty_flag = MeshRegistryDirty::new(logger.sub("mesh_registry_dirty"),
                                            on_dirty);
    Self { dirty_flag, dirty_flag }
}
```

Example of properly formatted code:

```rust
pub fn new() -> Self {
    let sub_logger = logger.sub("shape_dirty");
    let shape_dirty = ShapeDirty::new(sub_logger, on_dirty.clone());
    let sub_logger = logger.sub("mesh_registry_dirty");
    let dirty_flag = MeshRegistryDirty::new(sub_logger, on_dirty);
    Self { shape_dirty, dirty_flag }
}
```

### Getters and Setters

Getters do not have the `get_` prefix, while setters do. If a setter is provided
(method with the `set_` prefix), a `mut` accessor should be provided as well.
The correct way of defining getters and setters is presented below:

```rust
fn field(&self) -> &Type {
    &self.field
}

fn field_mut(&mut self) -> &mut Type {
    &mut self.field
}

fn set_field(&mut self, val: Type) {
    *self.field_mut = val;
}
```

### Trait exporting

All names should be designed to be used in a qualified fashion. However, this
makes one situation tricky. In order to use methods defined in a trait, it has
to be in scope. Consider a trait `display::Object`. We want to use it as
function bound like `fn test<T:display::Object>(t:T) {...}`, and we also want to
use methods defined in this trait (so it has to be in scope). In such a case,
`Clippy` warns that `display::Object` is unnecessary qualification and could be
replaced simply by `Object`, which is not what we want. Thus, in order to export
traits, please always rename them using the following convention:

```rust
/// Common traits.
pub mod traits {
    // Read the Rust Style Guide to learn more about the used naming.
    pub use super::Object    as TRAIT_Object;
    pub use super::ObjectOps as TRAIT_ObjectOps;
}
```

Having such a definition, we can import traits to scope using
`use display::object::traits::*`, and we would not have any warning about
unnecessary qualification anymore.
