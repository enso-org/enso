## Enso Signatures 1.0
## module Standard.Base.Meta
- type Type
    - constructors self -> (Standard.Base.Data.Vector.Vector Standard.Base.Meta.Constructor)
    - methods self -> Standard.Base.Data.Vector.Vector
    - qualified_name self -> Standard.Base.Data.Text.Text
    - name self -> Standard.Base.Data.Text.Text
    - find qualified_name:Standard.Base.Data.Text.Text -> Standard.Base.Any.Any
- type Atom
    - value self -> Standard.Base.Any.Any
    - fields self -> (Standard.Base.Data.Vector.Vector Standard.Base.Any.Any)
    - constructor self -> Standard.Base.Meta.Constructor
- type Constructor
    - value self -> Standard.Base.Function.Function
    - fields self -> (Standard.Base.Data.Vector.Vector Standard.Base.Data.Text.Text)
    - name self -> Standard.Base.Data.Text.Text
    - new self fields:(Standard.Base.Data.Vector.Vector|Standard.Base.Data.Array.Array) -> Standard.Base.Any.Any
    - declaring_type self -> Standard.Base.Meta.Type
- type Primitive
    - value self -> Standard.Base.Any.Any
- type Unresolved_Symbol
    - value self -> Standard.Base.Any.Any
    - rename self new_name:Standard.Base.Data.Text.Text -> Standard.Base.Meta.Unresolved_Symbol
    - name self -> Standard.Base.Data.Text.Text
- type Error
    - value self -> Standard.Base.Any.Any
- type Polyglot
    - value self -> Standard.Base.Any.Any
    - get_language self -> Standard.Base.Meta.Language
Standard.Base.Any.Any.is_same_object_as self value:Standard.Base.Any.Any -> Standard.Base.Data.Boolean.Boolean
Standard.Base.Any.Any.is_a self typ:Standard.Base.Any.Any -> Standard.Base.Data.Boolean.Boolean
Standard.Base.Error.Error.is_a self typ:Standard.Base.Any.Any -> Standard.Base.Data.Boolean.Boolean
atom_with_hole factory:Standard.Base.Any.Any -> Standard.Base.Any.Any
meta ~value:Standard.Base.Any.Any -> (Standard.Base.Meta.Atom|Standard.Base.Meta.Constructor|Standard.Base.Meta.Primitive|Standard.Base.Meta.Polyglot|Standard.Base.Meta.Unresolved_Symbol|Standard.Base.Meta.Error|Standard.Base.Meta.Type)
is_same_object value_1:Standard.Base.Any.Any value_2:Standard.Base.Any.Any -> Standard.Base.Data.Boolean.Boolean
is_a value:Standard.Base.Any.Any typ:Standard.Base.Any.Any -> Standard.Base.Data.Boolean.Boolean
type_of value:Standard.Base.Any.Any -> Standard.Base.Any.Any
get_annotation target:Standard.Base.Any.Any method:Standard.Base.Any.Any parameter_name:Standard.Base.Any.Any -> Standard.Base.Any.Any
- type Language
    - Java
    - Unknown
is_atom_constructor ~value:Standard.Base.Any.Any -> Standard.Base.Data.Boolean.Boolean
is_atom value:Standard.Base.Any.Any -> Standard.Base.Data.Boolean.Boolean
is_error value:Standard.Base.Any.Any -> Standard.Base.Data.Boolean.Boolean
is_type value:Standard.Base.Any.Any -> Standard.Base.Data.Boolean.Boolean
is_polyglot value:Standard.Base.Any.Any -> Standard.Base.Data.Boolean.Boolean
Standard.Base.Meta.Type.from that:Standard.Base.Any.Any -> Standard.Base.Meta.Type
