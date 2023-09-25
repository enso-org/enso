//! # Design
//!
//! The format produced by this module is not actually "serial", as such there is a bit of an
//! impedance mismatch using the `Serializer` trait: `serde` presents each field to the
//! serializer once, but we ultimately need to write to two different places in the output for
//! each "boxed" field (the data, and the reference to it).
//!
//! The approach used here is to maintain a stack of the fields of incomplete objects as we
//! descend in to them; when an object is completed, it is moved to the heap. This requires
//! moving each boxed object once.
//!
//! Alternatives:
//! - ⏰ Deferred: Generate a proper non-serializer with `metamodel`. This would support higher
//!   performance, and a recursion-free implementation that would allow arbitrarily-deep trees.
//! - ❌ Rejected: Use the `len` hints provided by `serde` to pre-allocate objects of the correct
//!   size: The requirement that every field have the same size representation would be too onerous.

use serde::ser;
use serde::Serialize;
use std::fmt::Debug;
use std::fmt::Display;
use std::fmt::Formatter;
use serde::ser::SerializeSeq;


// =================
// === Constants ===
// =================

/// Maximum allowed nesting depth of compound objects. This is empirically determined to be reached
/// before stack overflow on supported targets.
// FIXME: WASM test required.
const RECURSION_LIMIT: usize = 1024;



// =================
// === Serialize ===
// =================

pub fn serialize<T: Serialize>(value: T) -> Result<Vec<u8>> {
    let mut serializer = Serializer::new();
    value.serialize(&mut serializer)?;
    serializer.heap.append(&mut serializer.stack);
    Ok(serializer.heap)
}


// ==================
// === Serializer ===
// ==================

/// Converts Rust values to the portable format.
#[derive(Debug, Default)]
pub struct Serializer {
    /// Complete objects, located at their final addresses.
    heap: Vec<u8>,
    /// All the fields of currently-incomplete objects.
    stack: Vec<u8>,
    recursion_depth: usize,
    object_depth: usize,
    parent_structs: Vec<ParentStruct>,
}

impl Serializer {
    pub fn new() -> Self {
        Self::default()
    }

    fn object_serializer(&mut self) -> Result<ObjectSerializer> {
        if self.recursion_depth < RECURSION_LIMIT {
            self.recursion_depth += 1;
            self.object_depth += 1;
            let begin = self.stack.len();
            Ok(ObjectSerializer { serializer: self, begin })
        } else {
            Err(Error::RecursionLimitExceeded)
        }
    }

    fn build_object(&mut self, begin: usize) -> Result<()> {
        use serde::ser::Serializer;
        self.recursion_depth -= 1;
        self.object_depth -= 1;
        let address = self.heap.len();
        self.heap.extend(self.stack.drain(begin..));
        self.serialize_u32(u32::try_from(address).unwrap())
    }
}


// ==== Object Serializer ===

/// Serializes compound types.
#[derive(Debug)]
pub struct ObjectSerializer<'a> {
    serializer: &'a mut Serializer,
    begin: usize,
}

impl<'a> ObjectSerializer<'a> {
    fn finish(self) -> Result<()> {
        self.serializer.build_object(self.begin)
    }
}


// ==== Parent Struct ===

/// Information for transforming a struct into a combined parent/child representation.
#[derive(Debug, Copy, Clone)]
struct ParentStruct {
    object_depth_inside: usize,
    begin: usize,
}


// ==========================================
// === Serialization Trait Implementation ===
// ==========================================

impl<'a> ser::Serializer for &'a mut Serializer {
    type Ok = Ok;
    type Error = Error;

    type SerializeSeq = ObjectSerializer<'a>;
    type SerializeTuple = Self;
    type SerializeTupleStruct = Self;
    type SerializeTupleVariant = ObjectSerializer<'a>;
    type SerializeMap = ObjectSerializer<'a>;
    type SerializeStruct = Self;
    type SerializeStructVariant = ObjectSerializer<'a>;

    fn serialize_bool(self, v: bool) -> Result<()> {
        self.stack.push(v as u8);
        Ok(())
    }

    fn serialize_i8(self, v: i8) -> Result<()> {
        self.stack.push(v as u8);
        Ok(())
    }

    fn serialize_i16(self, v: i16) -> Result<()> {
        self.stack.extend_from_slice(&v.to_le_bytes());
        Ok(())
    }

    fn serialize_i32(self, v: i32) -> Result<()> {
        self.stack.extend_from_slice(&v.to_le_bytes());
        Ok(())
    }

    fn serialize_i64(self, v: i64) -> Result<()> {
        self.stack.extend_from_slice(&v.to_le_bytes());
        Ok(())
    }

    fn serialize_u8(self, v: u8) -> Result<()> {
        self.stack.push(v);
        Ok(())
    }

    fn serialize_u16(self, v: u16) -> Result<()> {
        self.stack.extend_from_slice(&v.to_le_bytes());
        Ok(())
    }

    fn serialize_u32(self, v: u32) -> Result<()> {
        self.stack.extend_from_slice(&v.to_le_bytes());
        Ok(())
    }

    fn serialize_u64(self, v: u64) -> Result<()> {
        self.stack.extend_from_slice(&v.to_le_bytes());
        Ok(())
    }

    fn serialize_f32(self, v: f32) -> Result<()> {
        self.serialize_u32(v.to_bits())
    }

    fn serialize_f64(self, v: f64) -> Result<()> {
        self.serialize_u64(v.to_bits())
    }

    fn serialize_char(self, v: char) -> Result<()> {
        self.serialize_u32(v.into())
    }

    fn serialize_str(self, v: &str) -> Result<()> {
        self.serialize_bytes(v.as_bytes())
    }

    fn serialize_bytes(self, v: &[u8]) -> Result<()> {
        let ser = self.serialize_seq(Some(v.len()))?;
        ser.serializer.stack.extend_from_slice(v);
        ser.finish()
    }

    fn serialize_none(self) -> Result<()> {
        self.serialize_u8(0)?;
        self.serialize_u32(0xcdcdcdcd)
    }

    fn serialize_some<T>(self, value: &T) -> Result<()>
        where T: ?Sized + Serialize {
        self.serialize_u8(1)?;
        let object = self.object_serializer()?;
        value.serialize(&mut *object.serializer)?;
        object.finish()
    }

    fn serialize_unit(self) -> Result<()> {
        Ok(())
    }

    fn serialize_unit_struct(self, _name: &'static str) -> Result<()> {
        self.serialize_unit()
    }

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        variant_index: u32,
        _variant: &'static str,
    ) -> Result<()> {
        let object = self.object_serializer()?;
        variant_index.serialize(&mut *object.serializer)?;
        object.finish()
    }

    fn serialize_newtype_struct<T>(self, _name: &'static str, value: &T) -> Result<()>
        where T: ?Sized + Serialize {
        value.serialize(self)
    }

    fn serialize_newtype_variant<T>(
        self,
        name: &'static str,
        variant_index: u32,
        _variant: &'static str,
        value: &T,
    ) -> Result<()>
        where
            T: ?Sized + Serialize,
    {
        if name == "Variant"
                && let Some(ancestor) = self.parent_structs.last()
                && ancestor.object_depth_inside == self.object_depth {
            let parent_start = ancestor.begin;
            // Add the child's fields to the stack (following the parent's fields).
            value.serialize(&mut *self)?;
            // Build the object on the heap.
            let address = self.heap.len();
            self.heap.extend_from_slice(&variant_index.to_le_bytes());
            self.heap.extend(self.stack.drain(parent_start..));
            self.serialize_u32(u32::try_from(address).unwrap())?;
        } else {
            let mut ser = self.object_serializer()?;
            ser.serialize_element(&variant_index)?;
            ser.serialize_element(value)?;
            ser.finish()?;
        }
        Ok(())
    }

    fn serialize_seq(self, len: Option<usize>) -> Result<Self::SerializeSeq> {
        let len = len.unwrap();
        let mut ser = self.object_serializer()?;
        ser.serialize_element(&u32::try_from(len).unwrap())?;
        Ok(ser)
    }

    fn serialize_tuple(self, _len: usize) -> Result<Self::SerializeTuple> {
        self.object_depth += 1;
        Ok(self)
    }

    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleStruct> {
        self.object_depth += 1;
        Ok(self)
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleVariant> {
        let ser = self.object_serializer()?;
        variant_index.serialize(&mut *ser.serializer)?;
        Ok(ser)
    }

    fn serialize_map(self, len: Option<usize>) -> Result<Self::SerializeMap> {
        self.serialize_seq(len)
    }

    fn serialize_struct(self, name: &'static str, _len: usize) -> Result<Self::SerializeStruct> {
        self.object_depth += 1;
        if matches!(name, "Tree" | "Token") {
            let object_depth_inside = self.object_depth;
            let begin = self.stack.len();
            self.parent_structs.push(ParentStruct { object_depth_inside, begin });
        }
        Ok(self)
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant> {
        let ser = self.object_serializer()?;
        variant_index.serialize(&mut *ser.serializer)?;
        Ok(ser)
    }
}


// === Inline Compound Type Trait Implementations ===

impl ser::SerializeStruct for &'_ mut Serializer {
    type Ok = Ok;
    type Error = Error;

    fn serialize_field<T>(&mut self, _key: &'static str, value: &T) -> Result<()>
        where T: ?Sized + Serialize {
        value.serialize(&mut **self)
    }

    fn end(self) -> Result<()> {
        if let Some(ancestor) = self.parent_structs.last() {
            if ancestor.object_depth_inside == self.object_depth {
                self.parent_structs.pop();
            }
        }
        self.object_depth -= 1;
        Ok(())
    }
}

impl ser::SerializeTuple for &'_ mut Serializer {
    type Ok = Ok;
    type Error = Error;

    fn serialize_element<T>(&mut self, value: &T) -> Result<()>
        where T: ?Sized + Serialize {
        value.serialize(&mut **self)
    }

    fn end(self) -> Result<()> {
        self.object_depth -= 1;
        Ok(())
    }
}

impl ser::SerializeTupleStruct for &'_ mut Serializer {
    type Ok = Ok;
    type Error = Error;

    fn serialize_field<T>(&mut self, value: &T) -> Result<()>
        where T: ?Sized + Serialize {
        value.serialize(&mut **self)
    }

    fn end(self) -> Result<()> {
        self.object_depth -= 1;
        Ok(())
    }
}


// === Boxed Compound Type Trait Implementations ===

impl ser::SerializeStructVariant for ObjectSerializer<'_> {
    type Ok = Ok;
    type Error = Error;

    fn serialize_field<T>(&mut self, _key: &'static str, value: &T) -> Result<()>
        where T: ?Sized + Serialize {
        value.serialize(&mut *self.serializer)
    }

    fn end(self) -> Result<()> {
        self.finish()
    }
}

impl SerializeSeq for ObjectSerializer<'_> {
    type Ok = Ok;
    type Error = Error;

    fn serialize_element<T>(&mut self, value: &T) -> Result<()>
        where T: ?Sized + Serialize {
        value.serialize(&mut *self.serializer)
    }

    fn end(self) -> Result<()> {
        self.finish()
    }
}

impl ser::SerializeTupleVariant for ObjectSerializer<'_> {
    type Ok = Ok;
    type Error = Error;

    fn serialize_field<T>(&mut self, value: &T) -> Result<()>
        where T: ?Sized + Serialize {
        value.serialize(&mut *self.serializer)
    }

    fn end(self) -> Result<()> {
        self.finish()
    }
}

impl ser::SerializeMap for ObjectSerializer<'_> {
    type Ok = Ok;
    type Error = Error;

    fn serialize_key<T>(&mut self, key: &T) -> Result<()>
        where T: ?Sized + Serialize {
        key.serialize(&mut *self.serializer)
    }

    fn serialize_value<T>(&mut self, value: &T) -> Result<()>
        where T: ?Sized + Serialize {
        value.serialize(&mut *self.serializer)
    }

    fn end(self) -> Result<()> {
        self.finish()
    }
}


// ====================
// === Result Types ===
// ====================

pub type Ok = ();

#[derive(Debug)]
pub enum Error {
    RecursionLimitExceeded,
    Custom(String),
}

impl ser::StdError for Error {}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}

impl ser::Error for Error {
    fn custom<T>(msg: T) -> Self
        where T: Display {
        Self::Custom(msg.to_string())
    }
}

pub type Result<T> = std::result::Result<T, Error>;


// =============
// === Tests ===
// =============

#[cfg(test)]
mod test {
    use serde::Serialize;

    #[test]
    fn test_infinite_recursion() {
        use std::rc::Rc;
        use std::cell::RefCell;
        /// A serializable object containing a reference to itself.
        #[derive(Serialize)]
        struct Cyclic {
            this: RefCell<Option<Rc<Cyclic>>>,
        }
        impl Cyclic {
            fn new() -> Rc<Self> {
                let cyclic = Rc::new(Cyclic { this: RefCell::new(None) });
                *cyclic.this.borrow_mut() = Some(Rc::clone(&cyclic));
                cyclic
            }
        }
        // Note that if recursion is not adequately limited the expected failure mode is aborting
        // due to stack overflow. We are just checking `is_err` here for good measure.
        assert!(super::serialize(&Cyclic::new()).is_err());
    }
}
