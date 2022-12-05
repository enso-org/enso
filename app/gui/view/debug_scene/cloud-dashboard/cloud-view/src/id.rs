//! Module containing an implementation of a unique identifier type.
//!
//! See [`Id`] for details on identifiers in general, and [`IdVariant`] for instructions on how to
//! implement new identifier variants (e.g., [`ProjectId`], [`OrganizationId`]).

use enso_prelude::*;

use serde::de;



// =================
// === Constants ===
// =================

/// When manually implementing [`Deserialize`] for a struct, [`serde`] requires that we provide a
/// user-facing name for the struct being deserialized, for error messages.
///
/// This constant provides the user-facing name of the [`Id`] struct for deserialization error
/// messages.
///
/// [`Deserialize`]: ::serde::de::Deserialize
const ID_STRUCT_NAME: &str = stringify!(Id);
/// Separator used in string representations of [`Id`]s. See [`Id`] for details.
pub const ID_PREFIX_SEPARATOR: &str = "-";



// ======================
// === OrganizationId ===
// ======================

/// Unique [`Id`] for an organization.
pub type OrganizationId = Id<OrganizationIdVariant>;



// =================
// === ProjectId ===
// =================

/// Unique [`Id`] for a [`Project`].
///
/// [`Project`]: crate::project::Project
pub type ProjectId = Id<ProjectIdVariant>;



// =================
// === IdVariant ===
// =================

/// Our [`Id<T>`] struct provides a convenient implementation of a unique identifier, but it is
/// generic. We want our identifiers to be type-safe (i.e., so we don't accidentally use a
/// [`ProjectId`] where we intended to use an [`OrganizationId`]), so our [`Id`] struct has a marker
/// field to keep track of the type of the [`Id`]. Any struct implementing this [`IdVariant`] trait
/// can thus be used as a marker, by passing it as a type parameter to [`Id::<T>`] (e.g.,
/// [`Id::<ProjectIdVariant>`] becomes a [`Project`] identifier).
///
/// By implementing this trait on a struct, you are creating a new variant of identifier marker. An
/// [`Id<T>`] parameterized with this marker will then have all the useful methods and traits
/// implemented by the [`Id`] struct available to it. For example, your [`Id`] variant will have
/// [`Display`], [`Serialize`], etc.
///
/// To keep things simple, we recommend that you implement this trait on an empty newtype struct (
/// see [`ProjectIdVariant`] for an example). Then provide a type alias for an [`Id::<T>`]
/// parameterized with said struct (see [`ProjectId`] for an example). Then use only the type alias.
///
/// [`Serialize`]: ::serde::Serialize
/// [`Project`]: crate::project::Project
pub trait IdVariant {
    /// Prefix applied to serialized representations of the [`Id`].
    ///
    /// For example, the [`ProjectId`] type has a prefix of `"proj"`, so the serialized
    /// representation of a [`ProjectId`] will look like `"proj-27xJM00p8jWoL2qByTo6tQfciWC"`.
    const PREFIX: &'static str;
}


// === Trait `impl`s ===

// Implement IdVariant for Id<T> itself so that the associated items of the marker type are
// available through Id itself (i.e., so you can do `ProjectId::PREFIX` rather than doing
// `ProjectIdVariant::PREFIX`).
impl<T> IdVariant for Id<T>
where T: IdVariant
{
    const PREFIX: &'static str = T::PREFIX;
}



// =============================
// === OrganizationIdVariant ===
// =============================

/// IdVariant struct that indicates an identifier is for an organization.
#[derive(Clone, Copy, Debug)]
#[non_exhaustive]
pub struct OrganizationIdVariant(PhantomData<()>);


// === Trait `impl`s ===

impl IdVariant for OrganizationIdVariant {
    const PREFIX: &'static str = "org";
}



// ========================
// === ProjectIdVariant ===
// ========================

/// Marker struct that indicates an identifier is for a [`Project`].
///
/// [`Project`]: crate::project::Project
#[derive(Clone, Copy, Debug)]
#[non_exhaustive]
pub struct ProjectIdVariant(PhantomData<()>);


// === Trait `impl`s ===

impl IdVariant for ProjectIdVariant {
    const PREFIX: &'static str = "proj";
}



// ==========
// === Id ===
// ==========

/// An identifer that is used to uniquely identify entities in the Cloud backend (e.g.,
/// [`Project`]s).
///
/// Internally, this is a [K-Sortable Unique IDentifier] (KSUID). KSUIDs are a variant of unique
/// identifier that have some nice properties that are useful to us. For example, KSUIDs are
/// naturally ordered by generation time because they incorporate a timestamp as part of the
/// identifier. This is desirable because it means that entities are stored in our database sorted
/// by their creation time, which means we don't have to sort our entities after fetching them.
///
/// Example:
/// ```
/// # use enso_prelude::*;
/// let id = enso_cloud_view::id::ProjectId::from_str("proj-27xJM00p8jWoL2qByTo6tQfciWC").unwrap();
/// ```
///
/// An [`Id`] string contains: the type of the entity this identifier is for (e.g.
/// [`ProjectId::PREFIX`] for [`Project`], [`OrganizationId::PREFIX`] for organization, etc.),
/// followed by the [`ID_PREFIX_SEPARATOR`], then the base62-encoded KSUID.
///
/// Using KSUIDs for identifiers gives several useful properties:
/// 1. Naturally ordered by generation time;
/// 2. Collision-free, coordination-free, dependency-free;
/// 3. Highly portable representations.
///
/// [K-Sortable Unique IDentifier]: https://github.com/segmentio/ksuid#what-is-a-ksuid
/// [`Project`]: crate::project::Project
#[derive(Clone, Copy, Derivative, Display)]
#[display(bound = "T: IdVariant")]
#[display(fmt = "{}{}{}", "T::PREFIX", "ID_PREFIX_SEPARATOR", "ksuid")]
#[derivative(PartialEq, PartialOrd, Eq, Ord)]
pub struct Id<T> {
    /// The inner [`svix_ksuid::Ksuid`] value for this identifier.
    pub ksuid:  svix_ksuid::Ksuid,
    /// Denotes the target entity type for this identifier; for internal use.
    #[derivative(PartialEq = "ignore", PartialOrd = "ignore", Ord = "ignore")]
    pub marker: PhantomData<T>,
}


// === Trait `impl`s` ===

impl<T> serde::Serialize for Id<T>
where Id<T>: Display
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where S: serde::Serializer {
        serializer.serialize_newtype_struct(ID_STRUCT_NAME, &self.to_string())
    }
}

impl<T> FromStr for Id<T>
where T: IdVariant
{
    type Err = crate::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let ksuid_str = match s.split_twice(T::PREFIX, ID_PREFIX_SEPARATOR) {
            Some(("", "", s)) => s,
            _ => Err(format!("\"{s}\" is not an {ID_STRUCT_NAME}."))?,
        };
        let ksuid = svix_ksuid::Ksuid::from_str(ksuid_str)
            .map_err(|_| format!("\"{ksuid_str}\" is not a Ksuid."))?;
        let marker = PhantomData;
        Ok(Self { ksuid, marker })
    }
}

/// This implementation is provided manually because [`Id`] is a dash-separated string, which
/// [`serde`] can't derive [`Deserialize`] for. By manually implementing [`Deserialize`], we can
/// delegate deserialization to our [`FromStr`] implementation which can parse an [`Id`] from a
/// [`String`] by splitting it into its [`PREFIX`] and [`Ksuid`] components.
///
/// [`Deserialize`]: ::serde::de::Deserialize
/// [`PREFIX`]: crate::id::IdVariant::PREFIX
/// [`Ksuid`]: ::svix_ksuid::Ksuid
impl<'a, T> serde::Deserialize<'a> for Id<T>
where Id<T>: FromStr
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where D: serde::Deserializer<'a> {
        struct IdVisitor<T>(PhantomData<T>);

        impl<'a, T> de::Visitor<'a> for IdVisitor<T>
        where Id<T>: FromStr
        {
            type Value = Id<T>;

            fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.write_str("identifier")
            }

            fn visit_newtype_struct<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
            where D: serde::Deserializer<'a> {
                deserializer.deserialize_any(IdVisitor(PhantomData))
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where E: de::Error {
                v.parse().map_err(|_| {
                    let unexpected = de::Unexpected::Str(v);
                    de::Error::invalid_value(unexpected, &"identifier string")
                })
            }
        }

        deserializer.deserialize_any(IdVisitor(PhantomData))
    }
}

impl<T> Debug for Id<T>
where T: IdVariant
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple(ID_STRUCT_NAME).field(&format_args!("{}", self)).finish()
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod test {
    use enso_prelude::*;

    use crate::id;

    #[test]
    fn test_id_from_str() {
        fn check(id_str: &str, expected_str: &str) {
            let result = id::OrganizationId::from_str(id_str);
            let result = result.map(|v| v.to_string());
            let result = result.map_err(|e| e.to_string());
            let debug_str = format!("{:?}", result);
            assert_eq!(debug_str, expected_str);
        }

        check("not_a_valid_id", r#"Err("\"not_a_valid_id\" is not an Id.")"#);
        check("org-", r#"Err("\"\" is not a Ksuid.")"#);
        check("org-27xJM00p8jWoL2qByTo6tQfciWC", r#"Ok("org-27xJM00p8jWoL2qByTo6tQfciWC")"#);
    }

    #[test]
    fn test_serialize_id() {
        fn check(id_str: &str, expected_str: &str) {
            let id = id::OrganizationId::from_str(id_str).unwrap();
            let result = serde_json::to_string(&id);
            let debug_str = format!("{:?}", result);
            assert_eq!(debug_str, expected_str);
        }

        check("org-27xJM00p8jWoL2qByTo6tQfciWC", r#"Ok("\"org-27xJM00p8jWoL2qByTo6tQfciWC\"")"#);
    }

    #[test]
    fn test_deserialize_id() {
        fn check(id_str: &str, expected_str: &str) {
            let result = serde_json::from_str::<id::OrganizationId>(id_str);
            let result = result.map(|v| v.to_string());
            let debug_str = format!("{:?}", result);
            assert_eq!(debug_str, expected_str);
        }

        check(r#""org-27xJM00p8jWoL2qByTo6tQfciWC""#, r#"Ok("org-27xJM00p8jWoL2qByTo6tQfciWC")"#);
    }
}
