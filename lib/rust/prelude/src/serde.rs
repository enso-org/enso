//! Module for utilities related to serialization/deserialization using the `serde` library.

use serde::Deserialize;



/// Try to deserialize value of type `Ret`. In case of any error, it is ignored and the default
/// value is returned instead.
pub fn deserialize_or_default<'d, Ret, D>(d: D) -> Result<Ret, D::Error>
where
    for<'e> Ret: Default + Deserialize<'e>,
    D: serde::Deserializer<'d>, {
    // We first parse as generic JSON value. This is necessary to consume parser input.
    // If we just tried parsing the desired type directly and ignored error, we would end up with
    // `trailing characters` error in non-trivial cases.
    let json_value = serde_json::Value::deserialize(d)?;
    serde_json::from_value(json_value).or_else(|_error| Ok(Ret::default()))
}

/// Deserialize a JSON value that is either of `Ret` type or equals `null`. A `null` is converted
/// to a default value of `Ret` type.
///
/// Example usage:
/// ```
/// # use serde::Deserialize;
/// # use enso_prelude::deserialize_null_as_default;
/// #[derive(Debug, Deserialize, PartialEq)]
/// struct Foo {
///     #[serde(default, deserialize_with = "deserialize_null_as_default")]
///     blah: Vec<i32>,
/// }
/// fn check_deserialized_eq(code: &str, expected_deserialized: &Foo) {
///     let deserialized = serde_json::from_str::<Foo>(code).unwrap();
///     assert_eq!(&deserialized, expected_deserialized);
/// }
/// let empty_foo = Foo { blah: vec![] };
/// check_deserialized_eq(r#"{"blah" : null }"#, &empty_foo);
/// check_deserialized_eq(r#"{}"#, &empty_foo);
/// check_deserialized_eq(r#"{"blah" : [] }"#, &empty_foo);
/// check_deserialized_eq(r#"{"blah" : [1,2,3] }"#, &Foo { blah: vec![1, 2, 3] });
/// ```
pub fn deserialize_null_as_default<'d, Ret, D>(d: D) -> Result<Ret, D::Error>
where
    for<'e> Ret: Default + Deserialize<'e>,
    D: serde::Deserializer<'d>, {
    let option_value = Option::deserialize(d)?;
    Ok(option_value.unwrap_or_default())
}


#[cfg(test)]
mod tests {
    use super::*;

    use serde::Serialize;

    #[test]
    fn deserialize_or_default_attribute_test() {
        // Two structures - same except for `deserialize_or_default` atribute.
        // One fails to deserialize, second one goes through.
        #[derive(Debug, Deserialize, PartialEq, Eq, Serialize)]
        struct Foo {
            blah: String,
            boom: Vec<i32>,
        }
        #[derive(Debug, Deserialize, Eq, PartialEq, Serialize)]
        struct Bar {
            #[serde(deserialize_with = "deserialize_or_default")]
            blah: String,
            boom: Vec<i32>,
        }
        let code = r#"{"blah" : {}, "boom" : [1,2,3] }"#;
        let result = serde_json::from_str::<Foo>(code);
        assert!(result.is_err());

        let deserialized = serde_json::from_str::<Bar>(code).unwrap();
        assert_eq!(deserialized, Bar { blah: "".into(), boom: vec![1, 2, 3] });
    }

    #[test]
    fn deserialize_or_default_attribute_for_optional_field() {
        #[derive(Debug, Deserialize, Eq, PartialEq, Serialize)]
        struct Foo {
            #[serde(default, deserialize_with = "deserialize_or_default")]
            blah: Option<String>,
            boom: Vec<i32>,
        }
        let code = r#"{"blah" : "blah", "boom" : [1,2,3] }"#;
        let deserialized = serde_json::from_str::<Foo>(code).unwrap();
        assert_eq!(deserialized, Foo { blah: Some("blah".to_owned()), boom: vec![1, 2, 3] });

        let code = r#"{"boom" : [1,2,3] }"#;
        let deserialized = serde_json::from_str::<Foo>(code).unwrap();
        assert_eq!(deserialized, Foo { blah: None, boom: vec![1, 2, 3] });
    }
}
