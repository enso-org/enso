//! Implementation of the [`json_to_struct`] macro, which converts a JSON file to a Rust struct.

// === Features ===
#![feature(exact_size_is_empty)]
#![feature(proc_macro_span)]
#![feature(proc_macro_def_site)]
#![feature(track_path)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]

use enso_prelude::*;

use serde_json::Value;
use std::path::PathBuf;
use std::str::FromStr;



// ==================
// === JSON Utils ===
// ==================

/// Merge two JSON definitions with overriding. Return overridden keys.
fn merge_json(a: &mut Value, b: &Value) -> Vec<String> {
    let mut overrides = vec![];
    merge_internal(a, b, &mut vec![], &mut overrides);
    overrides
}

fn merge_internal(a: &mut Value, b: &Value, path: &mut Vec<String>, overrides: &mut Vec<String>) {
    match (a, b) {
        (&mut Value::Object(ref mut a), Value::Object(b)) =>
            for (k, v) in b {
                path.push(k.clone());
                merge_internal(a.entry(k.clone()).or_insert(Value::Null), v, path, overrides);
                path.pop();
            },
        (a, b) => {
            if a != &Value::Null {
                overrides.push(path.join("."));
            }
            *a = b.clone();
        }
    }
}



// ===================
// === Boilerplate ===
// ===================

/// The boilerplate code which will be added to every generated code by the macro.
const BOILERPLATE: &str = "
/// Error containing the path to the field and the reason of the error.
#[derive(Debug, Clone)]
pub struct Error {
    pub path: String,
    pub reason: String,
}

impl Error {
    /// Constructor.
    pub fn new(path: impl Into<String>, reason: impl Into<String>) -> Self {
        let path = path.into();
        let reason = reason.into();
        Self { path, reason }
    }

    /// Change the path of this error.
    pub fn with_path(self, path: impl Into<String>) -> Self {
        let path = path.into();
        Self { path, ..self }
    }

    /// Convert the error to a human-readable form.
    pub fn display(&self) -> String {
        format!(\"{}: {}\", self.path, self.reason)
    }
}

/// Try setting a boolean value. If the value is not a boolean, returns an error.
pub fn set_bool(target: &mut bool, value: &str, name: &str) -> Option<Error> {
    let parsed_value = match value {
        \"false\" => Some(false),
        \"true\" => Some(true),
        \"0\" => Some(false),
        \"1\" => Some(true),
        \"enabled\" => Some(true),
        \"disabled\" => Some(false),
        \"on\" => Some(true),
        \"off\" => Some(false),
        _ => None
    };
    match parsed_value {
        Some(value) => {
            *target = value;
            None
        },
        None => Some(Error::new(name, format!(\"Expected a boolean, got '{}'\", value)))
    }
}

/// Try setting a number value. If the value is not a number, returns an error.
pub fn set_number(target: &mut f64, value: &str, name: &str) -> Option<Error> {
    if let Ok(number) = value.parse::<f64>() {
        *target = number;
        None
    } else {
        Some(Error::new(name, format!(\"Expected a number, got '{}'\", value)))
    }
}

/// Try setting a string value. If the value is not a string, returns an error.
pub fn set_string(target: &mut String, value: &str, name: &str) -> Option<Error> {
    *target = value.to_owned();
    None
}

/// Allows setting values by names of the original JSON fields. The name can contain dots to refer
/// to nested fields.
#[allow(missing_docs)]
trait Setter {
    fn set(&mut self, name: &str, value: String) -> Option<Error>;
}
";



// ======================
// === JSON To Struct ===
// ======================

/// Converts the provided JSON files into a Rust struct. If more than one file is provided, the
/// files will be merged. In case any keys will overlap, it will panic. Supports all JSON types but
/// arrays.
///
/// For example, for the following JSON file (`test.json`):
/// ```text
/// {
///   "test": {
///     "nested": {
///       "field1": 1
///     },
///     "field2": "value"
///   }
/// }
/// ```
/// The macro call `json_to_struct("test.json")` will include the [`BOILERPLATE`] code, and will
/// generate the following code:
///
/// ```text
/// #[derive(Clone, Debug)]
/// pub struct Args {
///     pub test: RootTest,
/// }
///
/// #[derive(Clone, Debug)]
/// pub struct RootTest {
///     pub field2: String,
///     pub nested: RootTestNested,
/// }
///
/// #[derive(Clone, Debug)]
/// pub struct RootTestNested {
///     pub field1: f64,
/// }
/// ```
///
/// Also, for each struct the implementation of `Default` and `Setter` will be generated.
#[proc_macro]
pub fn json_to_struct(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let paths = files_paths(input);
    mark_paths_as_tracked(&paths);
    let json = read_and_merge_jsons(paths);
    let code = generate(&json);
    // Uncomment for debug purposes:
    // std::fs::write("/tmp/out.txt", code.clone()).unwrap();
    proc_macro::TokenStream::from_str(&code).unwrap()
}

/// Parse the input token stream as file paths.
fn files_paths(input: proc_macro::TokenStream) -> Vec<PathBuf> {
    let span = proc_macro::Span::call_site();
    let call_site_file_desc = span.source_file();
    let call_site_file = PathBuf::from(call_site_file_desc.path().to_str().unwrap());
    let call_site_dir = call_site_file.parent().unwrap();
    let rel_paths: Vec<_> = input
        .into_iter()
        .filter_map(|tt| match tt {
            proc_macro::TokenTree::Literal(lit) => {
                let lit = lit.to_string();
                let lit = lit.trim_matches('"');
                Some(lit.to_string())
            }
            proc_macro::TokenTree::Punct(_) => None,
            _ => panic!("Wrong arguments to macro."),
        })
        .collect();
    rel_paths.into_iter().map(|t| call_site_dir.join(t)).collect()
}

/// Mark the JSON files as tracked, so this macro is re-evaluated when they cahnge.
fn mark_paths_as_tracked(paths: &[PathBuf]) {
    for path in paths {
        let resolved_path = path.canonicalize().unwrap();
        let path_str = resolved_path.to_str().unwrap();
        proc_macro::tracked_path::path(path_str)
    }
}

/// Read the JSON files and merge them into a single JSON value.
fn read_and_merge_jsons(paths: Vec<PathBuf>) -> Value {
    let values: Vec<_> = paths
        .iter()
        .map(|path| {
            let file = std::fs::File::open(path).unwrap();
            let value: Value = serde_json::from_reader(file).unwrap();
            value
        })
        .collect();

    let mut merged = Value::Null;
    for value in values {
        let overlaps = merge_json(&mut merged, &value);
        if !overlaps.is_empty() {
            let overlaps = overlaps.join(", ");
            panic!("The following fields overlap: {overlaps}.");
        }
    }
    merged
}



// =======================
// === Code Generation ===
// =======================

/// Adds a new code line with the given indentation.
fn add_code_line(code: &mut String, indent: usize, line: &str) {
    code.push_str(&"    ".repeat(indent));
    code.push_str(line);
    code.push('\n');
}

/// Wrapper for [`add_code_line`] which uses [`format!`] to generate the line.
macro_rules! ln {
    ($code:expr, $indent:tt, $($ts:tt)*) => {
        add_code_line($code, $indent, &format!($($ts)*))
    };
}

/// The main code generation entry point.
fn generate(val: &Value) -> String {
    let mut decls = vec![];
    generate_rec(val, &mut vec!["Args".to_string()], &mut decls);
    format!("{BOILERPLATE}\n\n{}", decls.join("\n\n"))
}

fn generate_rec(val: &Value, path: &mut Vec<String>, decls: &mut Vec<String>) {
    match val {
        Value::Object(obj) => {
            let name = path.iter().map(|t| t.capitalize_first_letter()).join("");
            let qname = path.join(".");
            let mut decl = String::new();
            let mut imp_default = String::new();
            let mut imp_set = String::new();

            ln!(&mut decl, 0, "#[derive(Clone, Debug)]");
            ln!(&mut decl, 0, "pub struct {name} {{");
            ln!(&mut decl, 1, "pub __name__: &'static str,");

            ln!(&mut imp_default, 0, "#[allow(clippy::derivable_impls)]");
            ln!(&mut imp_default, 0, "impl Default for {name} {{");
            ln!(&mut imp_default, 1, "fn default() -> Self {{");
            ln!(&mut imp_default, 2, "Self {{");
            ln!(&mut imp_default, 3, "__name__: \"{qname}\",");

            ln!(&mut imp_set, 0, "impl Setter for {name} {{");
            ln!(&mut imp_set, 1, "fn set(&mut self, name: &str, v: String) -> Option<Error> {{");

            for (k, v) in obj {
                let key = k.camel_case_to_snake_case();
                path.push(k.clone());
                let qname = path.join(".");
                ln!(&mut decl, 1, "pub {key}: {},", val_to_type(v, path));
                match v {
                    Value::String(v) => {
                        ln!(&mut imp_default, 3, "{key}: \"{v}\".to_string(),");
                        ln!(&mut imp_set, 2, "if name == \"{k}\" {{");
                        ln!(&mut imp_set, 3, "return set_string(&mut self.{key}, &v, \"{qname}\")");
                        ln!(&mut imp_set, 2, "}}");
                    }
                    Value::Number(v) => {
                        ln!(&mut imp_default, 3, "{key}: {},", format_number(v.as_f64().unwrap()));
                        ln!(&mut imp_set, 2, "if name == \"{k}\" {{");
                        ln!(&mut imp_set, 3, "return set_number(&mut self.{key}, &v, \"{qname}\")");
                        ln!(&mut imp_set, 2, "}}");
                    }
                    Value::Bool(v) => {
                        ln!(&mut imp_default, 3, "{key}: {v},");
                        ln!(&mut imp_set, 2, "if name == \"{k}\" {{");
                        ln!(&mut imp_set, 3, "return set_bool(&mut self.{key}, &v, \"{qname}\")");
                        ln!(&mut imp_set, 2, "}}");
                    }
                    Value::Object(_) => {
                        ln!(&mut imp_default, 3, "{key}: Default::default(),");
                        ln!(&mut imp_set, 2, "let pfx = \"{k}.\";");
                        ln!(&mut imp_set, 2, "if let Some(sub_name) = name.strip_prefix(pfx) {{");
                        ln!(&mut imp_set, 3, "return self.{key}.set(sub_name, v)");
                        ln!(&mut imp_set, 4, ".map(|e| e.with_path(\"{qname}\"))");
                        ln!(&mut imp_set, 2, "}}");
                        generate_rec(v, path, decls);
                    }
                    Value::Null => {
                        ln!(&mut imp_default, 3, "{key}: (),");
                        ln!(&mut imp_set, 2, "if name == \"{k}\" {{");
                        ln!(&mut imp_set, 3, "return None");
                        ln!(&mut imp_set, 2, "}}");
                    }
                    Value::Array(_) => panic!("Arrays are not supported ('{qname}')."),
                }
                path.pop();
            }
            ln!(&mut decl, 0, "}}");
            ln!(&mut imp_default, 2, "}}");
            ln!(&mut imp_default, 1, "}}");
            ln!(&mut imp_default, 0, "}}");
            ln!(
                &mut imp_set,
                2,
                "Some(Error::new(\"{qname}\", format!(\"Unknown field '{{}}'.\", name)))"
            );
            ln!(&mut imp_set, 1, "}}");
            ln!(&mut imp_set, 0, "}}");
            decls.push(decl);
            decls.push(imp_default);
            decls.push(imp_set);
        }
        _ => panic!(),
    }
}

/// Format the number to always contain a dot, so it can be properly parsed as Rust code.
fn format_number(num: f64) -> String {
    let mut str = format!("{num}");
    if !str.contains('.') {
        str.push_str(".0");
    }
    str
}

/// Convert a JSON value to a Rust type.
fn val_to_type(val: &Value, path: &[String]) -> String {
    match val {
        Value::String(_) => "String".to_string(),
        Value::Number(_) => "f64".to_string(),
        Value::Bool(_) => "bool".to_string(),
        Value::Object(_) => path.iter().map(|t| t.capitalize_first_letter()).join(""),
        Value::Null => "()".to_string(),
        Value::Array(_) => panic!("Arrays are not supported ('{}').", path.join(".")),
    }
}
