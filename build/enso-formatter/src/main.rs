//! This crate implements code formatter rules that are not implemented in rustfmt. These rules
//! are this codebase specific, and they may not be desired in other code bases, including:
//! - Sorting imports into groups (e.g. local imports, pub imports, etc.).
//! - Sorting module attributes into groups.
//! - Adding standard lint configuration to `lib.rs` and `main.rs` files.
//!
//! Possible extensions not yet implemented:
//! - Emitting warnings about star imports that are not ending with `traits::*` nor `prelude::*`.
//! - Sections are automatically keeping spacing.

// === Standard Linter Configuration ===
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

use lazy_static::lazy_static;
use regex::Regex;
use std::collections::HashMap;
use std::ffi::OsStr;
use std::fmt::Debug;
use std::fs;
use std::path::Path;
use std::path::PathBuf;



// =================
// === Constants ===
// =================

/// Standard linter configuration. It will be used in every `main.rs` and `lib.rs` file in the
/// codebase.
const STD_LINTER_ATTRIBS: &[&str] = &[
    "deny(unconditional_recursion)",
    "warn(missing_copy_implementations)",
    "warn(missing_debug_implementations)",
    "warn(missing_docs)",
    "warn(trivial_casts)",
    "warn(trivial_numeric_casts)",
    "warn(unsafe_code)",
    "warn(unused_import_braces)",
    "warn(unused_qualifications)",
];



// ===================
// === HeaderToken ===
// ===================

use HeaderToken::*;

/// A token that can be found in the header of a file.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
#[allow(missing_docs)]
pub enum HeaderToken {
    Attrib,
    ModuleAttrib,
    ModuleAttribWarn,
    ModuleAttribAllow,
    ModuleAttribDeny,
    ModuleAttribFeature,
    ModuleAttribFeature2,
    EmptyLine,
    ModuleDoc,
    Comment,
    CrateUse,
    CrateUseStar,
    CratePubUse,
    CratePubUseStar,
    Use,
    UseStar,
    PubUse,
    PubUseStar,
    PubMod,
    /// Special header token that is never parsed, but can be injected by the code.
    StandardLinterConfig,
}

/// A header token with the matched string and possibly attached attributes.
#[derive(Clone)]
pub struct HeaderElement {
    attrs:     Vec<String>,
    token:     HeaderToken,
    reg_match: String,
}

impl Debug for HeaderElement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}({:?})", self.token, self.reg_match.as_str())
    }
}

impl HeaderElement {
    /// Constructor.
    pub fn new(token: HeaderToken, reg_match: String) -> Self {
        let attrs = Default::default();
        Self { attrs, token, reg_match }
    }

    /// Length of the splice. Includes the length of the matched string and all attached attributes.
    pub fn len(&self) -> usize {
        let args_len: usize = self.attrs.iter().map(|t| t.len()).sum();
        self.reg_match.len() + args_len
    }

    /// Convert the element to a string representation.
    pub fn to_string(&self) -> String {
        format!("{}{}", self.attrs.join(""), self.reg_match)
    }
}

/// Wrappers for [`Regex::find`] which returns [`Result::Err`] on element found. It allows combining
/// multiple calls to this function with the Rust `?` syntax.
fn find_with<T>(input: &str, regex: &Regex, f: impl FnOnce(String) -> T) -> Result<(), T> {
    match regex.find(input) {
        Some(t) => Err(f(t.as_str().into())),
        None => Ok(()),
    }
}

/// Regex constructor that starts on the beginning of a line, can be surrounded by whitespaces and
/// ends with a line break.
fn re(input: &str) -> Regex {
    let str = format!(r"^ *{} *(; *)?((\r\n?)|\n)", input);
    Regex::new(&str).unwrap()
}

macro_rules! define_rules {
    ($($name:ident = $re:tt;)*) => {
        #[allow(non_upper_case_globals)]
        mod static_re {
            use super::*;
            lazy_static! {
                $(
                    pub static ref $name: Regex = re($re);
                )*
            }
        }

        fn match_header_internal(input: &str) -> Result<(), HeaderElement> {
            $( find_with(input, &static_re::$name, |t| HeaderElement::new($name, t))?; )*
            Ok(())
        }
    };
}

define_rules! {
    EmptyLine            = r"";
    ModuleDoc            = r"//![^\n\r]*";
    Comment              = r"//[^\n\r]*";
    CrateUse             = r"use +crate( *:: *[\w]+)*( +as +[\w]+)?";
    CrateUseStar         = r"use +crate( *:: *[\w*]+)*";
    CratePubUse          = r"pub +use +crate( *:: *[\w]+)*( +as +[\w]+)?";
    CratePubUseStar      = r"pub +use +crate( *:: *[\w*]+)*";
    Use                  = r"use +[\w]+( *:: *[\w]+)*( +as +[\w]+)?";
    UseStar              = r"use +[\w]+( *:: *[\w*]+)*";
    PubUse               = r"pub +use +[\w]+( *:: *[\w]+)*( +as +[\w]+)?";
    PubUseStar           = r"pub +use +[\w]+( *:: *[\w*]+)*";
    ModuleAttribFeature  = r"#!\[feature[^\]]*\]";
    ModuleAttribFeature2 = r"#!\[allow\(incomplete_features\)\]";
    ModuleAttribWarn     = r"#!\[warn[^\]]*\]";
    ModuleAttribAllow    = r"#!\[allow[^\]]*\]";
    ModuleAttribDeny     = r"#!\[deny[^\]]*\]";
    ModuleAttrib         = r"#!\[[^\]]*\]";
    Attrib               = r"#\[[^\]]*\]";
    PubMod               = r"pub +mod +[\w]+";
}

fn match_header(input: &str) -> Option<HeaderElement> {
    match match_header_internal(input) {
        Err(t) => Some(t),
        Ok(_) => None,
    }
}



// =======================
// === Pretty printing ===
// =======================

/// Prints H1 section if any of the provided tokens was used in the file being formatted.
fn print_h1(
    out: &mut String,
    map: &HashMap<HeaderToken, Vec<String>>,
    tokens: &[HeaderToken],
    str: &str,
) {
    if tokens.iter().map(|tok| map.contains_key(tok)).any(|t| t) {
        out.push_str("\n");
        out.push_str(&format!("// ===={}====\n", "=".repeat(str.len())));
        out.push_str(&format!("// === {} ===\n", str));
        out.push_str(&format!("// ===={}====\n", "=".repeat(str.len())));
        out.push_str("\n");
    }
}

/// Prints H2 section if any of the provided tokens was used in the file being formatted.
fn print_h2(
    out: &mut String,
    map: &HashMap<HeaderToken, Vec<String>>,
    tokens: &[HeaderToken],
    str: &str,
) {
    if tokens.iter().map(|tok| map.contains_key(tok)).any(|t| t) {
        out.push_str(&format!("// === {} ===\n", str));
    }
}

/// Prints all the entries associated with the provided tokens. If at least one entry was printed,
/// an empty line will be added in the end.
fn print(out: &mut String, map: &mut HashMap<HeaderToken, Vec<String>>, tokens: &[HeaderToken]) {
    let sub_results: Vec<bool> = tokens.iter().map(|t| print_single(out, map, *t)).collect();
    if sub_results.iter().any(|t| *t) {
        out.push_str("\n");
    }
}

/// Print all the entries associated with the provided token.
fn print_single(
    out: &mut String,
    map: &mut HashMap<HeaderToken, Vec<String>>,
    token: HeaderToken,
) -> bool {
    match map.remove(&token) {
        None => false,
        Some(t) => {
            out.push_str(&t.join(""));
            true
        }
    }
}


// =============
// === Logic ===
// =============

/// Process all files of the given path recursively.
fn process_path(path: impl AsRef<Path>) {
    let paths = discover_paths(path);
    let total = paths.len();
    for (i, sub_path) in paths.iter().enumerate() {
        let file_name = sub_path.file_name().map(|s| s.to_str()).flatten();
        let is_main_file = file_name == Some("lib.rs") || file_name == Some("main.rs");
        let dbg_msg = if is_main_file { " [main]" } else { "" };
        println!("[{}/{}] Processing {:?}{}.", i + 1, total, sub_path, dbg_msg);
        process_file(sub_path, false, is_main_file);
    }
}

/// Discover all paths containing Rust sources, recursively.
fn discover_paths(path: impl AsRef<Path>) -> Vec<PathBuf> {
    let mut vec = Vec::default();
    discover_paths_internal(&mut vec, path);
    vec
}

fn discover_paths_internal(vec: &mut Vec<PathBuf>, path: impl AsRef<Path>) {
    let path = path.as_ref();
    let md = fs::metadata(path).unwrap();
    if md.is_dir() {
        let sub_paths = fs::read_dir(path).unwrap();
        for sub_path in sub_paths {
            discover_paths_internal(vec, &sub_path.unwrap().path())
        }
    } else if md.is_file() && path.extension() == Some(OsStr::new("rs")) {
        vec.push(path.into());
    }
}

/// Process a single source file.
fn process_file(path: impl AsRef<Path>, preview: bool, is_main_file: bool) {
    let path = path.as_ref();
    let str = fs::read_to_string(path).unwrap();

    let mut str_ptr: &str = &str;
    let mut attrs = vec![];
    let mut header = vec![];
    loop {
        match match_header(str_ptr) {
            None => break,
            Some(mut m) => {
                str_ptr = &str_ptr[m.len()..];
                match m.token {
                    Attrib => attrs.push(m),
                    _ => {
                        if !attrs.is_empty() {
                            let old_attrs = std::mem::take(&mut attrs);
                            m.attrs = old_attrs.into_iter().map(|t| t.reg_match).collect();
                        }
                        header.push(m)
                    }
                }
            }
        }
    }

    // Do not consume the leading comments.
    let mut ending: Vec<&HeaderElement> = header
        .iter()
        .rev()
        .take_while(|t| (t.token == Comment) || (t.token == EmptyLine))
        .collect();
    ending.reverse();
    let incorrect_ending_len = ending.into_iter().skip_while(|t| t.token == EmptyLine).count();
    header.truncate(header.len() - incorrect_ending_len);
    let total_len: usize = header.iter().map(|t| t.len()).sum();

    // Build a mapping between tokens and registered entries.
    let mut map = HashMap::<HeaderToken, Vec<String>>::new();
    for elem in header {
        map.entry(elem.token).or_default().push(elem.to_string());
    }

    // Remove standard linter configuration from the configuration found in the file.
    if is_main_file {
        let vec = map.entry(ModuleAttribAllow).or_default();
        vec.retain(|t| !STD_LINTER_ATTRIBS.iter().map(|s| t.contains(s)).any(|b| b));
        if vec.is_empty() {
            map.remove(&ModuleAttribAllow);
        }

        let vec = map.entry(ModuleAttribDeny).or_default();
        vec.retain(|t| !STD_LINTER_ATTRIBS.iter().map(|s| t.contains(s)).any(|b| b));
        if vec.is_empty() {
            map.remove(&ModuleAttribDeny);
        }

        let vec = map.entry(ModuleAttribWarn).or_default();
        vec.retain(|t| !STD_LINTER_ATTRIBS.iter().map(|s| t.contains(s)).any(|b| b));
        if vec.is_empty() {
            map.remove(&ModuleAttribWarn);
        }

        let std_linter_attribs = STD_LINTER_ATTRIBS.iter().map(|t| format!("#![{}]\n", t));
        map.entry(StandardLinterConfig).or_default().extend(std_linter_attribs);
    }

    // Print the results.
    let mut out = String::new();
    print(&mut out, &mut map, &[ModuleDoc]);
    print(&mut out, &mut map, &[ModuleAttrib]);
    print_h2(&mut out, &map, &[StandardLinterConfig], "Standard Linter Configuration");
    print(&mut out, &mut map, &[StandardLinterConfig]);
    print_h2(
        &mut out,
        &map,
        &[ModuleAttribAllow, ModuleAttribDeny, ModuleAttribWarn],
        "Non-Standard Linter Configuration",
    );
    print(&mut out, &mut map, &[ModuleAttribAllow, ModuleAttribDeny, ModuleAttribWarn]);
    print_h2(&mut out, &map, &[ModuleAttribFeature2, ModuleAttribFeature], "Features");
    print(&mut out, &mut map, &[ModuleAttribFeature2, ModuleAttribFeature]);

    print(&mut out, &mut map, &[CrateUseStar, UseStar]);
    print(&mut out, &mut map, &[CrateUse]);
    print(&mut out, &mut map, &[Use]);

    print_h1(&mut out, &map, &[PubMod, CratePubUseStar, PubUseStar, CratePubUse, PubUse], "Export");
    print(&mut out, &mut map, &[PubMod]);
    print(&mut out, &mut map, &[CratePubUseStar, PubUseStar, CratePubUse, PubUse]);
    out.push_str("\n\n");

    out.push_str(&str[total_len..]);

    if preview {
        println!("{}", out);
    } else {
        fs::write(path, out).expect("Unable to write file");
    }
}



fn main() {
    // process_file("./lib/rust/ensogl/core/src/lib.rs", false, true);
    process_path(".");
}
