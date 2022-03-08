use lazy_static::lazy_static;
use regex::Regex;
use std::collections::HashMap;
use std::ffi::OsStr;
use std::fmt::Debug;
use std::fs;
use std::path::Path;
use std::path::PathBuf;



#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum HeaderToken {
    Attrib,
    ModuleAttrib,
    ModuleAttribWarn,
    ModuleAttribAllow,
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
}

use HeaderToken::*;

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
    pub fn new(token: HeaderToken, reg_match: String) -> Self {
        let attrs = Default::default();
        Self { attrs, token, reg_match }
    }

    pub fn len(&self) -> usize {
        let args_len: usize = self.attrs.iter().map(|t| t.len()).sum();
        self.reg_match.len() + args_len
    }

    pub fn to_string(&self) -> String {
        format!("{}{}", self.attrs.join(""), self.reg_match)
    }
}

fn find_with<'s, 't, T>(
    input: &'s str,
    regex: &'t Regex,
    f: impl FnOnce(String) -> T,
) -> Result<(), T> {
    match regex.find(input) {
        Some(t) => Err(f(t.as_str().into())),
        None => Ok(()),
    }
}

fn re(input: &str) -> Regex {
    let str = format!(r"^ *{} *(; *)?((\r\n?)|\n)", input);
    Regex::new(&str).unwrap()
}


#[rustfmt::skip]
lazy_static! {
    static ref r_empty_line: Regex             = re(r"");
    static ref r_module_comment: Regex         = re(r"//![^\n\r]*");
    static ref r_comment: Regex                = re(r"//[^\n\r]*");
    static ref r_crate_use: Regex              = re(r"use +crate( *:: *[\w]+)*( +as +[\w]+)?");
    static ref r_crate_use_star: Regex         = re(r"use +crate( *:: *[\w*]+)*");
    static ref r_crate_pub_use: Regex          = re(r"pub +use +crate( *:: *[\w]+)*( +as +[\w]+)?");
    static ref r_crate_pub_use_star: Regex     = re(r"pub +use +crate( *:: *[\w*]+)*");
    static ref r_use: Regex                    = re(r"use +[\w]+( *:: *[\w]+)*( +as +[\w]+)?");
    static ref r_use_star: Regex               = re(r"use +[\w]+( *:: *[\w*]+)*");
    static ref r_pub_use: Regex                = re(r"pub +use +[\w]+( *:: *[\w]+)*( +as +[\w]+)?");
    static ref r_pub_use_star: Regex           = re(r"pub +use +[\w]+( *:: *[\w*]+)*");
    static ref r_module_attrib_feature: Regex  = re(r"#!\[feature[^\]]*\]");
    static ref r_module_attrib_feature2: Regex = re(r"#!\[allow\(incomplete_features\)\]");
    static ref r_module_attrib_warn: Regex     = re(r"#!\[warn[^\]]*\]");
    static ref r_module_attrib_allow: Regex    = re(r"#!\[allow[^\]]*\]");
    static ref r_module_attrib: Regex          = re(r"#!\[[^\]]*\]");
    static ref r_attrib: Regex                 = re(r"#\[[^\]]*\]");
    static ref r_pub_mod: Regex                = re(r"pub +mod +[\w]+");
}


fn match_header_internal(input: &str) -> Result<(), HeaderElement> {
    find_with(input, &r_empty_line, |t| HeaderElement::new(EmptyLine, t))?;
    find_with(input, &r_module_comment, |t| HeaderElement::new(ModuleDoc, t))?;
    find_with(input, &r_comment, |t| HeaderElement::new(Comment, t))?;

    find_with(input, &r_crate_use, |t| HeaderElement::new(CrateUse, t))?;
    find_with(input, &r_crate_use_star, |t| HeaderElement::new(CrateUseStar, t))?;
    find_with(input, &r_crate_pub_use, |t| HeaderElement::new(CratePubUse, t))?;
    find_with(input, &r_crate_pub_use_star, |t| HeaderElement::new(CratePubUseStar, t))?;

    find_with(input, &r_use, |t| HeaderElement::new(Use, t))?;
    find_with(input, &r_use_star, |t| HeaderElement::new(UseStar, t))?;
    find_with(input, &r_pub_use, |t| HeaderElement::new(PubUse, t))?;
    find_with(input, &r_pub_use_star, |t| HeaderElement::new(PubUseStar, t))?;

    find_with(input, &r_module_attrib_feature, |t| HeaderElement::new(ModuleAttribFeature, t))?;
    find_with(input, &r_module_attrib_feature2, |t| HeaderElement::new(ModuleAttribFeature2, t))?;
    find_with(input, &r_module_attrib_warn, |t| HeaderElement::new(ModuleAttribWarn, t))?;
    find_with(input, &r_module_attrib_allow, |t| HeaderElement::new(ModuleAttribAllow, t))?;
    find_with(input, &r_module_attrib, |t| HeaderElement::new(ModuleAttrib, t))?;

    find_with(input, &r_attrib, |t| HeaderElement::new(Attrib, t))?;
    find_with(input, &r_pub_mod, |t| HeaderElement::new(PubMod, t))?;
    Ok(())
}

fn match_header(input: &str) -> Option<HeaderElement> {
    match match_header_internal(input) {
        Err(t) => Some(t),
        Ok(_) => None,
    }
}

fn main() {
    // process_file("./lib/rust/web/src/lib.rs", false);
    process_path(".");
}

fn process_path(path: impl AsRef<Path>) {
    let paths = discover_paths(path);
    let total = paths.len();
    for (i, sub_path) in paths.iter().enumerate() {
        println!("[{}/{}] Processing {:?}.", i + 1, total, sub_path);
        process_file(sub_path, false);
    }
}

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

fn process_file(path: impl AsRef<Path>, preview: bool) {
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

    // header.reverse();
    let mut ending: Vec<&HeaderElement> = header
        .iter()
        .rev()
        .take_while(|t| (t.token == Comment) || (t.token == EmptyLine))
        .collect();
    ending.reverse();

    let incorrect_ending_len = ending.into_iter().skip_while(|t| t.token == EmptyLine).count();

    header.truncate(header.len() - incorrect_ending_len);

    let total_len: usize = header.iter().map(|t| t.len()).sum();

    let mut header_map = HashMap::<HeaderToken, Vec<String>>::new();


    for elem in header {
        header_map.entry(elem.token).or_default().push(elem.to_string());
    }


    let mut out = String::new();
    print(&mut out, &mut header_map, &[ModuleDoc]);
    print(&mut out, &mut header_map, &[ModuleAttrib]);
    print_h2(&mut out, &header_map, &[ModuleAttribAllow, ModuleAttribWarn], "Linter configuration");
    print(&mut out, &mut header_map, &[ModuleAttribAllow, ModuleAttribWarn]);
    print_h2(&mut out, &header_map, &[ModuleAttribFeature2, ModuleAttribFeature], "Features");
    print(&mut out, &mut header_map, &[ModuleAttribFeature2, ModuleAttribFeature]);

    print(&mut out, &mut header_map, &[CrateUseStar, UseStar]);
    print(&mut out, &mut header_map, &[CrateUse]);
    print(&mut out, &mut header_map, &[Use]);

    print_h1(
        &mut out,
        &header_map,
        &[PubMod, CratePubUseStar, PubUseStar, CratePubUse, PubUse],
        "Export",
    );
    print(&mut out, &mut header_map, &[PubMod]);
    print(&mut out, &mut header_map, &[CratePubUseStar, PubUseStar, CratePubUse, PubUse]);
    out.push_str("\n\n");

    out.push_str(&str[total_len..]);

    if preview {
        println!("{}", out);
    } else {
        fs::write(path, out).expect("Unable to write file");
    }
}

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

fn print(out: &mut String, map: &mut HashMap<HeaderToken, Vec<String>>, tokens: &[HeaderToken]) {
    let sub_results: Vec<bool> = tokens.iter().map(|t| print_single(out, map, *t)).collect();
    if sub_results.iter().any(|t| *t == true) {
        out.push_str("\n");
    }
}

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
