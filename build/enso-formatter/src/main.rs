use regex::Regex;
use std::collections::HashMap;
use std::fmt::Debug;
use std::fs;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum HeaderToken {
    Attrib,
    ModuleAttrib,
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
pub struct HeaderElement<'t> {
    attrs:     Vec<regex::Match<'t>>,
    token:     HeaderToken,
    reg_match: regex::Match<'t>,
}

impl<'t> Debug for HeaderElement<'t> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}({:?})", self.token, self.reg_match.as_str())
    }
}

impl<'t> HeaderElement<'t> {
    pub fn new(token: HeaderToken, reg_match: regex::Match<'t>) -> Self {
        let attrs = Default::default();
        Self { attrs, token, reg_match }
    }

    pub fn len(&self) -> usize {
        self.reg_match.end()
    }

    pub fn as_str(&self) -> &str {
        self.reg_match.as_str()
    }
}

fn find_with<'s, 't, T>(
    input: &'s str,
    regex: &'t Regex,
    f: impl FnOnce(regex::Match<'s>) -> T,
) -> Result<(), T> {
    match regex.find(input) {
        Some(t) => Err(f(t)),
        None => Ok(()),
    }
}

fn re(input: &str) -> Regex {
    let str = format!(r"^ *{} *(; *)?((\r\n?)|\n)", input);
    Regex::new(&str).unwrap()
}

#[rustfmt::skip]
fn match_header_internal(input: &str) -> Result<(), HeaderElement> {
    let r_empty_line         = re(r"");
    let r_module_comment     = re(r"//![^\n\r]*");
    let r_comment            = re(r"//[^\n\r]*");
    let r_crate_use          = re(r"use +crate( *:: *[\w]+)*( +as +[\w]+)?");
    let r_crate_use_star     = re(r"use +crate( *:: *[\w*]+)*");
    let r_crate_pub_use      = re(r"pub +use +crate( *:: *[\w]+)*( +as +[\w]+)?");
    let r_crate_pub_use_star = re(r"pub +use +crate( *:: *[\w*]+)*");
    let r_use                = re(r"use +[\w]+( *:: *[\w]+)*( +as +[\w]+)?");
    let r_use_star           = re(r"use +[\w]+( *:: *[\w*]+)*");
    let r_pub_use            = re(r"pub +use +[\w]+( *:: *[\w]+)*( +as +[\w]+)?");
    let r_pub_use_star       = re(r"pub +use +[\w]+( *:: *[\w*]+)*");
    let r_module_attrib      = re(r"#!\[[^\]]*\]");
    let r_attrib             = re(r"#\[[^\]]*\]");
    let r_pub_mod            = re(r"pub +mod +[\w]+");
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
    let test = Regex::new(r"^ *#!\[[^\]]*\] *((\r\n?)|\n)").unwrap();
    let str = "#![allow(missing_docs)]\n\n";
    println!("{:?}", test.find(&str).map(|t| t.as_str()));

    let str = fs::read_to_string("./lib/rust/ensogl/core/src/display/scene.rs").unwrap();
    // let str = fs::read_to_string("./lib/rust/web/src/lib.rs").unwrap();
    // let str = "foo\n";
    // println!("{:?}", match_header(&str));

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

    let mut header: Vec<HeaderElement> = header
        .into_iter()
        .rev()
        .skip_while(|t| (t.token == Comment) | (t.token == EmptyLine))
        .collect();
    header.reverse();

    let total_len: usize = header.iter().map(|t| t.as_str().len()).sum();

    println!("??? {} {}", header_len, attrs_len);

    let mut header_map = HashMap::<HeaderToken, Vec<String>>::new();

    for elem in header {
        let attrs: Vec<&str> = elem.attrs.iter().map(|t| t.as_str()).collect();
        let attrs = attrs.join("");
        let entry = format!("{}{}", attrs, elem.as_str());
        header_map.entry(elem.token).or_default().push(entry);
    }

    // TODO: Comment not done
    let mut out = String::new();
    print(&mut out, &mut header_map, &[ModuleDoc]);
    print(&mut out, &mut header_map, &[ModuleAttrib]);
    print(&mut out, &mut header_map, &[PubMod]);
    print(&mut out, &mut header_map, &[CratePubUseStar, PubUseStar]);
    print(&mut out, &mut header_map, &[CratePubUse, PubUse]);
    print(&mut out, &mut header_map, &[CrateUseStar, UseStar]);
    print(&mut out, &mut header_map, &[CrateUse]);
    print(&mut out, &mut header_map, &[Use]);

    out.push_str(&str[total_len..]);
    println!("{}", out);
    // println!("{:#?}", header_map);
}

fn print(out: &mut String, map: &mut HashMap<HeaderToken, Vec<String>>, tokens: &[HeaderToken]) {
    if tokens.iter().map(|t| print_single(out, map, *t)).any(|t| t == true) {
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
