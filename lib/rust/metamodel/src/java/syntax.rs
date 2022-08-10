//! Java syntax.

use std::fmt;



const TARGET_VERSION: usize = 14;



// ===================
// === Syntax Data ===
// ===================

/// A class definition.
#[derive(Debug)]
pub struct Class {
    #[allow(missing_docs)]
    pub package:   Option<String>,
    #[allow(missing_docs)]
    pub name:      String,
    #[allow(missing_docs)]
    pub abstract_: bool,
    #[allow(missing_docs)]
    pub final_:    bool,
    #[allow(missing_docs)]
    pub static_:   bool,
    #[allow(missing_docs)]
    pub parent:    Option<Type>,
    #[allow(missing_docs)]
    pub fields:    Vec<Field>,
    #[allow(missing_docs)]
    pub methods:   Vec<Method>,
    #[allow(missing_docs)]
    pub sealed:    Option<Vec<Type>>,
    /// Classes defined in the scope of this class.
    pub nested:    Vec<Class>,
}

/// A class field definition.
#[derive(Debug)]
pub struct Field {
    #[allow(missing_docs)]
    pub type_:  Type,
    #[allow(missing_docs)]
    pub name:   String,
    #[allow(missing_docs)]
    pub final_: bool,
}

/// Identifies a type; this corresponds to `UnannType`[1] in the Java specification.
/// It is suitable for use as the type portion of a field declaration, local variable declaration,
/// formal parameter, or return type specification.
///
/// [1]: https://docs.oracle.com/javase/specs/jls/se18/html/jls-8.html#jls-UnannType
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Type {
    /// Class name.
    pub class:  String,
    /// Parameter list.
    pub params: Vec<String>,
}

impl Type {
    /// A simple type.
    pub fn named(name: impl Into<String>) -> Self {
        let class = name.into();
        let params = vec![];
        Type { class, params }
    }

    /// A generic type.
    pub fn generic(name: impl Into<String>, params: Vec<String>) -> Self {
        let class = name.into();
        Type { class, params }
    }
}

/// A method.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Method {
    #[allow(missing_docs)]
    pub name:       String,
    #[allow(missing_docs)]
    pub arguments:  Vec<(Type, String)>,
    /// Visibility modifier; if None, the Java default is package-visible.
    pub visibility: Option<Visibility>,
    /// Return value, unless this is a constructor.
    pub return_:    Option<Type>,
    #[allow(missing_docs)]
    pub static_:    bool,
    #[allow(missing_docs)]
    pub final_:     bool,
    /// Literal body, not including brackets.
    pub body:       String,
    #[allow(missing_docs)]
    pub override_:  bool,
    #[allow(missing_docs)]
    pub throws:     Vec<Type>,
}

/// Java visibility modifier keyword for a variable or method.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Visibility {
    #[allow(missing_docs)]
    Private,
    #[allow(missing_docs)]
    Protected,
    #[allow(missing_docs)]
    Public,
}


// === Constructors ===

impl Method {
    /// Create a method.
    pub fn new(name: impl Into<String>, return_: Type) -> Self {
        let name = name.into();
        let return_ = Some(return_);
        let arguments = Default::default();
        let static_ = Default::default();
        let final_ = Default::default();
        let body = Default::default();
        let override_ = Default::default();
        let throws = Default::default();
        let visibility = Some(Visibility::Public);
        Method { name, arguments, return_, static_, final_, body, override_, throws, visibility }
    }

    /// Create a constructor.
    pub fn constructor(name: impl Into<String>) -> Self {
        let name = name.into();
        let arguments = Default::default();
        let return_ = Default::default();
        let static_ = Default::default();
        let final_ = Default::default();
        let body = Default::default();
        let override_ = Default::default();
        let throws = Default::default();
        let visibility = None;
        Method { name, arguments, return_, static_, final_, body, override_, throws, visibility }
    }
}



// =========================
// === Rendering to Text ===
// =========================

impl fmt::Display for Class {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Class {
            package,
            name,
            abstract_,
            final_,
            static_,
            parent,
            fields,
            methods,
            nested,
            sealed,
        } = &self;
        if let Some(package) = package {
            writeln!(f, "package {};", package)?;
        }
        let mut modifiers = vec!["public".to_string()];
        static_.then(|| modifiers.push("static".to_string()));
        final_.then(|| modifiers.push("final".to_string()));
        abstract_.then(|| modifiers.push("abstract".to_string()));
        if TARGET_VERSION >= 15 && sealed.is_some() {
            modifiers.push("sealed".to_string())
        }
        let mut tokens = modifiers;
        tokens.push("class".to_string());
        tokens.push(name.to_string());
        if let Some(parent) = parent {
            tokens.push("extends".to_string());
            tokens.push(parent.to_string());
        }
        if let Some(sealed) = sealed {
            if !sealed.is_empty() {
                let types: Vec<_> = sealed.iter().map(|ty| ty.to_string()).collect();
                tokens.push(format!("permits {}", types.join(", ")));
            }
        }
        let tokens = tokens.join(" ");
        writeln!(f, "{} {{", tokens)?;
        for field in fields {
            write!(f, "{}", field)?;
        }
        for method in methods {
            write!(f, "{}", method)?;
        }
        for class in nested {
            write!(f, "{}", class)?;
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}

impl fmt::Display for Field {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Field { type_, name, final_ } = &self;
        let mut tokens = vec![];
        final_.then(|| tokens.push("final".to_string()));
        tokens.push(type_.to_string());
        tokens.push(name.clone());
        let tokens = tokens.join(" ");
        writeln!(f, "{};", tokens)
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", &self.class)?;
        if !self.params.is_empty() {
            write!(f, "<{}>", self.params.join(", "))?;
        }
        Ok(())
    }
}

impl fmt::Display for Method {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Method {
            name,
            arguments,
            return_,
            static_,
            final_,
            body,
            override_,
            throws,
            visibility,
        } = &self;
        let mut tokens = vec![];
        override_.then(|| tokens.push("@Override".to_string()));
        if let Some(visibility) = visibility {
            tokens.push(visibility.to_string());
        }
        static_.then(|| tokens.push("static".to_string()));
        final_.then(|| tokens.push("final".to_string()));
        if let Some(return_) = return_ {
            tokens.push(return_.to_string());
        }
        tokens.push(name.to_string());
        let tokens = tokens.join(" ");
        let arguments: Vec<_> =
            arguments.iter().map(|(ty, name)| format!("{} {}", ty, name)).collect();
        let arguments = arguments.join(", ");
        writeln!(f, "{}({})", tokens, arguments)?;
        if !throws.is_empty() {
            let types: Vec<_> = throws.iter().map(|ty| ty.to_string()).collect();
            let types = types.join(", ");
            writeln!(f, "throws {types}")?;
        }
        writeln!(f, "{{")?;
        writeln!(f, "{body}")?;
        writeln!(f, "}}")?;
        Ok(())
    }
}

impl fmt::Display for Visibility {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Visibility::Private => "private",
            Visibility::Protected => "protected",
            Visibility::Public => "public",
        })
    }
}
