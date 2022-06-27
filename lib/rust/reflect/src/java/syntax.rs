use std::fmt;

const TARGET_VERSION: usize = 14;
//const TARGET_VERSION: usize = 15;



// ===================
// === Syntax Data ===
// ===================

#[derive(Debug)]
pub struct Class {
    pub package:   Option<String>,
    pub name:      String,
    pub abstract_: bool,
    pub final_:    bool,
    pub static_:   bool,
    pub parent:    Option<Type>,
    pub fields:    Vec<Field>,
    pub methods:   Vec<Method>,
    pub nested:    Vec<Class>,
    pub sealed:    Option<Vec<Type>>,
}

#[derive(Debug)]
pub struct Field {
    pub type_:  Type,
    pub name:   String,
    pub final_: bool,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Type {
    pub class:  String,
    pub params: Vec<String>,
}

impl Type {
    pub fn named(name: impl Into<String>) -> Self {
        let class = name.into();
        let params = vec![];
        Type { class, params }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Method {
    pub name:      String,
    pub arguments: Vec<(Type, String)>,
    pub return_:   Option<Type>,
    pub static_:   bool,
    pub final_:    bool,
    pub body:      String,
    pub override_: bool,
    pub throws:    Vec<Type>,
}


// === Constructors ===

impl Method {
    pub fn new(name: impl Into<String>, return_: Type) -> Self {
        let name = name.into();
        let return_ = Some(return_);
        let arguments = Default::default();
        let static_ = Default::default();
        let final_ = Default::default();
        let body = Default::default();
        let override_ = Default::default();
        let throws = Default::default();
        Method { name, arguments, return_, static_, final_, body, override_, throws }
    }

    pub fn constructor(name: impl Into<String>) -> Self {
        let name = name.into();
        let arguments = Default::default();
        let return_ = Default::default();
        let static_ = Default::default();
        let final_ = Default::default();
        let body = Default::default();
        let override_ = Default::default();
        let throws = Default::default();
        Method { name, arguments, return_, static_, final_, body, override_, throws }
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
        let mut tokens = vec!["protected".to_string()];
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
        let Method { name, arguments, return_, static_, final_, body, override_, throws } = &self;
        let mut tokens = vec![];
        override_.then(|| tokens.push("@Override".to_string()));
        tokens.push("public".to_string());
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
