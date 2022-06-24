use std::fmt::Formatter;

const TARGET_VERSION: usize = 15;

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
    pub fn named(name: &str) -> Self {
        let class = name.to_string();
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
    pub body:      Body,
    pub override_: bool,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Body {
    Verbatim(Vec<String>),
    //Block(Block),
}

impl Method {
    pub fn new(name: String, return_: Type) -> Self {
        let return_ = Some(return_);
        let arguments = Default::default();
        let static_ = Default::default();
        let final_ = Default::default();
        let body = Body::Verbatim(Default::default());
        let override_ = Default::default();
        Method { name, arguments, return_, static_, final_, body, override_ }
    }

    pub fn constructor(name: String) -> Self {
        let arguments = Default::default();
        let return_ = Default::default();
        let static_ = Default::default();
        let final_ = Default::default();
        let body = Body::Verbatim(Default::default());
        let override_ = Default::default();
        Method { name, arguments, return_, static_, final_, body, override_ }
    }
}

/*
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Block {
    statements: Vec<Statement>,
}

impl std::fmt::Display for Block {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for statement in &self.statements {
            write!(f, "{}", statement)?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Statement {
    Bind { type_: String, name: String, value: Expr },
    Assign { location: Location, value: Expr },
    Expr(Expr),
    If { condition: Expr, if_: Block, else_: Option<Block> },
    Return(Expr),
}

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Bind { type_, name, value } => writeln!(f, "{} {} = {};", type_, name, value),
            Statement::Assign { location, value } => writeln!(f, "{} = {};", location, value),
            Statement::Expr(expr) => writeln!(f, "{};", expr),
            Statement::If { condition, if_, else_ } => {
                writeln!(f, "if ({}) {{", condition)?;
                writeln!(f, "{}", if_)?;
                write!(f, "}}")?;
                if let Some(else_) = else_ {
                    writeln!(f, " else {{")?;
                    writeln!(f, "{}", else_)?;
                    writeln!(f, "}}")?;
                } else {
                    writeln!(f)?;
                }
                Ok(())
            }
            Statement::Return(expr) => writeln!(f, "return {};", expr),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    Apply(Location, Vec<Expr>),
    Value(Location),
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Apply(fun, args) => {
                let args: Vec<_> = args.into_iter().map(|arg| arg.to_string()).collect();
                let args = args.join(", ");
                write!(f, "{}({})", fun, args)
            },
            Expr::Value(value) => write!(f, "{}", value),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Location {
    components: Vec<String>,
}

impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.components.join("."))
    }
}
 */

impl std::fmt::Display for Class {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
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

impl std::fmt::Display for Field {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Field { type_, name, final_ } = &self;
        let mut tokens = vec!["protected".to_string()];
        final_.then(|| tokens.push("final".to_string()));
        tokens.push(type_.to_string());
        tokens.push(name.clone());
        let tokens = tokens.join(" ");
        writeln!(f, "{};", tokens)
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &self.class)?;
        if !self.params.is_empty() {
            write!(f, "<{}>", self.params.join(", "))?;
        }
        Ok(())
    }
}

impl std::fmt::Display for Method {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Method { name, arguments, return_, static_, final_, body, override_ } = &self;
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
        writeln!(f, "{}({}) {{", tokens, arguments)?;
        match body {
            Body::Verbatim(body) =>
                for statement in body {
                    writeln!(f, "{}", statement)?;
                }, // Body::Block(block) => { writeln!(f, "{}", block)?; }
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}
