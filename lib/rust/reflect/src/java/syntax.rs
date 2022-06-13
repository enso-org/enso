use std::fmt::Formatter;

#[derive(Debug)]
pub struct Class {
    pub name:      String,
    pub abstract_: bool,
    pub final_:    bool,
    pub static_:   bool,
    pub parent:    Option<String>,
    pub fields:    Vec<Field>,
    pub methods:   Vec<Method>,
    pub nested:    Vec<Class>,
}

#[derive(Debug)]
pub struct Field {
    pub class:  String,
    pub params: Vec<String>,
    pub name:   String,
    pub final_: bool,
}

#[derive(Debug)]
pub struct Method {
    pub name:      String,
    pub arguments: Vec<(String, String)>,
    pub return_:   String,
    pub static_:   bool,
    pub final_:    bool,
    //pub body:    Block,
    pub body:      Vec<String>,
}

/*
#[derive(Debug)]
pub struct Block {
    statements: Vec<Statement>,
    return_: Option<Expr>,
}

#[derive(Debug)]
pub enum Statement {
    Bind { type_: String, name: String, value: Expr },
    Assign { location: Location, value: Expr },
    Expr(Expr),
    If { condition: Expr, if_: Block, else_: Option<Block> },
}

#[derive(Debug)]
pub enum Expr {
    Apply(Location, vec![Expr]),
    Value(Location),
}

#[derive(Debug)]
pub struct Location {
    components: Vec<String>,
}
 */

impl std::fmt::Display for Class {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Class { name, abstract_, final_, static_, parent, fields, methods, nested } = &self;
        let mut modifiers = vec!["public"];
        static_.then(|| modifiers.push("static"));
        final_.then(|| modifiers.push("final"));
        abstract_.then(|| modifiers.push("abstract"));
        let mut tokens = modifiers;
        tokens.push("class");
        tokens.push(&name);
        if let Some(parent) = parent {
            tokens.push("extends");
            tokens.push(parent);
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
        let Field { class, params, name, final_ } = &self;
        let mut tokens = vec!["public".to_string()];
        final_.then(|| tokens.push("final".to_string()));
        tokens.push(if params.is_empty() {
            class.clone()
        } else {
            format!("{}<{}>", class.clone(), params.join(", "))
        });
        tokens.push(name.clone());
        let tokens = tokens.join(" ");
        writeln!(f, "{};", tokens)
    }
}

impl std::fmt::Display for Method {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let Method { name, arguments, return_, static_, final_, body } = &self;
        let mut tokens = vec!["public"];
        static_.then(|| tokens.push("static"));
        final_.then(|| tokens.push("final"));
        tokens.push(return_);
        tokens.push(&name);
        let tokens = tokens.join(" ");
        let arguments: Vec<_> =
            arguments.iter().map(|(ty, name)| format!("{} {}", ty, name)).collect();
        let arguments = arguments.join(", ");
        writeln!(f, "{}({}) {{", tokens, arguments)?;
        for statement in body {
            writeln!(f, "{}", statement)?;
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}
