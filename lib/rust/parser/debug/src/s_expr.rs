//! Tree S-expr representation

use enso_metamodel_lexpr::ToSExpr;
use enso_reflect::Reflect;
use lexpr::Value;
use std::collections::HashSet;

/// Produce an S-expression representation of the input AST type.
pub fn to_s_expr<T>(value: &T, code: &str) -> Value
where
    T: serde::Serialize + Reflect,
{
    use enso_parser::syntax::token::variant::*;
    use enso_parser::syntax::tree;
    let (graph, rust_to_meta) = enso_metamodel::rust::to_meta(value.reflect_type());
    let ast_ty = rust_to_meta[&value.reflect_type().id];
    let base = code.as_bytes().as_ptr() as usize;
    let code: Box<str> = Box::from(code);
    let mut to_s_expr = ToSExpr::new(&graph);
    to_s_expr.mapper(ast_ty, strip_hidden_fields);
    let stringish_tokens = vec![
        Digits::reflect(),
        NumberBase::reflect(),
        TextSection::reflect(),
        Operator::reflect(),
        TypeAnnotationOperator::reflect(),
        ArrowOperator::reflect(),
        AutoscopeOperator::reflect(),
        UnaryOperator::reflect(),
        LambdaOperator::reflect(),
        SuspensionOperator::reflect(),
    ];
    let stringish_tokens = stringish_tokens.into_iter().map(|t| rust_to_meta[&t.id]);
    let skip_tokens = vec![
        SuspendedDefaultArguments::reflect(),
        CloseSymbol::reflect(),
        Newline::reflect(),
        OpenSymbol::reflect(),
        TextEnd::reflect(),
        TextStart::reflect(),
        Wildcard::reflect(),
        TypeKeyword::reflect(),
        ForeignKeyword::reflect(),
        CaseKeyword::reflect(),
        OfKeyword::reflect(),
        AnnotationOperator::reflect(),
        AssignmentOperator::reflect(),
        DotOperator::reflect(),
    ];
    skip_tokens.into_iter().for_each(|token| to_s_expr.skip(rust_to_meta[&token.id]));
    let identish_tokens = vec![Ident::reflect(), AllKeyword::reflect(), PrivateKeyword::reflect()];
    let identish_tokens = identish_tokens.into_iter().map(|t| rust_to_meta[&t.id]);
    let text_escape_token = rust_to_meta[&TextEscape::reflect().id];
    let token_to_str = move |token: Value| {
        let range = token_code_range(&token, base);
        if range.is_empty() { "".into() } else { code[range].to_owned().into_boxed_str() }
    };
    for token in identish_tokens {
        let token_to_str_ = token_to_str.clone();
        to_s_expr.mapper(token, move |token| Value::symbol(token_to_str_(token)));
    }
    for token in stringish_tokens {
        let token_to_str_ = token_to_str.clone();
        to_s_expr.mapper(token, move |token| Value::string(token_to_str_(token)));
    }
    let into_car = |cons| match cons {
        Value::Cons(cons) => cons.into_pair().0,
        _ => panic!(),
    };
    let simplify_escape = |mut list| {
        let mut last = None;
        while let Value::Cons(cons) = list {
            let (car, cdr) = cons.into_pair();
            last = Some(car);
            list = cdr;
        }
        last.unwrap()
    };
    let strip_invalid = |list| {
        let Value::Cons(cons) = list else { unreachable!() };
        let (car, _) = cons.into_pair();
        Value::cons(car, Value::Null)
    };
    let line = rust_to_meta[&tree::block::Line::reflect().id];
    let operator_line = rust_to_meta[&tree::block::OperatorLine::reflect().id];
    let type_signature_line = rust_to_meta[&tree::TypeSignatureLine::reflect().id];
    let invalid = rust_to_meta[&tree::Invalid::reflect().id];
    let tree = rust_to_meta[&tree::Tree::reflect().id];
    to_s_expr.mapper(line, into_car);
    to_s_expr.mapper(operator_line, into_car);
    to_s_expr.mapper(type_signature_line, into_car);
    to_s_expr.mapper(invalid, strip_invalid);
    to_s_expr.mapper(text_escape_token, simplify_escape);
    to_s_expr.mapper(tree, strip_hidden_fields);
    tuplify(to_s_expr.value(ast_ty, &value))
}

/// Strip fields that are not useful to a human reader, like source-code offsets.
fn strip_hidden_fields(tree: Value) -> Value {
    let hidden_tree_fields = [
        ":spanLeftOffsetVisible",
        ":spanLeftOffsetCodeReprBegin",
        ":spanLeftOffsetCodeReprLen",
        ":spanLeftOffsetCodeLenUtf8",
        ":spanLeftOffsetCodeLenUtf16",
        ":spanLeftOffsetCodeLenNewlines",
        ":spanLeftOffsetCodeLenLineChars16",
        ":spanLeftOffsetCodeStartUtf8",
        ":spanLeftOffsetCodeStartUtf16",
        ":spanLeftOffsetCodeStartLine",
        ":spanLeftOffsetCodeStartCol16",
        ":spanCodeLengthUtf8",
        ":spanCodeLengthUtf16",
        ":spanCodeLengthNewlines",
        ":spanCodeLengthLineChars16",
        ":warnings",
    ];
    let hidden_tree_fields: HashSet<_> = hidden_tree_fields.into_iter().collect();
    Value::list(tree.to_vec().unwrap().into_iter().filter(|val| match val {
        Value::Cons(cons) => match cons.car() {
            Value::Symbol(symbol) => !hidden_tree_fields.contains(symbol.as_ref()),
            _ => panic!(),
        },
        _ => true,
    }))
}

/// Given an S-expression representation of a [`Token`] and the base address for `Code` `Cow`s,
/// return the range of the input code the token references.
fn token_code_range(token: &Value, base: usize) -> std::ops::Range<usize> {
    let get_u32 =
        |field| fields(token).find(|(name, _)| *name == field).unwrap().1.as_u64().unwrap() as u32;
    let begin = get_u32(":codeReprBegin");
    let len = get_u32(":codeReprLen");
    if len == 0 {
        return 0..0;
    }
    let begin = (begin as u64) | (base as u64 & !(u32::MAX as u64));
    let begin = if begin < (base as u64) { begin + 1 << 32 } else { begin };
    let begin = begin as usize - base;
    let len = len as usize;
    begin..(begin + len)
}

/// Iterate the field `(name, value)` pairs of the S-expression of a struct with named fields.
fn fields(value: &'_ Value) -> impl Iterator<Item = (&'_ str, &'_ Value)> {
    value.list_iter().unwrap().filter_map(|value| match value {
        Value::Cons(cons) => match cons.car() {
            Value::Symbol(symbol) => Some((&symbol[..], cons.cdr())),
            _ => None,
        },
        _ => None,
    })
}

/// Strip field names from struct representations, so that they are printed more concisely, as if
/// they were tuple-structs.
fn tuplify(value: Value) -> Value {
    let (car, cdr) = match value {
        Value::Cons(cons) => cons.into_pair(),
        Value::Vector(mut vector) => {
            for value in vector.iter_mut() {
                let original = std::mem::replace(value, Value::Nil);
                *value = tuplify(original);
            }
            return Value::Vector(vector);
        }
        value => return value,
    };
    if let Value::Symbol(symbol) = &car {
        if let Some(':') = symbol.chars().next() {
            return tuplify(cdr);
        }
    }
    let car = tuplify(car);
    let cdr = tuplify(cdr);
    Value::Cons(lexpr::Cons::new(car, cdr))
}
