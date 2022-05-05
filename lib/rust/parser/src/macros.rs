use crate::prelude::*;
use crate::Ast;
use crate::Lexer;
use crate::Token;
use crate::TokenOrAst;
use crate::TokenOrAstOrMacroResolver;
use enso_data_structures::im_list;

#[derive(Clone, Copy, Debug)]
pub struct Item {
    pub has_rhs_spacing: Option<bool>,
}

#[derive(Clone, Debug)]
pub enum Pattern {
    Everything,
    Nothing,
    Or(Box<Pattern>, Box<Pattern>),
    Item(Item),
    // TokenVariant(lexer::KindVariant),
    // Seq(Box<Pattern>, Box<Pattern>),
}

pub struct Error<T> {
    tokens:  Vec<T>,
    message: String,
}

impl<T> Debug for Error<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("Pattern match error")
    }
}

impl<T> Error<T> {
    pub fn new(tokens: Vec<T>, message: impl Into<String>) -> Self {
        let message = message.into();
        Self { tokens, message }
    }
}

impl Pattern {
    pub fn resolve<T: TryAsRef<TokenOrAst>>(&self, input: Vec<T>) -> (Vec<T>, Vec<T>) {
        match self {
            Self::Everything => (input, default()),
            Self::Nothing => (default(), input),
            _ => todo!(),
        }
    }

    pub fn resolve2<T: TryAsRef<TokenOrAst>>(
        &self,
        mut input: Vec<T>,
        has_spacing: bool,
    ) -> Result<(Vec<T>, Vec<T>), Error<T>> {
        match self {
            Self::Everything => Ok((input, default())),
            Self::Nothing => Ok((default(), input)),
            Self::Or(fst, snd) => fst
                .resolve2(input, has_spacing)
                .or_else(|err| snd.resolve2(err.tokens, has_spacing)),
            Self::Item(item) => match input.first() {
                None => Err(Error::new(input, "Expected an item.")),
                Some(first) => match first.try_as_ref() {
                    None => Err(Error::new(input, "Expected an item.")),
                    Some(first) => match item.has_rhs_spacing {
                        Some(spacing) =>
                            if spacing == has_spacing {
                                Ok((vec![input.pop_front().unwrap()], input))
                            } else {
                                Err(Error::new(input, "Expected an item."))
                            },
                        None => Ok((vec![input.pop_front().unwrap()], input)),
                    },
                },
            },
        }
    }
}



#[derive(Derivative)]
#[derivative(Debug)]
pub struct Definition<'a> {
    pub rev_prefix_pattern: Option<Pattern>,
    pub segments:           im_list::NonEmpty<SegmentDefinition<'a>>,
    #[derivative(Debug = "ignore")]
    pub body: Rc<
        dyn for<'b> Fn(
            &Lexer<'b>,
            Option<Vec<TokenOrAst>>,
            NonEmptyVec<(Token, Vec<TokenOrAst>)>,
        ) -> Ast,
    >,
}



// =========================
// === SegmentDefinition ===
// =========================

#[derive(Clone, Debug)]
pub struct SegmentDefinition<'a> {
    pub header:  &'a str,
    pub pattern: Pattern,
}

impl<'a> SegmentDefinition<'a> {
    pub fn new(header: &'a str, pattern: Pattern) -> Self {
        Self { header, pattern }
    }
}



// ===================
// === Rust Macros ===
// ===================

#[macro_export]
macro_rules! macro_definition {
    ( ($($section:literal, $pattern:expr),* $(,)?) $body:expr ) => {
        $crate::macro_definition!{[None] ($($section, $pattern),*) $body}
    };
    ( ($prefix:expr, $($section:literal, $pattern:expr),* $(,)?) $body:expr ) => {
        $crate::macro_definition!{[Some($prefix)] ($($section, $pattern),*) $body}
    };
    ( [$prefix:expr] ($($section:literal, $pattern:expr),* $(,)?) $body:expr ) => {
        macros::Definition {
            rev_prefix_pattern: $prefix,
            segments: im_list::NonEmpty::try_from(vec![
                $(macros::SegmentDefinition::new($section, $pattern)),*]).unwrap(),
            body: Rc::new($body),
        }
    };
}
