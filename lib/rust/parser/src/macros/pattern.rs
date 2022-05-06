use crate::prelude::*;
use crate::TokenOrAst;



#[derive(Clone, Debug)]
pub enum Pattern {
    Everything,
    Nothing,
    Or(Box<Pattern>, Box<Pattern>),
    Item(Item),
}

#[derive(Clone, Copy, Debug)]
pub struct Item {
    pub has_rhs_spacing: Option<bool>,
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
