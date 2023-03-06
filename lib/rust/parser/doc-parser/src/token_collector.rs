use crate::*;



pub fn parse(input: &str) -> TokenCollector<IgnoredLocation> {
    let mut docs = TokenCollector::default();
    let mut lexer = Lexer::default();
    for (line_number, line) in input.trim_start().lines().enumerate() {
        let location = Location::start_of_line(line_number);
        let line = Span { location, text: line };
        lexer.line::<IgnoredLocation>(line, &mut docs);
    }
    lexer.finish(&mut docs);
    docs
}



// =======================
// === Token Collector ===
// =======================

/// Token consumer that reifies the sequence of tokens for debugging and tests.
#[derive(Default, Debug)]
pub struct TokenCollector<L> {
    pub tokens:    Vec<Token>,
    location_type: std::marker::PhantomData<L>,
}

#[derive(Debug)]
pub enum Token {
    Flag { flag: Flag, description: String },
    EnterMarkedSection { mark: Mark, header: String },
    EnterKeyedSection { header: String },
    Start(ScopeType),
    End(ScopeType),
    StartQuote,
    EndQuote,
    Text(String),
    RawLine(String),
}

impl<L> TokenConsumer<L> for TokenCollector<L> {
    fn flag(&mut self, flag: Flag, description: Option<Span<L>>) {
        self.tokens.push(Token::Flag {
            flag,
            description: description.map(String::from).unwrap_or_default(),
        })
    }

    fn enter_marked_section(&mut self, mark: Mark, header: Option<Span<L>>) {
        self.tokens.push(Token::EnterMarkedSection {
            mark,
            header: header.map(String::from).unwrap_or_default(),
        })
    }

    fn enter_keyed_section(&mut self, header: Span<L>) {
        self.tokens.push(Token::EnterKeyedSection { header: header.into() })
    }

    fn text(&mut self, text: Span<L>) {
        match self.tokens.last_mut() {
            Some(Token::Text(current)) => {
                current.push(' ');
                current.push_str(text.text.as_ref())
            }
            _ => self.tokens.push(Token::Text(text.text.into())),
        }
    }

    fn start_list(&mut self) {
        self.tokens.push(Token::Start(ScopeType::List));
    }

    fn start_list_item(&mut self) {
        self.tokens.push(Token::Start(ScopeType::ListItem));
    }

    fn start_paragraph(&mut self) {
        self.tokens.push(Token::Start(ScopeType::Paragraph));
    }

    fn start_raw(&mut self) {
        self.tokens.push(Token::Start(ScopeType::Raw));
    }

    fn start_quote(&mut self) {
        self.tokens.push(Token::StartQuote);
    }

    fn end_quote(&mut self) {
        self.tokens.push(Token::EndQuote);
    }

    fn whitespace(&mut self) {
        self.tokens.push(Token::Text(" ".to_owned()));
    }

    fn raw_line(&mut self, text: Span<L>) {
        self.tokens.push(Token::RawLine(text.text.into()));
    }

    fn end(&mut self, scope: ScopeType) {
        self.tokens.push(Token::End(scope));
    }
}
