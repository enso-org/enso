use crate::*;

// ======================
// === Token Literals ===
// =====================

/// Token representing blank.
pub const BLANK_TOKEN:char = '_';

/// Symbol appearing after base of the number literal.
pub const NUMBER_BASE_SEPARATOR:char = '_';

/// Suffix to made a modifier from an operator
pub const MOD_SUFFIX:char = '=';

/// Symbol enclosing raw Text line.
pub const FMT_QUOTE:char = '\'';

/// Symbol enclosing formatted Text line.
pub const RAW_QUOTE:char = '"';

/// Symbol used to break lines in Text block.
pub const NEWLINE:char = '\n';

/// Symbol introducing escape segment in the Text.
pub const BACKSLASH:char = '\\';

/// Symbol enclosing expression segment in the formatted Text.
pub const EXPR_QUOTE:char = '`';

/// Symbol that introduces UTF-16 code in the formatted Text segment.
pub const UNICODE16_INTRODUCER:char = 'u';

/// String that opens "UTF-21" code in the formatted Text segment.
pub const UNICODE21_OPENER:&str = "u{";

/// String that closese "UTF-21" code in the formatted Text segment.
pub const UNICODE21_CLOSER:&str = "}";

/// Symbol that introduces UTF-16 code in the formatted Text segment.
pub const UNICODE32_INTRODUCER:char = 'U';

/// Quotes opening block of the raw text.
pub const RAW_BLOCK_QUOTES:&'static str = "\"\"\"";

/// Quotes opening block of the formatted text.
pub const FMT_BLOCK_QUOTES:&'static str = "'''";



// ===============
// === Builder ===
// ===============

make_repr_span!(Empty);
make_repr_span!(Letter, self.char);
make_repr_span!(Space , self);
make_repr_span!(Text  , self.str);
make_repr_span!(Seq   , self.first, self.second);



// =====================
// === TextBlockLine ===
// =====================

/// Not an instance of `HasSpan`, as it needs to know parent block's offset.
impl<T: HasSpan> TextBlockLine<T> {
    fn span(&self, block_offset:usize) -> usize {
        let line_count              = self.empty_lines.len() + 1;
        let empty_lines_space:usize = self.empty_lines.iter().sum();
        let line_breaks             = line_count * NEWLINE.span();
        empty_lines_space + line_breaks + block_offset + self.text.span()
    }
}

impl<T: HasRepr> TextBlockLine<T> {
    fn write_repr(&self, target:&mut String, block_offset:usize) {
        for empty_line_spaces in &self.empty_lines {
            (NEWLINE,empty_line_spaces).write_repr(target);
        }
        (NEWLINE,block_offset,&self.text).write_repr(target);
    }
}



// =====================
// === Text Segments ===
// =====================

make_repr_span!(SegmentPlain    ,             self.value);
make_repr_span!(SegmentRawEscape, BACKSLASH,  self.code );
make_repr_span!(SegmentExpr<T>  , EXPR_QUOTE, self.value, EXPR_QUOTE);
make_repr_span!(SegmentEscape   , BACKSLASH,  self.code );



// =================
// === RawEscape ===
// =================

make_repr_span!(Unfinished);
make_repr_span!(Invalid , self.str );
make_repr_span!(Slash   , BACKSLASH);
make_repr_span!(Quote   , FMT_QUOTE);
make_repr_span!(RawQuote, RAW_QUOTE);



// ==============
// === Escape ===
// ==============

make_repr_span!(EscapeCharacter , self.c     );
make_repr_span!(EscapeControl   , self.name  );
make_repr_span!(EscapeNumber    , self.digits);
make_repr_span!(EscapeUnicode16 , UNICODE16_INTRODUCER, self.digits);
make_repr_span!(EscapeUnicode21 , UNICODE21_OPENER    , self.digits
                                , UNICODE21_CLOSER);
make_repr_span!(EscapeUnicode32 , UNICODE32_INTRODUCER, self.digits);



// =============
// === Block ===
// =============

make_repr_span!(BlockLine<T>, self.elem, self.off);



// =============
// === Macro ===
// =============

// === Macro Segments ==

make_repr_span!(MacroMatchSegment<T> , self.head, self.body);
make_repr_span!(MacroAmbiguousSegment, self.head, self.body);


// === MacroPatternMatch subtypes ===

make_repr_span!(MacroPatternMatchRawBegin  );
make_repr_span!(MacroPatternMatchRawEnd    );
make_repr_span!(MacroPatternMatchRawNothing);
make_repr_span!(MacroPatternMatchRawSeq    <T>, self.elem);
make_repr_span!(MacroPatternMatchRawOr     <T>, self.elem);
make_repr_span!(MacroPatternMatchRawMany   <T>, self.elem);
make_repr_span!(MacroPatternMatchRawExcept <T>, self.elem);
make_repr_span!(MacroPatternMatchRawBuild  <T>, self.elem);
make_repr_span!(MacroPatternMatchRawErr    <T>, self.elem);
make_repr_span!(MacroPatternMatchRawTag    <T>, self.elem);
make_repr_span!(MacroPatternMatchRawCls    <T>, self.elem);
make_repr_span!(MacroPatternMatchRawTok    <T>, self.elem);
make_repr_span!(MacroPatternMatchRawBlank  <T>, self.elem);
make_repr_span!(MacroPatternMatchRawVar    <T>, self.elem);
make_repr_span!(MacroPatternMatchRawCons   <T>, self.elem);
make_repr_span!(MacroPatternMatchRawOpr    <T>, self.elem);
make_repr_span!(MacroPatternMatchRawMod    <T>, self.elem);
make_repr_span!(MacroPatternMatchRawNum    <T>, self.elem);
make_repr_span!(MacroPatternMatchRawText   <T>, self.elem);
make_repr_span!(MacroPatternMatchRawBlock  <T>, self.elem);
make_repr_span!(MacroPatternMatchRawMacro  <T>, self.elem);
make_repr_span!(MacroPatternMatchRawInvalid<T>, self.elem);


// === Switch ===

make_repr_span!(Switch<T>, self.get());


// === Shifted ===

make_repr_span!(Shifted<T>, self.off, self.wrapped);
make_repr_span!(ShiftedVec1<T>, self.head, self.tail);



// =============================================================================
// === Shape ===================================================================
// =============================================================================

// ===============
// === Invalid ===
// ===============

make_repr_span!(Unrecognized, self.str  );
make_repr_span!(InvalidQuote, self.quote);
make_repr_span!(InlineBlock , self.quote);



// ===================
// === Identifiers ===
// ===================

make_repr_span!(Blank           , BLANK_TOKEN);
make_repr_span!(Var             , self.name  );
make_repr_span!(Cons            , self.name  );
make_repr_span!(Opr             , self.name  );
make_repr_span!(Mod             , self.name, MOD_SUFFIX );
make_repr_span!(InvalidSuffix<T>, self.elem, self.suffix);



// ==============
// === Number ===
// ==============

/// Helper to represent that optional number base has additional character.
struct NumberBase<T>(T);
make_repr_span!(NumberBase<T>, self.0, NUMBER_BASE_SEPARATOR);
make_repr_span!(Number       , self.base.as_ref().map(|b| NumberBase(b))
                             , self.int);
make_repr_span!(DanglingBase , self.base, NUMBER_BASE_SEPARATOR);



// ============
// === Text ===
// ============

// === Indented ===

/// Helper to represent line with additional spacing prepended.
struct Indented<T>(usize,T);
make_repr_span!(Indented<T>, self.0, self.1);
impl<T> Block<T> {
    fn indented<'t, U>(&self, t:&'t U) -> Indented<&'t U> {
        Indented(self.indent,t)
    }
}


// === Lines ===

make_repr_span!(TextLineRaw     , RAW_QUOTE, self.text, RAW_QUOTE);
make_repr_span!(TextLineFmt<T>  , FMT_QUOTE, self.text, FMT_QUOTE);


// === TextBlockRaw ==

impl HasSpan for TextBlockRaw {
    fn span(&self) -> usize {
        let mut acc = (RAW_BLOCK_QUOTES,self.spaces).span();
        for line in self.text.iter() {
            acc += line.span(self.offset);
        }
        acc
    }
}

impl HasRepr for TextBlockRaw {
    fn write_repr(&self, target:&mut String) {
        (RAW_BLOCK_QUOTES, self.spaces).write_repr(target);
        for line in self.text.iter() {
            line.write_repr(target, self.offset);
        }
    }
}


// === TextBlockFmt ==

impl<T: HasSpan> HasSpan for TextBlockFmt<T> {
    fn span(&self) -> usize {
        let lines            = self.text.iter();
        let line_spans       = lines.map(|line| line.span(self.offset));
        let lines_span:usize = line_spans.sum();
        FMT_BLOCK_QUOTES.span() + self.spaces + lines_span
    }
}

impl<T: HasRepr> HasRepr for TextBlockFmt<T> {
    fn write_repr(&self, target:&mut String) {
        (FMT_BLOCK_QUOTES,self.spaces).write_repr(target);
        for line in self.text.iter() {
            line.write_repr(target,self.offset);
        };
    }
}


// === TextUnclosed ==

// TODO: [mwu] `TextUnclosed<T>` as it needs to cut off closing quote from the
//             stored text line. Likely this type should be stored like this.
impl<T: HasSpan> HasSpan for TextUnclosed<T> {
    fn span(&self) -> usize {
        self.line.span() - 1 // remove missing quote
    }
}

impl<T: HasRepr> HasRepr for TextUnclosed<T> {
    fn write_repr(&self, target:&mut String) {
        self.line.write_repr(target);
        target.pop();  // remove missing quote
    }
}



// ====================
// === Applications ===
// ====================

make_repr_span!(Prefix      <T>, self.func, self.off, self.arg);
make_repr_span!(Infix       <T>, self.larg, self.loff, self.opr, self.roff
                               , self.rarg);
make_repr_span!(SectionLeft <T>, self.arg,  self.off, self.opr);
make_repr_span!(SectionRight<T>, self.opr,  self.off, self.arg);
make_repr_span!(SectionSides<T>, self.opr);



// ==============
// === Module ===
// ==============

// === Module ==

impl<T: HasSpan> HasSpan for Module<T> {
    fn span(&self) -> usize {
        assert!(self.lines.len() > 0);
        let break_count = self.lines.len() - 1;
        let breaks_span = break_count * NEWLINE.span();
        let lines_span  = self.lines.span();
        lines_span + breaks_span
    }
}

impl<T: HasRepr> HasRepr for Module<T> {
    fn write_repr(&self, target:&mut String) {
        let mut iter = self.lines.iter();
        if let Some(first_line) = iter.next() {
            first_line.write_repr(target)
        }
        for line in iter {
            (NEWLINE,line).write_repr(target)
        }
    }
}


// === Block ==

impl<T: HasSpan> HasSpan for Block<T> {
    fn span(&self) -> usize {
        let line_span = |line:&BlockLine<Option<T>>| {
            let indent = line.elem.as_ref().map_or(0, |_| self.indent);
            NEWLINE.span() + indent + line.span()
        };
        let head_span   = if self.is_orphan { 0 } else { 1 };
        let empty_lines = self.empty_lines.span() + self.empty_lines.len();
        let first_line  = self.indent + self.first_line.span();
        let lines       = self.lines.iter().map(line_span).sum::<usize>();
        head_span + empty_lines + first_line + lines
    }
}

impl<T: HasRepr> HasRepr for Block<T> {
    fn write_repr(&self, target:&mut String) {
        (!self.is_orphan).as_some(NEWLINE).write_repr(target);
        for empty_line_space in &self.empty_lines {
            (empty_line_space,NEWLINE).write_repr(target);
        }
        self.indented(&self.first_line).write_repr(target);
        for line in &self.lines {
            (NEWLINE,self.indented(line)).write_repr(target);
        }
    }
}



// ==============
// === Macros ===
// ==============

// === Match ==

make_span!(Match<T>, self.pfx, self.segs);

impl<T: HasRepr> HasRepr for Match<T> {
    fn write_repr(&self, target:&mut String) {
        for pat_match in &self.pfx {
            for sast in pat_match.iter() {
                // reverse the order for prefix: ast before spacing
                (&sast.wrapped,&sast.off).write_repr(target);
            }
        }
        self.segs.write_repr(target);
    }
}


// === Ambiguous ===

make_repr_span!(Ambiguous, self.segs);



// =====================
// === Spaceless AST ===
// =====================

not_supported_repr!(Comment);
not_supported_repr!(Import<T>);
not_supported_repr!(Mixfix<T>);
not_supported_repr!(Group<T>);
not_supported_repr!(Def<T>);
not_supported_repr!(Foreign);



// =============
// === Tests ===
// =============

/// Tests for spacelesss AST. Other AST is covered by parsing tests that verify
/// that correct spans and text representation are generated. Only spaceless AST
/// is not returned by the parser and can't be covered in this way.
#[cfg(test)]
mod tests {
    use super::*;

    // === Comment ===

    fn make_comment() -> Shape<Ast> {
        Comment {lines:vec![]}.into()
    }

    #[test]
    #[should_panic]
    fn comment_panics_on_repr() {
        make_comment().repr();
    }

    #[test]
    #[should_panic]
    fn comment_panics_on_span() {
        make_comment().span();
    }


    // === Import ===

    fn make_import() -> Shape<Ast> {
        Import {path : vec![]}.into()
    }

    #[test]
    #[should_panic]
    fn import_panics_on_repr() {
        make_import().repr();
    }

    #[test]
    #[should_panic]
    fn import_panics_on_span() {
        make_import().span();
    }


    // === Mixfix ===

    fn make_mixfix() -> Shape<Ast> {
        Mixfix {
            name : vec![],
            args : vec![]
        }.into()
    }

    #[test]
    #[should_panic]
    fn mixfix_panics_on_repr() {
        make_mixfix().repr();
    }

    #[test]
    #[should_panic]
    fn mixfix_panics_on_span() {
        make_mixfix().span();
    }


    // === Group ===

    fn make_group() -> Shape<Ast> {
        Group {body : None}.into()
    }

    #[test]
    #[should_panic]
    fn group_panics_on_repr() {
        make_group().repr();
    }

    #[test]
    #[should_panic]
    fn group_panics_on_span() {
        make_group().span();
    }


    // === Def ===

    fn make_def() -> Shape<Ast> {
        Def {
            name : Ast::cons("Foo"),
            args : vec![],
            body : None
        }.into()
    }

    #[test]
    #[should_panic]
    fn def_panics_on_repr() {
        make_def().repr();
    }

    #[test]
    #[should_panic]
    fn def_panics_on_span() {
        make_def().span();
    }

    // === Foreign ===

    fn make_foreign() -> Shape<Ast> {
        Foreign {
            indent : 0,
            lang   : "Python".into(),
            code   : vec![]
        }.into()
    }

    #[test]
    #[should_panic]
    fn foreign_panics_on_repr() {
        make_foreign().repr();
    }

    #[test]
    #[should_panic]
    fn foreign_panics_on_span() {
        make_foreign().span();
    }
}
