{-# LANGUAGE Strict            #-}
{-# LANGUAGE OverloadedStrings #-}

module Luna.Syntax.Text.Lexer.Symbol where

import Prologue hiding (Symbol, List, element)
import Luna.Syntax.Text.Lexer.Token

import Data.Text32 (Text32)

-- FIXME[WD]: TO REFACTOR
class ShowCons a where
    showCons :: a -> Text32


------------------
-- === Tags === --
------------------

-- === Definition === --

type Tags = [Text32]

class IsTagged a where
    getTags :: a -> Tags
    default getTags :: Show a => a -> Tags
    getTags = tagFromShow ; {-# INLINE getTags #-}


-- === Utils === --

singleTag :: Text32 -> Tags
singleTag = pure ; {-# INLINE singleTag #-}

tagFromShow :: Show a => a -> Tags
tagFromShow = singleTag . convert . show ; {-# INLINE tagFromShow #-}


-- === Instances === --

data Tagged a = Tagged Tags a deriving (Show, Functor, Foldable, Traversable)



--------------------
-- === Symbol === --
--------------------

-- === Definitions === --

data Symbol -- Layout
            = STX
            | ETX
            | EOL
            | Terminator
            | BlockStart
            | Block       !Bound
            | Group       !Bound
            | Marker      !Word64

            -- Ident
            | Var         !Text32
            | Cons        !Text32
            | Wildcard

            -- Keyword
            | KwAll
            | KwCase
            | KwClass
            | KwDef
            | KwForeign
            | KwImport
            | KwNative
            | KwOf

            -- Operator
            | Operator    !Text32
            | Modifier    !Text32
            | Accessor
            -- | Arrow
            | Assignment
            | Typed
            | TypeApp
            | Merge
            | Range
            | Anything

            -- Literal
            | Number      !Number
            | Quote       !StrType  !Bound
            | Str         !Text32
            | StrEsc      !StrEscType
            | List        !Bound
            | StrWrongEsc !Int

            -- Comment
            | Disable
            | Doc         !Text32

            -- Config
            | Metadata    !Text32
            -- | Pragma ...

            -- Other
            | Unknown     !Text32
            | Incorrect   !Text32
            deriving (Generic, Show, Eq, Ord)

data StrEscType = CharStrEsc  !Int
                | NumStrEsc   !Int
                | QuoteEscape !StrType
                | SlashEsc
                deriving (Generic, Show, Eq, Ord)

data Bound   = Begin | End              deriving (Generic, Show, Eq, Ord)
data StrType = RawStr | FmtStr | NatStr deriving (Generic, Show, Eq, Ord)
data Numbase = Dec | Bin | Oct | Hex    deriving (Generic, Show, Eq, Ord)
data Number  = NumRep { _base     :: Numbase
                      , _intPart  :: Text32
                      , _fracPart :: Text32
                      , _expPart  :: Text32
                      } deriving (Generic, Show, Eq, Ord)

instance NFData Symbol
instance NFData StrEscType
instance NFData Bound
instance NFData StrType
instance NFData Numbase
instance NFData Number
makeClassy ''Symbol
makeLenses ''Number


-- === Utils === --

checkSpecialVar :: Text32 -> Symbol
checkSpecialVar = \case
    "all"     -> KwAll
    "case"    -> KwCase
    "class"   -> KwClass
    "def"     -> KwDef
    "foreign" -> KwForeign
    "import"  -> KwImport
    "native"  -> KwNative
    "of"      -> KwOf
    "_"       -> Wildcard
    name      -> Var name
{-# INLINE checkSpecialVar #-}

matchVar, matchCons, matchOperator, matchModifier, matchStr, matchDocComment, matchMetadata :: Symbol -> Maybe Text32
matchNumber   :: Symbol -> Maybe Number
matchMarker   :: Symbol -> Maybe Word64
matchStrEsc   :: Symbol -> Maybe StrEscType
matchVar        = \case { Var        a -> Just a ; _ -> Nothing } ; {-# INLINE matchVar        #-}
matchCons       = \case { Cons       a -> Just a ; _ -> Nothing } ; {-# INLINE matchCons       #-}
matchOperator   = \case { Operator   a -> Just a ; _ -> Nothing } ; {-# INLINE matchOperator   #-}
matchModifier   = \case { Modifier   a -> Just a ; _ -> Nothing } ; {-# INLINE matchModifier   #-}
matchStr        = \case { Str        a -> Just a ; _ -> Nothing } ; {-# INLINE matchStr        #-}
matchStrEsc     = \case { StrEsc     a -> Just a ; _ -> Nothing } ; {-# INLINE matchStrEsc     #-}
matchNumber     = \case { Number     a -> Just a ; _ -> Nothing } ; {-# INLINE matchNumber     #-}
matchMarker     = \case { Marker     a -> Just a ; _ -> Nothing } ; {-# INLINE matchMarker     #-}
matchDocComment = \case { Doc        a -> Just a ; _ -> Nothing } ; {-# INLINE matchDocComment #-}
matchMetadata   = \case { Metadata   a -> Just a ; _ -> Nothing } ; {-# INLINE matchMetadata   #-}


intNum :: Text32 -> Number
intNum  i = NumRep Dec i mempty mempty ; {-# INLINE intNum #-}

pretty :: Symbol -> Text32
pretty = \case
    STX         {} -> "Start of text"
    ETX         {} -> "End of text"
    EOL         {} -> "End of line"
    Terminator  {} -> "Expression terminator"
    BlockStart  {} -> "Expression block start"
    Block       {} -> "Block"
    Group       {} -> "Expression Group"
    Marker      {} -> "Internal position marker"
    Var         {} -> "Variable"
    Cons        {} -> "Constructor"
    Wildcard    {} -> "Wildcard"
    KwAll       {} -> "Keyword `All`"
    KwCase      {} -> "Keyword `Case`"
    KwClass     {} -> "Keyword `Class`"
    KwDef       {} -> "Keyword `Def`"
    KwForeign   {} -> "Keyword `Foreign`"
    KwImport    {} -> "Keyword `Import`"
    KwNative    {} -> "Keyword `Native`"
    KwOf        {} -> "Keyword `Of`"
    Operator    {} -> "Operator"
    Modifier    {} -> "Modifier"
    Accessor    {} -> "Accessor"
    Assignment  {} -> "Assignment"
    Typed       {} -> "Typed"
    TypeApp     {} -> "Type application"
    Merge       {} -> "Merge operator"
    Range       {} -> "Range operator"
    Anything    {} -> "Anything operator"
    Number      {} -> "Number"
    Quote       {} -> "Quote"
    Str         {} -> "String literal"
    StrEsc      {} -> "String escape sequence"
    List        {} -> "List"
    StrWrongEsc {} -> "Wrong string escape sequence"
    Disable     {} -> "Disable block"
    Doc         {} -> "Documentation"
    Metadata    {} -> "Metadata"
    Unknown     s  -> "Unknown symbol " <> s
    Incorrect   s  -> "Incorrect " <> s
{-# INLINE pretty #-}


-- === Instances === --

-- FIXME[WD]: Templatehaskellize vvv
instance ShowCons Symbol where
    showCons = \case
        STX         {} -> "STX"
        ETX         {} -> "ETX"
        EOL         {} -> "EOL"
        Terminator  {} -> "Terminator"
        BlockStart  {} -> "BlockStart"
        Block       {} -> "Block"
        Group       {} -> "Group"
        Marker      {} -> "Marker"
        Var         {} -> "Var"
        Cons        {} -> "Cons"
        Wildcard    {} -> "Wildcard"
        KwAll       {} -> "KwAll"
        KwCase      {} -> "KwCase"
        KwClass     {} -> "KwClass"
        KwDef       {} -> "KwDef"
        KwForeign   {} -> "KwForeign"
        KwImport    {} -> "KwImport"
        KwNative    {} -> "KwNative"
        KwOf        {} -> "KwOf"
        Operator    {} -> "Operator"
        Modifier    {} -> "Modifier"
        Accessor    {} -> "Accessor"
        Assignment  {} -> "Assignment"
        Typed       {} -> "Typed"
        TypeApp     {} -> "TypeApp"
        Merge       {} -> "Merge"
        Range       {} -> "Range"
        Anything    {} -> "Anything"
        Number      {} -> "Number"
        Quote       {} -> "Quote"
        Str         {} -> "Str"
        StrEsc      {} -> "StrEsc"
        List        {} -> "List"
        StrWrongEsc {} -> "StrWrongEsc"
        Disable     {} -> "Disable"
        Doc         {} -> "Doc"
        Metadata    {} -> "Metadata"
        Unknown     {} -> "Unknown"
        Incorrect   {} -> "Incorrect"
    {-# INLINE showCons #-}

-- Tags
instance IsTagged Symbol where
    getTags a = (: [showCons a]) $ case a of
        STX         {} -> "Layout"
        ETX         {} -> "Layout"
        EOL         {} -> "Layout"
        Terminator  {} -> "Layout"
        BlockStart  {} -> "Layout"
        Block       {} -> "Layout"
        Group       {} -> "Layout"
        Marker      {} -> "Layout"
        Var         {} -> "Ident"
        Cons        {} -> "Ident"
        Wildcard    {} -> "Ident"
        KwAll       {} -> "Keyword"
        KwCase      {} -> "Keyword"
        KwClass     {} -> "Keyword"
        KwDef       {} -> "Keyword"
        KwForeign   {} -> "Keyword"
        KwImport    {} -> "Keyword"
        KwNative    {} -> "Keyword"
        KwOf        {} -> "Keyword"
        Operator    {} -> "Operator"
        Modifier    {} -> "Operator"
        Accessor    {} -> "Operator"
        Assignment  {} -> "Operator"
        Typed       {} -> "Operator"
        TypeApp     {} -> "Operator"
        Merge       {} -> "Operator"
        Range       {} -> "Operator"
        Anything    {} -> "Operator"
        Number      {} -> "Literal"
        Quote       {} -> "Literal"
        Str         {} -> "Literal"
        StrEsc      {} -> "Literal"
        List        {} -> "Literal"
        StrWrongEsc {} -> "Literal"
        Disable     {} -> "Control"
        Doc         {} -> "Comment"
        Metadata    {} -> "Config"
        Unknown     {} -> "Error"
        Incorrect   {} -> "Error"
    {-# INLINE getTags #-}


-- === Accessors === --

instance HasSymbol (Symbol, a) where symbol = _1 ; {-# INLINE symbol #-}
instance HasSymbol (a, Symbol) where symbol = _2 ; {-# INLINE symbol #-}
instance HasSymbol a => HasSymbol (Token a) where symbol = element . symbol ; {-# INLINE symbol #-}
