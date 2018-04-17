{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}

module Luna.Syntax.Text.Lexer.Symbol where

import Luna.Syntax.Text.Lexer.Token
import Prologue                     hiding (List, Symbol, element)

import qualified Data.Text32              as Text32
import qualified Luna.IR.Term.Ast.Invalid as Invalid

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

data Symbol
    -- Layout
    = STX
    | ETX
    | EOL
    | Terminator
    | BlockStart
    | Block       !Bound
    | Group       !Bound
    | Marker      !Word64

    -- Ident
    | Var          !Text32
    | VarMultiName !(Maybe Text32) ![Text32]
    | Cons         !Text32
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
    | Unknown     !Text32 -- DEPRECATED
    | Incorrect   !Text32 -- DEPRECATED
    | Invalid     !Invalid.Symbol
    deriving (Eq, Generic, Ord, Show)

data StrEscType
    = CharStrEsc  !Int
    | NumStrEsc   !Int
    | QuoteEscape !StrType
    | SlashEsc
    deriving (Eq, Generic, Ord, Show)

data Bound   = Begin | End              deriving (Eq, Generic, Ord, Show)
data StrType = RawStr | FmtStr | NatStr deriving (Eq, Generic, Ord, Show)
-- data Numbase = Dec | Bin | Oct | Hex    deriving (Eq, Generic, Ord, Show)
data Number  = NumRep
    { _base     :: Word8
    , _intPart  :: [Word8]
    , _fracPart :: [Word8]
    } deriving (Eq, Generic, Ord, Show)

instance NFData Symbol
instance NFData StrEscType
instance NFData Bound
instance NFData StrType
-- instance NFData Numbase
instance NFData Number
makeClassy ''Symbol
makeLenses ''Number

-- instance Convertible Numbase Word8 where
--     convert = \case
--         Bin -> 2
--         Oct -> 8
--         Dec -> 10
--         Hex -> 16


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
    s         -> Var s
{-# INLINE checkSpecialVar #-}

matchVar, matchCons, matchOperator, matchModifier, matchStr, matchDocComment,
    matchMetadata :: Symbol -> Maybe Text32
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
matchInvalid    = \case { Invalid    a -> Just a ; _ -> Nothing } ; {-# INLINE matchInvalid    #-}

-- intNum :: Text32 -> Number
-- intNum  i = NumRep Dec i mempty mempty ; {-# INLINE intNum #-}

pretty :: Symbol -> Text32
pretty = \case
    STX          {} -> "start of text"
    ETX          {} -> "end of text"
    EOL          {} -> "end of line"
    Terminator   {} -> "expression terminator"
    BlockStart   {} -> "expression block start"
    Block        {} -> "block"
    Group        {} -> "expression group"
    Marker       {} -> "metadata (position marker)"
    Var          {} -> "variable name"
    VarMultiName {} -> "multipart variable name"
    Cons         {} -> "Constructor"
    Wildcard     {} -> "wildcard"
    KwAll        {} -> "keyword `All`"
    KwCase       {} -> "keyword `Case`"
    KwClass      {} -> "keyword `Class`"
    KwDef        {} -> "keyword `Def`"
    KwForeign    {} -> "keyword `Foreign`"
    KwImport     {} -> "keyword `Import`"
    KwNative     {} -> "keyword `Native`"
    KwOf         {} -> "keyword `Of`"
    Operator     {} -> "operator"
    Modifier     {} -> "modifier"
    Accessor     {} -> "accessor"
    Assignment   {} -> "assignment"
    Typed        {} -> "typed"
    TypeApp      {} -> "type application"
    Merge        {} -> "merge operator"
    Range        {} -> "range operator"
    Anything     {} -> "anything operator"
    Number       {} -> "number"
    Quote        {} -> "quote"
    Str          {} -> "string literal"
    StrEsc       {} -> "string escape sequence"
    List         {} -> "list"
    StrWrongEsc  {} -> "wrong string escape sequence"
    Disable      {} -> "disable block"
    Doc          {} -> "documentation"
    Metadata     {} -> "metadata"
    Unknown      s  -> "unknown symbol " <> s
    Incorrect    s  -> "incorrect " <> s
    Invalid      s  -> "invalid " <> convert (show s)
{-# INLINE pretty #-}


-- === Instances === --

-- TODO: Generate with TH
instance ShowCons Symbol where
    showCons = \case
        STX          {} -> "STX"
        ETX          {} -> "ETX"
        EOL          {} -> "EOL"
        Terminator   {} -> "Terminator"
        BlockStart   {} -> "BlockStart"
        Block        {} -> "Block"
        Group        {} -> "Group"
        Marker       {} -> "Marker"
        Var          {} -> "Var"
        VarMultiName {} -> "VarMultiName"
        Cons         {} -> "Cons"
        Wildcard     {} -> "Wildcard"
        KwAll        {} -> "KwAll"
        KwCase       {} -> "KwCase"
        KwClass      {} -> "KwClass"
        KwDef        {} -> "KwDef"
        KwForeign    {} -> "KwForeign"
        KwImport     {} -> "KwImport"
        KwNative     {} -> "KwNative"
        KwOf         {} -> "KwOf"
        Operator     {} -> "Operator"
        Modifier     {} -> "Modifier"
        Accessor     {} -> "Accessor"
        Assignment   {} -> "Assignment"
        Typed        {} -> "Typed"
        TypeApp      {} -> "TypeApp"
        Merge        {} -> "Merge"
        Range        {} -> "Range"
        Anything     {} -> "Anything"
        Number       {} -> "Number"
        Quote        {} -> "Quote"
        Str          {} -> "Str"
        StrEsc       {} -> "StrEsc"
        List         {} -> "List"
        StrWrongEsc  {} -> "StrWrongEsc"
        Disable      {} -> "Disable"
        Doc          {} -> "Doc"
        Metadata     {} -> "Metadata"
        Unknown      {} -> "Unknown"
        Incorrect    {} -> "Incorrect"
        Invalid      {} -> "Invalid"
    {-# INLINE showCons #-}

instance IsTagged Symbol where
    getTags a = (: [showCons a]) $ case a of
        STX          {} -> "Layout"
        ETX          {} -> "Layout"
        EOL          {} -> "Layout"
        Terminator   {} -> "Layout"
        BlockStart   {} -> "Layout"
        Block        {} -> "Layout"
        Group        {} -> "Layout"
        Marker       {} -> "Layout"
        Var          {} -> "Ident"
        VarMultiName {} -> "Ident"
        Cons         {} -> "Ident"
        Wildcard     {} -> "Ident"
        KwAll        {} -> "Keyword"
        KwCase       {} -> "Keyword"
        KwClass      {} -> "Keyword"
        KwDef        {} -> "Keyword"
        KwForeign    {} -> "Keyword"
        KwImport     {} -> "Keyword"
        KwNative     {} -> "Keyword"
        KwOf         {} -> "Keyword"
        Operator     {} -> "Operator"
        Modifier     {} -> "Operator"
        Accessor     {} -> "Operator"
        Assignment   {} -> "Operator"
        Typed        {} -> "Operator"
        TypeApp      {} -> "Operator"
        Merge        {} -> "Operator"
        Range        {} -> "Operator"
        Anything     {} -> "Operator"
        Number       {} -> "Literal"
        Quote        {} -> "Literal"
        Str          {} -> "Literal"
        StrEsc       {} -> "Literal"
        List         {} -> "Literal"
        StrWrongEsc  {} -> "Literal"
        Disable      {} -> "Control"
        Doc          {} -> "Comment"
        Metadata     {} -> "Config"
        Unknown      {} -> "Error"
        Incorrect    {} -> "Error"
        Invalid      {} -> "Error"
    {-# INLINE getTags #-}


-- === Accessors === --

instance HasSymbol (Symbol, a) where symbol = _1 ; {-# INLINE symbol #-}
instance HasSymbol (a, Symbol) where symbol = _2 ; {-# INLINE symbol #-}
instance HasSymbol a => HasSymbol (Token a) where symbol = element . symbol ; {-# INLINE symbol #-}
