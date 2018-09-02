{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.Syntax.Text.Parser.IR.Ast where

import qualified Prelude  as P
import           Prologue hiding (Text, imp, seq, some, span, takeWhile)

import qualified Data.Text32                           as Text
import qualified Luna.IR.Term.Ast.Invalid              as Invalid
import qualified Luna.Syntax.Text.Parser.Data.CodeSpan as CodeSpan

import Luna.IR                               (SomeTerm, Term)
import Luna.Syntax.Text.Parser.Data.CodeSpan (CodeSpan (CodeSpan),
                                              CodeSpanRange (CodeSpanRange))
import OCI.Data.Name                         (Name)


--

import qualified Control.Monad.State.Layered          as State
import qualified Data.Attoparsec.Internal.Types       as AttoParsec
import qualified Data.Attoparsec.Text32               as Parsec
import qualified Data.Char                            as Char
import qualified Data.Set                             as Set
import qualified Data.Text.Position                   as Position
import qualified Data.Text.Span                       as Span
import qualified GHC.Exts                             as GHC
import qualified Luna.Syntax.Text.Lexer               as Lexer
import qualified Luna.Syntax.Text.Lexer.Symbol        as Lexer
import qualified Luna.Syntax.Text.Parser.State.Marker as Marker
import qualified Luna.Syntax.Text.Scope               as Scope

import Control.Monad.State.Layered              (StateT, StatesT)
import Data.Set                                 (Set)
import Data.Text.Position                       (FileOffset (..))
import Data.Text.Position                       (Delta, Position)
import Luna.Syntax.Text.Parser.State.LastOffset (LastOffset (LastOffset))
import Text.Parser.State.Indent                 (Indent)

import Data.Parser             hiding (Result, Token, Tokens, endOfInput)
import Text.Parser.Combinators (some)



type Text = Text.Text32
data SyntaxVersion = Syntax1 | Syntax2 deriving (Show)




data Spanned a = Spanned
    { _span :: CodeSpan
    , _ast  :: a
    } deriving (Eq, Functor, Ord, Show)
makeLenses ''Spanned

instance Convertible (Spanned a) a where
    convert = view ast
    {-# INLINE convert #-}


unsafeUnspan :: Spanned a -> a
unsafeUnspan = \(Spanned _ a) -> a
{-# INLINE unsafeUnspan #-}


prependAsOffset :: Spanned a -> (Spanned b -> Spanned b)
prependAsOffset = \t -> span %~ (CodeSpan.prependAsOffset (t ^. span))
{-# INLINE prependAsOffset #-}

prependOffset :: Spanned a -> (Spanned b -> Spanned b)
prependOffset = \t -> span %~ (CodeSpan.prependAsOffset $ CodeSpan.dropLength (t ^. span))
{-# INLINE prependOffset #-}

prependOffset' :: CodeSpan -> (Spanned b -> Spanned b)
prependOffset' = \t -> span %~ (CodeSpan.prependAsOffset $ CodeSpan.dropLength t)
{-# INLINE prependOffset' #-}






-----------------------
-- === Blacklist === --
-----------------------

-- === Definition === --

-- data Blacklist = Blacklist
--     { _unknownBlacklist :: Set Char
--     , _operators        :: Set Name
--     }
-- makeLenses ''Blacklist


-- -- === API === --

-- withBlacklistedUnknown :: State.Monad Blacklist m => Char -> m a -> m a
-- withBlacklistedUnknown = \a
--      -> State.withModified @Blacklist (unknownBlacklist %~ Set.insert a)
-- {-# INLINE withBlacklistedUnknown #-}


-- withBlacklistedOperator :: State.Monad Blacklist m => Name -> m a -> m a
-- withBlacklistedOperator = \a -> State.withModified @Blacklist (operators %~ Set.insert a)
-- {-# INLINE withBlacklistedOperator #-}


-- checkBlacklistedUnknown :: Char -> Parser ()
-- checkBlacklistedUnknown = \a -> do
--     blacklisted <- Set.member a . view unknownBlacklist <$> State.get @Blacklist
--     when_ blacklisted $ fail "Blacklisted"
-- {-# INLINE checkBlacklistedUnknown #-}

-- checkBlacklistedOperator :: Name -> Parser ()
-- checkBlacklistedOperator = \a -> do
--     blacklisted <- Set.member a . view operators <$> State.get @Blacklist
--     when_ blacklisted $ fail "Blacklisted"
-- {-# INLINE checkBlacklistedOperator #-}



-- -- === Instances === --

-- instance Mempty Blacklist where
--     mempty = Blacklist mempty mempty
--     {-# INLINE mempty #-}

-- instance Default Blacklist where
--     def = mempty
--     {-# INLINE def #-}





    -- data Location = Location
--     { _offset :: Delta
--     , _column :: Delta
--     , _row    :: Delta
--     } deriving (Eq, Generic, Ord, Show)
-- makeLenses ''Location


-- === Instances === --

-- instance NFData Location
-- instance Default Location where
--     def = Location 0 0 0
--     {-# INLINE def #-}


type Parser = StatesT
   '[ Result
    , SyntaxVersion
    , Indent
    , Position
    , LastOffset
    , CodeSpanRange
    , Marker.State
    , FileOffset
    , Scope.Scope
    ] Parsec.Parser



class Monad m => KnownParserOffset m where
    getParserOffset :: m Delta

instance KnownParserOffset Parsec.Parser where
    getParserOffset = AttoParsec.Parser $ \t pos more _ succ ->
        succ t pos more (convert $! AttoParsec.fromPos pos)
    {-# INLINE getParserOffset #-}

instance (MonadTrans t, Monad (t m), KnownParserOffset m)
      => KnownParserOffset (t m) where
    getParserOffset = lift getParserOffset
    {-# INLINE getParserOffset #-}




newtype Result = Result [Spanned Ast] deriving (Default, Mempty, Show)

register :: State.Monad Result m => Spanned Ast -> m ()
register = \a -> State.modify_ @Result $ wrapped %~ (a:)
{-# INLINE register #-}

lookupLastToken :: State.Getter Result m => m (Maybe (Spanned Ast))
lookupLastToken = head . unwrap <$> State.get @Result
{-# INLINE lookupLastToken #-}

lookupLastSymbol :: State.Getter Result m => m (Maybe Ast)
lookupLastSymbol = unspan <<$>> lookupLastToken
{-# INLINE lookupLastSymbol #-}

evalResult :: Monad m => StateT Result m a -> m [Spanned Ast]
evalResult = fmap (reverse . unwrap) . State.execDefT
{-# INLINE evalResult #-}



-----------------
-- === AST === --
-----------------

-- data Ast a
--     = Var { name :: Name }
    -- = AccSection   { path     :: [Name]                               }
    -- | Cons         { name     :: Name    , args  :: [Ast]             }
    -- | Disabled     { body     :: Ast                                  }
    -- | Documented   { doc      :: Txt     , base   :: Ast              }
    -- | Function     { name     :: Ast     , args   :: [Ast]
    --                , body     :: Ast                                  }
    -- | DefHeader    { tp       :: Ast     , unis   :: [Ast]
    --                , accs     :: [Ast]   , apps   :: [Ast]            }
    -- | Grouped      { body     :: Ast                                  }
    -- | Imp          { source   :: Ast     , target :: ImportTargetData }
    -- | ImportHub    { imps     :: [Ast]                                }
    -- | ImportSource { body     :: ImportSourceData                     }
    -- | Invalid      { desc     :: Invalid.Symbol                       }
    -- | List         { items    :: [Ast]                                }
    -- | Marked       { marker   :: Ast     , body   :: Ast              }
    -- | Marker       { id       :: Word64                               }
    -- | SectionLeft  { operator :: Ast     , body   :: Ast              }
    -- | SectionRight { operator :: Ast     , body   :: Ast              }
    -- | Modify       { base     :: Ast     , path   :: [Name]
    --                , operator :: Name    , value  :: Ast              }
    -- | Metadata     { content  :: Txt                                  }
    -- | Record       { isNative :: Bool    , name   :: Name
    --                , params   :: [Ast]   , conss  :: [Ast]
    --                , decls    :: [Ast]                                }
    -- | RecordCons   { name     :: Name    , fields :: [Ast]            }
    -- | RecordFields { names    :: [Name]  , tp     :: Ast              }
    -- | Seq          { former   :: Ast     , later  :: Ast              }
    -- | Tuple        { items    :: [Ast]                                }
    -- | Typed        { base     :: Ast     , tp     :: Ast              }
    -- | Unit         { imps     :: Ast     , units  :: [Ast]
    --                , cls      :: Ast                                  }
    -- DEPRECATED:
    -- | FunctionSig  { name     :: Ast     , sig    :: Ast         }

-- data Foo a = Foo {-# UNPACK #-} !a
-- data Spanned2 a = Spanned2
--     { _span2 :: CodeSpan
--     , _ast2  :: a
--     } deriving (Show)

-- type family Link t a


-- data Ast2 t
--     = AstTypeDef (TypeDef t)


-- type Unspanned = Ast Spanned


type family ExpandField t a
data Simple

-- type instance Link Parsed a = Spanned2 (LinkParsed a)
-- type family LinkParsed a where
--     LinkParsed (Maybe a) = Maybe (LinkParsed a)

-- type S a = ExpandField Simple a

-- type instance ExpandField Simple a = ExpandFieldSimple a

-- type family ExpandFieldSimple a where
--     ExpandFieldSimple (NonEmpty a)   = NonEmpty (ExpandFieldSimple a)
--     ExpandFieldSimple [a]            = [ExpandFieldSimple a]
--     ExpandFieldSimple Delta          = Delta
--     ExpandFieldSimple Int            = Int
--     ExpandFieldSimple Bool           = Bool
--     ExpandFieldSimple Name           = Name
--     ExpandFieldSimple Text           = Text
--     ExpandFieldSimple Invalid.Symbol = Invalid.Symbol

--     ExpandFieldSimple Ast            = Spanned Ast
--     ExpandFieldSimple LineBreak      = Spanned LineBreak
--     ExpandFieldSimple StrChunk       = Spanned StrChunk

data Struct
type family Link t a
type Link' t = Link t Struct

data Var       t = Atom_Var       { name     :: Name                    }
data Cons      t = Atom_Cons      { name     :: Name                    }
data Operator  t = Atom_Operator  { name     :: Name                    }
data Modifier  t = Atom_Modifier  { name     :: Name                    }
data Wildcard  t = Atom_Wildcard

data Number    t = Atom_Number    { digits   :: NonEmpty Word8          }
data Str       t = Atom_Str       { chunks   :: [Link t (StrChunk t)]   }

data Block    t = Atom_Block    { lines1   :: NonEmpty (Link' t)      }
data Tokens    t = Atom_Tokens    { lines    :: [Link' t]               }
data Marker    t = Atom_Marker    { markerID :: Int                     }
data LineBreak t = Atom_LineBreak { indent   :: Delta                   }

data Comment   t = Atom_Comment   { text     :: Text                    }
data Documented t = Atom_Documented { doc :: Link' t, base :: Link' t }

data Invalid   t = Atom_Invalid   { desc     :: Invalid.Symbol          }

data App       t = Atom_App       { func     :: Link' t, arg :: Link' t }
data InfixApp  t = Atom_InfixApp  { argl :: Link' t, func :: Link' t, argr :: Link' t }
data SectionLeft  t = Atom_SectionLeft  { arg      :: Link' t, func :: Link' t }
data SectionRight t = Atom_SectionRight { func     :: Link' t, arg :: Link' t }
data Missing   t = Atom_Missing
data List      t = Atom_List      { items    :: [Link' t]               }
data Unit      t = Atom_Unit      { body     :: Link' t                 }

data StrChunk t
    = StrPlain   Text
    | StrNewLine (LineBreak t)



instance Show (Link' t) => Show (Var       t) where show  (Atom_Var       t1   ) = "Var"       <> " (" <> show t1 <> ")"
instance Show (Link' t) => Show (Cons      t) where show  (Atom_Cons      t1   ) = "Cons"      <> " (" <> show t1 <> ")"
instance Show (Link' t) => Show (Operator  t) where show  (Atom_Operator  t1   ) = "Operator"  <> " (" <> show t1 <> ")"
instance Show (Link' t) => Show (Modifier  t) where show  (Atom_Modifier  t1   ) = "Modifier"  <> " (" <> show t1 <> ")"
instance Show (Link' t) => Show (Wildcard  t) where show  (Atom_Wildcard       ) = "Wildcard"
instance Show (Link' t) => Show (Number    t) where show  (Atom_Number    t1   ) = "Number"    <> " (" <> show t1 <> ")"
instance Show (Link' t) => Show (Block    t) where show  (Atom_Block    t1   ) = "Block"    <> " (" <> show t1 <> ")"
instance Show (Link' t) => Show (Tokens    t) where show  (Atom_Tokens    t1   ) = "Tokens"    <> " (" <> show t1 <> ")"
instance Show (Link' t) => Show (Marker    t) where show  (Atom_Marker    t1   ) = "Marker"    <> " (" <> show t1 <> ")"
instance Show (Link' t) => Show (LineBreak t) where show  (Atom_LineBreak t1   ) = "LineBreak" <> " (" <> show t1 <> ")"
instance Show (Link' t) => Show (Comment   t) where show  (Atom_Comment   t1   ) = "Comment"   <> " (" <> show t1 <> ")"
instance Show (Link' t) => Show (Documented t) where show  (Atom_Documented   t1 t2) = "Documented"   <> " (" <> show t1 <> ") (" <> show t2 <> ")"
instance Show (Link' t) => Show (Invalid   t) where show  (Atom_Invalid   t1   ) = "Invalid"   <> " (" <> show t1 <> ")"
instance Show (Link' t) => Show (App       t) where show  (Atom_App       t1 t2) = "App"       <> " (" <> show t1 <> ") (" <> show t2 <> ")"
instance Show (Link' t) => Show (InfixApp  t) where show  (Atom_InfixApp  t1 t2 t3) = "InfixApp"       <> " (" <> show t1 <> ") (" <> show t2 <> ") (" <> show t3 <> ")"
instance Show (Link' t) => Show (Missing   t) where show  (Atom_Missing        ) = "Missing"
instance Show (Link' t) => Show (List      t) where show  (Atom_List      t1   ) = "List"      <> " (" <> show t1 <> ")"
instance Show (Link' t) => Show (Unit      t) where show  (Atom_Unit      t1   ) = "Unit"      <> " (" <> show t1 <> ")"
instance Show (Link' t) => Show (SectionLeft      t) where show  (Atom_SectionLeft  t1 t2) = "SectionLeft"  <> " (" <> show t1 <> ") (" <> show t2 <> ")"
instance Show (Link' t) => Show (SectionRight     t) where show  (Atom_SectionRight t1 t2) = "SectionRight" <> " (" <> show t1 <> ") (" <> show t2 <> ")"
instance (Show (Link' t), Show (Link t (StrChunk t))) => Show (Str       t) where show  (Atom_Str       t1   ) = "Str"       <> " (" <> show t1 <> ")"


pattern Var       t1    = AstVar       (Atom_Var       t1)
pattern Cons      t1    = AstCons      (Atom_Cons      t1)
pattern Operator  t1    = AstOperator  (Atom_Operator  t1)
pattern Modifier  t1    = AstModifier  (Atom_Modifier  t1)
pattern Wildcard        = AstWildcard  (Atom_Wildcard)
pattern Number    t1    = AstNumber    (Atom_Number    t1)
pattern Str       t1    = AstStr       (Atom_Str       t1)
pattern Block    t1    = AstBlock    (Atom_Block    t1)
-- -- -- pattern Tokens    t1    = AstTokens    (Atom_Tokens    t1)
pattern Marker    t1    = AstMarker    (Atom_Marker    t1)
pattern LineBreak t1    = AstLineBreak (Atom_LineBreak t1)
pattern Comment   t1    = AstComment   (Atom_Comment   t1)
pattern Documented   t1 t2 = AstDocumented   (Atom_Documented   t1 t2)
pattern Invalid   t1    = AstInvalid   (Atom_Invalid   t1)
pattern App       t1 t2 = AstApp       (Atom_App       t1 t2)
pattern InfixApp  t1 t2 t3 = AstInfixApp       (Atom_InfixApp       t1 t2 t3)
pattern Missing         = AstMissing   (Atom_Missing)
pattern List      t1    = AstList      (Atom_List      t1)
pattern Unit      t1    = AstUnit      (Atom_Unit      t1)
pattern SectionLeft  t1 t2 = AstSectionLeft  (Atom_SectionLeft  t1 t2)
pattern SectionRight t1 t2 = AstSectionRight (Atom_SectionRight t1 t2)

pattern SVar       t1    = SimpleAstVar       (Atom_Var       t1)
pattern SCons      t1    = SimpleAstCons      (Atom_Cons      t1)
pattern SOperator  t1    = SimpleAstOperator  (Atom_Operator  t1)
pattern SModifier  t1    = SimpleAstModifier  (Atom_Modifier  t1)
pattern SWildcard        = SimpleAstWildcard  (Atom_Wildcard)
pattern SNumber    t1    = SimpleAstNumber    (Atom_Number    t1)
pattern SStr       t1    = SimpleAstStr       (Atom_Str       t1)
pattern SBlock    t1    = SimpleAstBlock    (Atom_Block    t1)
-- -- pattern STokens    t1    = SimpleAstTokens    (Atom_Tokens    t1)
pattern SMarker    t1    = SimpleAstMarker    (Atom_Marker    t1)
pattern SLineBreak t1    = SimpleAstLineBreak (Atom_LineBreak t1)
pattern SComment   t1    = SimpleAstComment   (Atom_Comment   t1)
pattern SDocumented   t1 t2    = SimpleAstDocumented   (Atom_Documented   t1 t2)
pattern SInvalid   t1    = SimpleAstInvalid   (Atom_Invalid   t1)
pattern SApp       t1 t2 = SimpleAstApp       (Atom_App       t1 t2)
pattern SInfixApp       t1 t2 t3 = SimpleAstInfixApp       (Atom_InfixApp       t1 t2 t3)
pattern SMissing         = SimpleAstMissing   (Atom_Missing)
pattern SList      t1    = SimpleAstList      (Atom_List      t1)
pattern SUnit      t1    = SimpleAstUnit      (Atom_Unit      t1)
pattern SSectionLeft  t1 t2 = SimpleAstSectionLeft  (Atom_SectionLeft  t1 t2)
pattern SSectionRight t1 t2 = SimpleAstSectionRight (Atom_SectionRight t1 t2)


pattern XList t <- Spanned s (AstList      (Atom_List      (prependOffsetToHead s -> t)))

prependOffsetToHead t = \case
    []     -> []
    (p:ps) -> prependOffset' t p : ps
-- deriving instance Show (Link' t) => Show (Var       t)
-- deriving instance Show (Link' t) => Show (Cons      t)
-- deriving instance Show (Link' t) => Show (Operator  t)
-- deriving instance Show (Link' t) => Show (Modifier  t)
-- deriving instance Show (Link' t) => Show (Wildcard  t)
-- deriving instance Show (Link' t) => Show (Number    t)
-- deriving instance Show (Link' t) => Show (Tokens    t)
-- deriving instance Show (Link' t) => Show (Marker    t)
-- deriving instance Show (Link' t) => Show (LineBreak t)
-- deriving instance Show (Link' t) => Show (Comment   t)
-- deriving instance Show (Link' t) => Show (Invalid   t)
-- deriving instance Show (Link' t) => Show (App       t)
-- deriving instance Show (Link' t) => Show (Missing   t)
-- deriving instance Show (Link' t) => Show (List      t)
deriving instance Show (Link' t) => Show (StrChunk  t)
-- deriving instance (Show (Link' t), Show (Link t (StrChunk t))) => Show (Str t)

deriving instance Eq (Link' t) => Eq (Var       t)
deriving instance Eq (Link' t) => Eq (Cons      t)
deriving instance Eq (Link' t) => Eq (Operator  t)
deriving instance Eq (Link' t) => Eq (Modifier  t)
deriving instance Eq (Link' t) => Eq (Wildcard  t)
deriving instance Eq (Link' t) => Eq (Number    t)
deriving instance Eq (Link' t) => Eq (Block    t)
deriving instance Eq (Link' t) => Eq (Tokens    t)
deriving instance Eq (Link' t) => Eq (Marker    t)
deriving instance Eq (Link' t) => Eq (LineBreak t)
deriving instance Eq (Link' t) => Eq (Comment   t)
deriving instance Eq (Link' t) => Eq (Documented   t)
deriving instance Eq (Link' t) => Eq (Invalid   t)
deriving instance Eq (Link' t) => Eq (App       t)
deriving instance Eq (Link' t) => Eq (InfixApp  t)
deriving instance Eq (Link' t) => Eq (Missing   t)
deriving instance Eq (Link' t) => Eq (List      t)
deriving instance Eq (Link' t) => Eq (Unit      t)
deriving instance Eq (Link' t) => Eq (SectionLeft      t)
deriving instance Eq (Link' t) => Eq (SectionRight      t)
deriving instance Eq (Link' t) => Eq (StrChunk  t)
deriving instance (Eq (Link' t), Eq (Link t (StrChunk t))) => Eq (Str t)

deriving instance Ord (Link' t) => Ord (Var       t)
deriving instance Ord (Link' t) => Ord (Cons      t)
deriving instance Ord (Link' t) => Ord (Operator  t)
deriving instance Ord (Link' t) => Ord (Modifier  t)
deriving instance Ord (Link' t) => Ord (Wildcard  t)
deriving instance Ord (Link' t) => Ord (Number    t)
deriving instance Ord (Link' t) => Ord (Block    t)
deriving instance Ord (Link' t) => Ord (Tokens    t)
deriving instance Ord (Link' t) => Ord (Marker    t)
deriving instance Ord (Link' t) => Ord (LineBreak t)
deriving instance Ord (Link' t) => Ord (Comment   t)
deriving instance Ord (Link' t) => Ord (Documented   t)
deriving instance Ord (Link' t) => Ord (Invalid   t)
deriving instance Ord (Link' t) => Ord (App       t)
deriving instance Ord (Link' t) => Ord (InfixApp  t)
deriving instance Ord (Link' t) => Ord (Missing   t)
deriving instance Ord (Link' t) => Ord (List      t)
deriving instance Ord (Link' t) => Ord (Unit      t)
deriving instance Ord (Link' t) => Ord (SectionLeft      t)
deriving instance Ord (Link' t) => Ord (SectionRight t)
deriving instance Ord (Link' t) => Ord (StrChunk  t)
deriving instance (Ord (Link' t), Ord (Link t (StrChunk t))) => Ord (Str t)


type instance Link Ast (StrChunk s) = Spanned (StrChunk s)
type instance Link Ast Struct = Spanned Ast

data Ast
    -- Identifiers
    = AstVar       (Var Ast)
    | AstCons      (Cons Ast)
    | AstOperator  (Operator Ast)
    | AstModifier  (Modifier Ast)
    | AstWildcard  (Wildcard Ast)

    -- Literals
    | AstNumber    (Number Ast)
    | AstStr       (Str Ast)

    -- Layouting
    | AstBlock    (Block Ast)
    | AstMarker    (Marker Ast)
    | AstLineBreak (LineBreak Ast)

    -- Docs
    | AstComment   (Comment Ast)
    | AstDocumented   (Documented Ast)

    -- Errors
    | AstInvalid   (Invalid Ast)

    | AstApp       (App Ast)
    | AstInfixApp  (InfixApp Ast)
    | AstMissing   (Missing Ast)
    | AstList      (List Ast)
    | AstUnit      (Unit Ast)
    | AstSectionLeft (SectionLeft Ast)
    | AstSectionRight (SectionRight Ast)
    deriving (Eq, Ord, Show)

class PrependSpan a where
    prependSpan :: CodeSpan -> a -> a
    prependSpan = \_ -> id



prepSpan :: CodeSpan -> (Spanned b -> Spanned b)
prepSpan = \t -> span %~ (CodeSpan.prependAsOffset t)
{-# INLINE prepSpan #-}

prepSpanToNonEmpty :: CodeSpan -> (NonEmpty (Spanned b) -> NonEmpty (Spanned b))
prepSpanToNonEmpty = \t (a :| as) -> (a & span %~ (CodeSpan.prependAsOffset t)) :| as
{-# INLINE prepSpanToNonEmpty #-}

prepSpanToList :: CodeSpan -> ([Spanned b] -> [Spanned b])
prepSpanToList = \t -> \case
    [] -> []
    (a:as) -> (a & span %~ (CodeSpan.prependAsOffset t)) : as
{-# INLINE prepSpanToList #-}

instance PrependSpan (Var Ast)
instance PrependSpan (Cons Ast)
instance PrependSpan (Operator Ast)
instance PrependSpan (Modifier Ast)
instance PrependSpan (Wildcard Ast)
instance PrependSpan (Number Ast)
instance PrependSpan (Str Ast)
instance PrependSpan (Marker Ast)
instance PrependSpan (LineBreak Ast)
instance PrependSpan (Comment Ast)
instance PrependSpan (Invalid Ast)
instance PrependSpan (Missing Ast)


instance PrependSpan (Block Ast) where
    prependSpan = \span (Atom_Block a) -> Atom_Block $ prepSpanToNonEmpty span a
    {-# INLINE prependSpan #-}

instance PrependSpan (Tokens Ast) where
    prependSpan = \span (Atom_Tokens a) -> Atom_Tokens $ prepSpanToList span a
    {-# INLINE prependSpan #-}

instance PrependSpan (App Ast) where
    prependSpan = \span (Atom_App a b) -> Atom_App (prepSpan span a) b
    {-# INLINE prependSpan #-}

instance PrependSpan (InfixApp Ast) where
    prependSpan = \span (Atom_InfixApp a b c) -> Atom_InfixApp (prepSpan span a) b c
    {-# INLINE prependSpan #-}

instance PrependSpan (SectionLeft Ast) where
    prependSpan = \span (Atom_SectionLeft a b) -> Atom_SectionLeft (prepSpan span a) b
    {-# INLINE prependSpan #-}

instance PrependSpan (SectionRight Ast) where
    prependSpan = \span (Atom_SectionRight a b) -> Atom_SectionRight (prepSpan span a) b
    {-# INLINE prependSpan #-}

instance PrependSpan (List Ast) where
    prependSpan = \span (Atom_List a) -> Atom_List (prepSpanToList span a)
    {-# INLINE prependSpan #-}

instance PrependSpan (Unit Ast) where
    prependSpan = \span (Atom_Unit a) -> Atom_Unit (prepSpan span a)
    {-# INLINE prependSpan #-}

instance PrependSpan (Documented Ast) where
    prependSpan = \span (Atom_Documented a b) -> Atom_Documented (prepSpan span a) b
    {-# INLINE prependSpan #-}


unspan :: Spanned Ast -> Ast
unspan = \(Spanned cs a) ->
    let span = CodeSpan.dropLength cs
    in case a of
        AstVar          a -> AstVar         $ prependSpan span a
        AstCons         a -> AstCons        $ prependSpan span a
        AstOperator     a -> AstOperator    $ prependSpan span a
        AstModifier     a -> AstModifier    $ prependSpan span a
        AstWildcard     a -> AstWildcard    $ prependSpan span a
        AstNumber       a -> AstNumber      $ prependSpan span a
        AstStr          a -> AstStr         $ prependSpan span a
        AstBlock       a  -> AstBlock      $ prependSpan span a
        AstMarker       a -> AstMarker      $ prependSpan span a
        AstLineBreak    a -> AstLineBreak   $ prependSpan span a
        AstComment      a -> AstComment     $ prependSpan span a
        AstDocumented   a -> AstDocumented  $ prependSpan span a
        AstInvalid      a -> AstInvalid     $ prependSpan span a
        AstApp          a -> AstApp         $ prependSpan span a
        AstInfixApp     a -> AstInfixApp    $ prependSpan span a
        AstMissing      a -> AstMissing     $ prependSpan span a
        AstList         a -> AstList        $ prependSpan span a
        AstUnit         a -> AstUnit        $ prependSpan span a
        AstSectionLeft  a -> AstSectionLeft $ prependSpan span a
        AstSectionRight a -> AstSectionRight$ prependSpan span a
{-# INLINE unspan #-}


type instance Link SimpleAst (StrChunk s) = StrChunk s
type instance Link SimpleAst Struct = SimpleAst

data SimpleAst
    -- Identifiers
    = SimpleAstVar       (Var SimpleAst)
    | SimpleAstCons      (Cons SimpleAst)
    | SimpleAstOperator  (Operator SimpleAst)
    | SimpleAstModifier  (Modifier SimpleAst)
    | SimpleAstWildcard  (Wildcard SimpleAst)

    -- Literals
    | SimpleAstNumber    (Number SimpleAst)
    | SimpleAstStr       (Str SimpleAst)

    -- Layouting
    | SimpleAstBlock    (Block SimpleAst)
    -- | SimpleAstTokens    (Tokens SimpleAst)
    | SimpleAstMarker    (Marker SimpleAst)
    | SimpleAstLineBreak (LineBreak SimpleAst)

    -- Docs
    | SimpleAstComment   (Comment SimpleAst)
    | SimpleAstDocumented   (Documented SimpleAst)

    -- Errors
    | SimpleAstInvalid   (Invalid SimpleAst)

    | SimpleAstApp       (App SimpleAst)
    | SimpleAstInfixApp  (InfixApp SimpleAst)
    | SimpleAstMissing   (Missing SimpleAst)
    | SimpleAstList      (List SimpleAst)
    | SimpleAstUnit      (Unit SimpleAst)
    | SimpleAstSectionLeft      (SectionLeft SimpleAst)
    | SimpleAstSectionRight      (SectionRight SimpleAst)
    deriving (Eq, Ord)

instance Show SimpleAst where
    show = \case
        SimpleAstVar       t -> show t
        SimpleAstCons      t -> show t
        SimpleAstOperator  t -> show t
        SimpleAstModifier  t -> show t
        SimpleAstWildcard  t -> show t
        SimpleAstNumber    t -> show t
        SimpleAstStr       t -> show t
        SimpleAstBlock    t -> show t
        -- SimpleAstTokens    t -> show t
        SimpleAstMarker    t -> show t
        SimpleAstLineBreak t -> show t
        SimpleAstComment   t -> show t
        SimpleAstDocumented   t -> show t
        SimpleAstInvalid   t -> show t
        SimpleAstApp       t -> show t
        SimpleAstInfixApp  t -> show t
        SimpleAstMissing   t -> show t
        SimpleAstList      t -> show t
        SimpleAstUnit      t -> show t
        SimpleAstSectionLeft      t -> show t
        SimpleAstSectionRight      t -> show t


sapp :: SimpleAst -> SimpleAst -> SimpleAst
sapp = SApp
{-# INLINE sapp #-}

sapp2 :: SimpleAst -> SimpleAst -> SimpleAst -> SimpleAst
sapp2 = \f -> sapp . sapp f
{-# INLINE sapp2 #-}

sapp3 :: SimpleAst -> SimpleAst -> SimpleAst -> SimpleAst -> SimpleAst
sapp3 = \f -> sapp .: sapp2 f
{-# INLINE sapp3 #-}

sapp4 :: SimpleAst -> SimpleAst -> SimpleAst -> SimpleAst -> SimpleAst
      -> SimpleAst
sapp4 = \f -> sapp .:. sapp3 f
{-# INLINE sapp4 #-}

sapps :: SimpleAst -> [SimpleAst] -> SimpleAst
sapps = foldl' sapp
{-# INLINE sapps #-}


instance IsString SimpleAst where
    fromString = convert

instance Convertible String (t1 -> SimpleAst)
      => IsString (t1 -> SimpleAst) where
    fromString = convert

instance Convertible String (t1 -> t2 -> SimpleAst)
      => IsString (t1 -> t2 -> SimpleAst) where
    fromString = convert

instance Convertible String (t1 -> t2 -> t3 -> SimpleAst)
      => IsString (t1 -> t2 -> t3 -> SimpleAst) where
    fromString = convert

-- instance Convertible String (t1 -> t2 -> t3 -> t4 -> SimpleAst)
--       => IsString (t1 -> t2 -> t3 -> t4 -> SimpleAst) where
--     fromString = convert


appMany :: SimpleAst -> [SimpleAst] -> SimpleAst
appMany = \a -> \case
    []     -> a
    (t:ts) -> appMany (sapp a t) ts
{-# appMany #-}

instance Convertible String SimpleAst where
    convert = let
        isMixfix = ('_' `elem`)
        in \case
            ""  -> SMissing
            "_" -> SWildcard
            x@(s:ss) -> let n = convert x in if
                | s == '_'       -> SVar      n
                | Char.isLower s -> SVar      n
                | Char.isUpper s -> SCons     n
                | Char.isDigit s -> SNumber $ charsToDigits (s :| ss)
                | isMixfix x     -> SVar      n
                | otherwise      -> case last ss of
                    Just '=' -> SModifier $ convert (s : unsafeInit ss)
                    _        -> SOperator n

charsToDigits :: NonEmpty Char -> NonEmpty Word8
charsToDigits (c :| cs) = go c :| (go <$> cs) where
    go c = fromIntegral $ Char.ord c - 48

instance Convertible SimpleAst (t1 -> SimpleAst)
      => Convertible String (t1 -> SimpleAst) where
    convert = convert . convertTo @SimpleAst

instance Convertible SimpleAst (t1 -> t2 -> SimpleAst)
      => Convertible String (t1 -> t2 -> SimpleAst) where
    convert = convert . convertTo @SimpleAst

instance Convertible SimpleAst (t1 -> t2 -> t3 -> SimpleAst)
      => Convertible String (t1 -> t2 -> t3 -> SimpleAst) where
    convert = convert . convertTo @SimpleAst

instance Convertible SimpleAst (t1 -> t2 -> t3 -> t4 -> SimpleAst)
      => Convertible String (t1 -> t2 -> t3 -> t4 -> SimpleAst) where
    convert = convert . convertTo @SimpleAst



instance t1 ~ SimpleAst
      => Convertible SimpleAst (t1 -> SimpleAst) where
    convert = sapp

instance (t1 ~ SimpleAst, t2 ~ SimpleAst)
      => Convertible SimpleAst (t1 -> t2 -> SimpleAst) where
    convert = sapp2

instance (t1 ~ SimpleAst, t2 ~ SimpleAst, t3 ~ SimpleAst)
      => Convertible SimpleAst (t1 -> t2 -> t3 -> SimpleAst) where
    convert = sapp3

instance (t1 ~ SimpleAst, t2 ~ SimpleAst, t3 ~ SimpleAst, t4 ~ SimpleAst)
      => Convertible SimpleAst (t1 -> t2 -> t3 -> t4 -> SimpleAst) where
    convert = sapp4

instance t1 ~ SimpleAst
      => Convertible Invalid.Symbol (t1 -> SimpleAst) where
    convert = convert . convertTo @SimpleAst

instance (t1 ~ SimpleAst, t2 ~ SimpleAst)
      => Convertible Invalid.Symbol (t1 -> t2 -> SimpleAst) where
    convert = convert . convertTo @SimpleAst

instance Convertible Invalid.Symbol SimpleAst where
    convert = SInvalid

instance {-# OVERLAPPABLE #-} Convertible' a SimpleAst
      => Convertible [a] SimpleAst where
    convert = SList . fmap convert'

instance GHC.IsList SimpleAst where
    type Item SimpleAst = SimpleAst
    fromList = convert

instance Num SimpleAst where
    fromInteger = intToSimpleAst
    (-) = flip SInfixApp (SOperator "-")
    (+) = flip SInfixApp (SOperator "+")
    (*) = flip SInfixApp (SOperator "*")

intToSimpleAst :: Integral a => a -> SimpleAst
intToSimpleAst a = if a < 0
    then undefined
    else SNumber $ intToDigits a

intToDigits :: Integral a => a -> NonEmpty Word8
intToDigits = go [] where
    go s x = loop (head :| s) tail where
        head = fromIntegral (x`mod` 10)
        tail = x `div` 10
    loop s@(r :| rs) = \case
        0 -> s
        x -> go (r : rs) x

instance a ~ SimpleAst
      => Num (a -> SimpleAst) where
    fromInteger i = SApp (fromInteger i)
--     (-) = extractOp (-)
--     (+) = extractOp (+)
--     (*) = extractOp (*)

-- extractOp op a b = sapp $ op (extract a) (extract b) where
--     extract f = let SApp x _ = f SMissing in x

class Simplify a where
    type family Simplified a
    simplify :: a -> Simplified a

    type Simplified a = a
    default simplify :: Simplified a ~ a => a -> Simplified a
    simplify = id

instance Simplify Int
instance Simplify Word8
instance Simplify Word16
instance Simplify Word32
instance Simplify Word64
instance Simplify Invalid.Symbol
instance Simplify Text
instance Simplify Name
instance Simplify Delta

instance Simplify a
      => Simplify   (NonEmpty a) where
    type Simplified (NonEmpty a) = NonEmpty (Simplified a)
    simplify = fmap simplify

instance Simplify a
      => Simplify   (Spanned a) where
    type Simplified (Spanned a) = Simplified a
    simplify = simplify . unsafeUnspan

instance Simplify a
      => Simplify   [a] where
    type Simplified [a] = [Simplified a]
    simplify = fmap simplify

instance Simplify (StrChunk Ast) where
    type Simplified (StrChunk Ast) = StrChunk SimpleAst
    simplify = \case
        StrPlain   t                  -> StrPlain   t
        StrNewLine (Atom_LineBreak t) -> StrNewLine (Atom_LineBreak t)

instance Simplify   Ast where
    type Simplified Ast = SimpleAst
    simplify = \case
        Var       t1    -> SVar       (simplify t1)
        Cons      t1    -> SCons      (simplify t1)
        Operator  t1    -> SOperator  (simplify t1)
        Modifier  t1    -> SModifier  (simplify t1)
        Wildcard        -> SWildcard
        Number    t1    -> SNumber    (simplify t1)
        Str       t1    -> SStr       (simplify t1)
        Block    t1    -> SBlock    (simplify t1)
        -- Tokens    t1    -> STokens    (simplify t1)
        Marker    t1    -> SMarker    (simplify t1)
        LineBreak t1    -> SLineBreak (simplify t1)
        Comment   t1    -> SComment   (simplify t1)
        Documented   t1 t2   -> SDocumented   (simplify t1) (simplify t2)
        Invalid   t1    -> SInvalid   (simplify t1)
        App       t1 t2 -> SApp       (simplify t1) (simplify t2)
        InfixApp  t1 t2 t3 -> SInfixApp  (simplify t1) (simplify t2) (simplify t3)
        Missing         -> SMissing
        List      t1    -> SList      (simplify t1)
        Unit      t1    -> SUnit      (simplify t1)
        SectionLeft  t1 t2 -> SSectionLeft  (simplify t1) (simplify t2)
        SectionRight t1 t2 -> SSectionRight (simplify t1) (simplify t2)



------ FIXME vvv
makeLenses ''Result


dropOffset :: Spanned a -> Spanned a
dropOffset = span %~ CodeSpan.dropOffset
{-# INLINE dropOffset #-}

inheritCodeSpan1 :: (Spanned Ast -> Ast) -> Spanned Ast -> Spanned Ast
inheritCodeSpan1 = \f t1 -> let
    s1 = t1 ^. span
    in Spanned s1 $! f (dropOffset t1)
{-# INLINE inheritCodeSpan1 #-}

inheritCodeSpan2
    :: (Spanned Ast -> Spanned Ast -> Ast)
    -> (Spanned Ast -> Spanned Ast -> Spanned Ast)
inheritCodeSpan2 = \f t1 t2 -> let
    s1 = t1 ^. span
    s2 = t2 ^. span
    in Spanned (s1 <> s2) $! f (dropOffset t1) t2
{-# INLINE inheritCodeSpan2 #-}

inheritCodeSpan3
    :: (Spanned Ast -> Spanned Ast -> Spanned Ast -> Ast)
    -> (Spanned Ast -> Spanned Ast -> Spanned Ast -> Spanned Ast)
inheritCodeSpan3 = \f t1 t2 t3 -> let
    s1 = t1 ^. span
    s2 = t2 ^. span
    s3 = t3 ^. span
    in Spanned (s1 <> s2 <> s3) $! f (dropOffset t1) t2 t3
{-# INLINE inheritCodeSpan3 #-}

inheritCodeSpanList1
    :: (NonEmpty (Spanned a) -> b) -> (NonEmpty (Spanned a) -> Spanned b)
inheritCodeSpanList1 = \f (a :| as) -> let
    s  = view span a
    ss = view span <$> as
    in Spanned (foldl' (<>) s ss) $! f (dropOffset a :| as)
{-# INLINE inheritCodeSpanList1 #-}

inheritCodeSpanList
    :: ([Spanned a] -> b) -> ([Spanned a] -> Spanned b)
inheritCodeSpanList = \f args -> case args of
    []     -> Spanned mempty $! f args
    (a:as) -> let
        s  = view span a
        ss = view span <$> as
        in Spanned (foldl' (<>) s ss) $! f (dropOffset a : as)
{-# INLINE inheritCodeSpanList #-}

-- Warning.
-- When using this function you need to handle all child-component spans
-- correctly. Consider three components 'a b c'. If you want to convert them
-- to list of components, you have to remove the offset span from the 'a' child
-- and put it to the newly created parent instead. Use this function only when
-- you create non-hierarchical components and use the public utils otherwise.
computeCodeSpanList1__
    :: NonEmpty (Spanned Ast) -> Ast -> (Spanned Ast)
computeCodeSpanList1__ = \ts -> let
    (s :| ss) = view span <$> ts
    in Spanned (foldl' (<>) s ss)
{-# INLINE computeCodeSpanList1__ #-}



-------------------
-- === Spans === --
-------------------

getLastOffset :: State.Getter LastOffset m => m Delta
getLastOffset = unwrap <$> State.get @LastOffset
{-# INLINE getLastOffset #-}

putLastOffset :: State.Setter LastOffset m => Delta -> m ()
putLastOffset = State.put @LastOffset . wrap
{-# INLINE putLastOffset #-}

spanned :: Parser a -> Parser (CodeSpan, a)
spanned = \parser -> do
    lastEndOff <- getLastOffset
    startOff   <- getParserOffset
    out        <- parser
    endOff     <- getParserOffset
    nextOff    <- whiteSpace
    putLastOffset endOff

    let spacing   = startOff - lastEndOff
        bodyLen   = endOff - startOff
        totalSpan = bodyLen + nextOff
        realSpan  = Span.leftSpacedSpan spacing bodyLen

    Position.incColumn totalSpan
    pure (CodeSpan realSpan realSpan, out)
{-# INLINE spanned #-}

newline :: Parser ()
newline = do
    len <- eol
    off <- whiteSpace
    Position.succLine
    Position.incColumn off
{-# INLINE newline #-}

lexeme :: Parser a -> Parser a
lexeme = \p -> p <* whiteSpace
{-# INLINE lexeme #-}

whiteSpace :: Parser Delta
whiteSpace = convert . Text.length <$> takeMany ' '
{-# INLINE whiteSpace #-}

computeSpan :: Parser a -> Parser (Spanned a)
computeSpan = \p -> uncurry Spanned <$> spanned p
{-# INLINE computeSpan #-}

eol :: Parser Delta
eol = (1 <$ n) <|> (2 <$ rn) <|> (1 <$ r) where
    n  = token '\n'
    r  = token '\r'
    rn = r >> n
{-# INLINE eol #-}

isEolBeginChar :: Char -> Bool
isEolBeginChar = (`elem` eolStartChars)
{-# INLINE isEolBeginChar #-}

eolStartChars :: [Char]
eolStartChars = ['\n', '\r', '\ETX']
{-# INLINE eolStartChars #-}



-- === Smart constructors === --

-- app :: Spanned Ast -> Spanned Ast -> Spanned Ast
-- app = inheritCodeSpan2 $ \base arg -> AstApp $ App base arg
-- {-# INLINE app #-}

-- cons' :: Name -> Ast
-- cons' = \name -> Cons name
-- {-# INLINE cons' #-}

-- invalid' :: Invalid.Symbol -> Ast
-- invalid' = \desc -> Invalid desc
-- {-# INLINE invalid' #-}

invalid :: Invalid.Symbol -> Spanned Ast
invalid = Spanned mempty . Invalid
{-# INLINE invalid #-}

-- marker' :: Int -> Ast
-- marker' = \id -> AstMarker $ Marker id
-- {-# INLINE marker' #-}

-- modifier' :: Name -> Ast
-- modifier' = \name -> AstModifier $ Modifier name
-- {-# INLINE modifier' #-}

-- operator' :: Name -> Ast
-- operator' = \name -> AstOperator $ Operator name
-- {-# INLINE operator' #-}

-- var' :: Name -> Ast
-- var' = \name -> AstVar $ Var name
-- {-# INLINE var' #-}

-- lineBreak' :: Delta -> Ast
-- lineBreak' = \indent -> AstLineBreak $ LineBreak indent
-- {-# INLINE lineBreak' #-}

-- comment' :: Text -> Ast
-- comment' = \txt -> AstComment $ Comment txt
-- {-# INLINE comment' #-}

-- wildcard' :: Ast
-- wildcard' = AstWildcard Wildcard
-- {-# INLINE wildcard' #-}

-- number' :: NonEmpty Word8 -> Ast
-- number' = \digits -> AstNumber $ Number digits
-- {-# INLINE number' #-}

-- str' :: [Spanned (StrChunk Ast)] -> Ast
-- str' = \chunks -> AstStr $ Str chunks
-- {-# INLINE str' #-}

-- tokens' :: [Spanned Ast] -> Ast
-- tokens' = \toks -> AstTokens $ Tokens toks
-- {-# INLINE tokens' #-}


--

sectionLeft :: Spanned Ast -> Spanned Ast -> Spanned Ast
sectionLeft = inheritCodeSpan2 $ \arg func -> SectionLeft arg func
{-# INLINE sectionLeft #-}

sectionRight :: Spanned Ast -> Spanned Ast -> Spanned Ast
sectionRight = inheritCodeSpan2 $ \func arg -> SectionRight func arg
{-# INLINE sectionRight #-}

app :: Spanned Ast -> Spanned Ast -> Spanned Ast
app = inheritCodeSpan2 $ \func arg -> App func arg
{-# INLINE app #-}

app2 :: Spanned Ast -> Spanned Ast -> Spanned Ast -> Spanned Ast
app2 = \f a b -> app (app f a) b
{-# INLINE app2 #-}

infixApp :: Spanned Ast -> Spanned Ast -> Spanned Ast -> Spanned Ast
infixApp = inheritCodeSpan3 $ \l f r -> InfixApp l f r
{-# INLINE infixApp #-}

apps :: Spanned Ast -> [Spanned Ast] -> Spanned Ast
apps = foldl' app
{-# INLINE apps #-}

-- missing' :: Ast
-- missing' = AstMissing Missing
-- {-# INLINE missing' #-}

missing :: Spanned Ast
missing = Spanned mempty Missing
{-# INLINE missing #-}

-- list' :: [Spanned Ast] -> Ast
-- list' = \items -> AstList $ List items
-- {-# INLINE list' #-}

list :: [Spanned Ast] -> Spanned Ast
list = inheritCodeSpanList $ \items -> List items
{-# INLINE list #-}

unit :: Spanned Ast -> Spanned Ast
unit = inheritCodeSpan1 $ \items -> Unit items
{-# INLINE unit #-}

block :: NonEmpty (Spanned Ast) -> Spanned Ast
block = inheritCodeSpanList1 $ \items -> Block items
{-# INLINE block #-}

isOperator :: Ast -> Bool
isOperator = \case
    AstOperator {} -> True
    _ -> False
{-# INLINE isOperator #-}

documented :: Spanned Ast -> Spanned Ast -> Spanned Ast
documented = inheritCodeSpan2 $ \doc base -> Documented doc base
{-# INLINE documented #-}

-- data Ast
--     -- Identifiers
--     = AstVar       Var
--     | AstCons      Cons
--     | AstOperator  Operator
--     | AstModifier  Modifier
--     | AstWildcard  Wildcard

--     -- Literals
--     | AstNumber    Number
--     | AstStr       Str

--     -- Layouting
--     | AstTokens    Tokens
--     | AstMarker    Marker
--     | AstLineBreak LineBreak

--     -- Docs
--     | AstComment   Comment

--     -- Errors
--     | AstInvalid   Invalid

--     deriving (Show)





-- isDigitCharAtBase :: Word8 -> Char -> Bool
-- isDigitCharAtBase = \base char -> case charToDigit char of
--     Just n  -> n < base
--     Nothing -> False
-- {-# INLINE isDigitCharAtBase #-}

-- isDecDigitChar :: Char -> Bool
-- isDecDigitChar = isDigitCharAtBase 10 ; {-# INLINE isDecDigitChar #-}

-- charToDigit :: Char -> Maybe Word8
-- charToDigit = \char -> let
--     n = Char.ord char
--     in unsafeConvert <$> if
--         | n >= 48 && n <= 57  -> Just $ n - 48      -- 0 to 9
--         | n >= 65 && n <= 90  -> Just $ n - 65 + 10 -- A to Z
--         | n >= 97 && n <= 122 -> Just $ n - 97 + 10 -- a to z
--         | otherwise           -> Nothing
-- {-# INLINE charToDigit #-}

-- unsafeCharToDigit :: Char -> Word8
-- unsafeCharToDigit = \c -> case charToDigit c of
--     Just t  -> t
--     Nothing -> error $ "Cannot convert char " <> [c] <> " to digit."
-- {-# INLINE unsafeCharToDigit #-}

-- lexNumber :: Lexer
-- lexNumber = addSymbol =<< checkInvalidSuffix number where
--     number  = Symbol.Number <$> (special <|> dec)
--     special = token '0' *> choice [p0 'x' 16, p0 'o' 8, p0 'b' 2]
--     dec     = Symbol.NumRep 10            <$> body 10 <*> frac 10
--     p0 s n  = Symbol.NumRep n  <$ token s <*> body n  <*> frac n
--     body n  = Txt.toList . Txt.map unsafeCharToDigit
--           <$> takeWhile1 (isDigitCharAtBase n)
--     frac  n = option mempty $ token '.' *> body n
-- {-# INLINE lexNumber #-}
