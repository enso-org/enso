{-# LANGUAGE UndecidableInstances #-}


module Luna.IR.Format2 where

import Luna.Prelude hiding (List, String, Integer, Rational)

import Type.List                  (TakeUntil)

import Luna.IR.Term.Core
import Luna.IR.Term.Function
import Luna.IR.Term.Unit
import Luna.IR.Term.World
import Data.Property              hiding (Update)
import Type.Relation              (Super)
import Type.Bool
import Data.Reprx
import Type.Container (Every)
import Data.Families  (makeLunaComponents)
import OCI.IR.Layout.Format
import OCI.IR.Term
import Luna.IR.Format


--------------------------------
-- === Expression formats === --
--------------------------------


-- === Relations === --

type instance TermTypesOf Literal    = '[ Star    , String , Number    ]
type instance TermTypesOf Value      = '[ Cons    , Lam    , Match, Monadic ] <> TermTypesOf Literal
type instance TermTypesOf Thunk      = '[ Acc     , App                ] <> TermTypesOf Value
type instance TermTypesOf Phrase     = '[ Blank   , Unify  , Var , Seq ] <> TermTypesOf Thunk
type instance TermTypesOf Draft      = '[ Missing , Grouped, FmtString, FieldLens, World, Unit, UnitProxy, ClsASG, ASGFunction, FunctionSig, RootedFunction, ASGRootedFunction
                                        , List, Tuple, LeftSection, RightSection, AccSection] <> TermTypesOf Phrase -- TODO: move World, Unit, Class, etc to separate layout


type instance Access Format Star                      = Literal
type instance Access Format String                    = Literal
type instance Access Format Number                    = Literal
type instance Access Format Cons                      = Value
type instance Access Format Match                     = Value
type instance Access Format Lam                       = Value
type instance Access Format Monadic                   = Value
type instance Access Format Acc                       = Thunk
type instance Access Format App                       = Thunk
type instance Access Format Blank                     = Phrase
type instance Access Format Unify                     = Phrase
type instance Access Format Var                       = Phrase
type instance Access Format Seq                       = Phrase
type instance Access Format Grouped                   = Draft
type instance Access Format Missing                   = Draft
type instance Access Format FmtString                 = Draft
type instance Access Format FieldLens                 = Draft
type instance Access Format List                      = Draft
type instance Access Format Tuple                     = Draft
type instance Access Format AccSection                = Draft
type instance Access Format LeftSection               = Draft
type instance Access Format RightSection              = Draft
type instance Access Format Update                    = Draft
type instance Access Format Modify                    = Draft

type instance Access Format Unit                      = Draft
type instance Access Format UnitProxy                 = Draft
type instance Access Format Import                    = Draft
type instance Access Format ImportHub                 = Draft
type instance Access Format UnresolvedImport          = Draft
type instance Access Format UnresolvedImportHub       = Draft
type instance Access Format UnresolvedImportSrc       = Draft
type instance Access Format UnresolvedImportHub       = Draft
type instance Access Format ForeignImportList         = Draft
type instance Access Format ForeignLocationImportList = Draft
type instance Access Format ForeignSymbolImport       = Draft
type instance Access Format ForeignImportSafety       = Draft
type instance Access Format ClsASG                    = Draft
type instance Access Format RecASG                    = Draft
type instance Access Format FieldASG                  = Draft
type instance Access Format Typed                     = Draft
type instance Access Format ASGFunction               = Draft
type instance Access Format FunctionSig               = Draft
type instance Access Format RootedFunction            = Draft
type instance Access Format ASGRootedFunction         = Draft
type instance Access Format Invalid                   = Draft

type instance Access Format Disabled                  = Draft
type instance Access Format Marker                    = Draft
type instance Access Format Marked                    = Draft
type instance Access Format Documented                = Draft
type instance Access Format Metadata                  = Draft
