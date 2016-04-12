module Old.Luna.Syntax.Term.Expr.Lit (module Old.Luna.Syntax.Term.Expr.Lit, module X) where

import           Prelude.Luna            hiding (Rational, Integer, String)
import qualified Prelude.Luna            as P
import           Luna.Pretty.Styles (StaticNameOnly)
import           Luna.Syntax.Name
import           Luna.Syntax.Term.Expr.Symbol as X (Star(Star))


--------------------
-- === String === --
--------------------

newtype String = String P.String deriving (Show, Read, Ord, Eq, IsString)
makeClassy  ''String
makeWrapped ''String

-- === Instances === --

-- Conversions
instance Convertible Name   String where convert = String  ∘ convert ; {-# INLINE convert #-}
instance Convertible String Name   where convert = convert ∘ unwrap' ; {-# INLINE convert #-}

-- Repr
instance {-# OVERLAPPABLE #-} Repr StaticNameOnly String where repr = fromString ∘ show ∘ unwrap'


--------------------
-- === Number === --
--------------------

data System = Rational P.Rational
            | Integer  P.Integer
            | Double   P.Double -- FIXME[WD->MK]: to delete
            deriving (Show, Read)

data Number = Number { _radix :: Int, _system :: System } deriving (Show, Read)
makeClassy ''Number


binary, ternary, quaternary, quinary, senary, septenary, octal, nonary, decimal, undecimal, duodecimal, tridecimal,
    tetradecimal, pentadecimal, hexadecimal, septendecimal, octodecimal, nonadecimal, vigesimal, unovigesimal,
    duovigesimal, triovigesimal, quadrovigesimal, pentavigesimal, hexavigesimal, heptovigesimal, ocotovigesimal,
    novovigesimal, trigesimal, unotrigesimal, duotrigesimal, triotrigesimal, quadrotrigesimal, pentatrigesimal, hexatrigesimal
    :: System -> Number

binary           = Number 2
ternary          = Number 3
quaternary       = Number 4
quinary          = Number 5
senary           = Number 6
septenary        = Number 7
octal            = Number 8
nonary           = Number 9
decimal          = Number 10
undecimal        = Number 11
duodecimal       = Number 12
tridecimal       = Number 13
tetradecimal     = Number 14
pentadecimal     = Number 15
hexadecimal      = Number 16
septendecimal    = Number 17
octodecimal      = Number 18
nonadecimal      = Number 19
vigesimal        = Number 20
unovigesimal     = Number 21
duovigesimal     = Number 22
triovigesimal    = Number 23
quadrovigesimal  = Number 24
pentavigesimal   = Number 25
hexavigesimal    = Number 26
heptovigesimal   = Number 27
ocotovigesimal   = Number 28
novovigesimal    = Number 29
trigesimal       = Number 30
unotrigesimal    = Number 31
duotrigesimal    = Number 32
triotrigesimal   = Number 33
quadrotrigesimal = Number 34
pentatrigesimal  = Number 35
hexatrigesimal   = Number 36
