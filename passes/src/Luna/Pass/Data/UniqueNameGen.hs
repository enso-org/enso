module Luna.Pass.Data.UniqueNameGen where

import Luna.Prelude as P
import Luna.IR
import OCI.Pass.Manager
import Data.TypeDesc

newtype UniqueNameGen = UniqueNameGen P.String
makeWrapped ''UniqueNameGen

initNameGen :: MonadPassManager m => m ()
initNameGen = setAttr (getTypeDesc @UniqueNameGen) $ UniqueNameGen ""

advanceGen :: UniqueNameGen -> (P.String, UniqueNameGen)
advanceGen a = ('#' : str, gen) where
    gen = wrap str
    str = incString $ unwrap a

    incString s = case incString' s of
        (s, True)  -> 'a' : s
        (s, False) -> s

    incString' ""       = ("", True)
    incString' (a : as) = case incString' as of
        (s, False) -> (a : s, False)
        (s, True)  -> if a == 'z' then ('a' : s, True) else (succ a : s, False)

genName :: Editor Attr UniqueNameGen m => m Name
genName = do
    gen <- getAttr @UniqueNameGen
    let (res, newGen) = advanceGen gen
    putAttr @UniqueNameGen newGen
    return $ convert res
