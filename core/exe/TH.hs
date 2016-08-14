{-# LANGUAGE NoMonomorphismRestriction #-}

module TH where
import Prelude
import Language.Haskell.TH
import Control.Lens
import Data.List (elemIndex)
import qualified Data.IntMap as Map
import Data.Monoid


testTH2 el matchesQ fnames = do
    matches <- sequence matchesQ


    [TySynInstD _ (TySynEqn ts lst)] <- reifyInstances (mkName "All") [ConT $ mkName "Atom"]
    let atoms = typeListElems lst

    Just idxs <- sequence <$> mapM (matchToID atoms) matches

    let funcs     = VarE <$> fnames
        funcTable = Map.fromList $ zip idxs funcs

        defaultMatch = VarE $ mkName "defaultMatch"
        runCase      = VarE $ mkName "runCase"
        table        = ListE $ row <$> [0 .. length atoms]
        row i        = case Map.lookup i funcTable of
            Just f  -> f
            Nothing -> defaultMatch


    return $ AppE (AppE runCase (VarE el)) table



matchToFunc match = mkMatch $ LamE [pat] exp where
    Match pat (NormalB exp) _ = match
    mkMatch                   = AppE (VarE $ mkName "matchx")

matchToID atoms match = do
    let ConP symDN _ = match
    symD <- reify symDN
    let atom  = getAtomFromSymD symD
        idx   = atom `elemIndex` atoms
    return idx


class HasName a where
    name :: Lens' a Name

instance HasName Dec where
    name = lens get set where
        get = \case
            TySynInstD n _ -> n
        set a n = case a of
            TySynInstD _ a -> TySynInstD n a

typeListElems :: Type -> [Type]
typeListElems = \case
    SigT (AppT (AppT _ a) lst) _ -> a : typeListElems lst
    SigT PromotedNilT _          -> []


getAtomFromSymD d = findTarget t2 where
    DataConI _ t _                               = d
    ForallT _ _ t2                               = t
    findTarget = \case
        AppT (AppT ArrowT _) t     -> findTarget t
        AppT (AppT _ x) _ -> x
