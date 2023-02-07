{-# LANGUAGE TemplateHaskell #-}

module Graphics.HEGL.TH (
    gen2DCoordDecls,
    gen3DCoordDecls,
    genGlExprId
) where

import Language.Haskell.TH
import Data.List (delete)

import Graphics.HEGL.GLExpr

import Debug.Trace (trace)


choose :: Eq a => Int -> [a] -> [[a]]
choose 0 xs = [[]]
choose n xs = do
    x <- xs
    ys <- choose (n-1) (delete x xs)
    return $ x:ys

coordCon "x" = "CoordX"
coordCon "y" = "CoordY"
coordCon "z" = "CoordZ"
coordCon "w" = "CoordW"

-- e.g.: xy_ v = OpCoordMulti (genID ()) (CoordX `CoordCons` (CoordY `CoordCons` CoordNil)) v
gen2DCoordDecls :: Q [Dec]
gen2DCoordDecls = return $ fmap gen $ choose 2 ["x", "y", "z", "w"] where
    gen coords@[x, y] = 
        FunD
            (mkName $ concat coords ++ "_")
            [Clause
                [VarP $ mkName "v"]
                (NormalB
                (AppE
                    (AppE
                        (AppE
                            (ConE $ mkName "OpCoordMulti")
                            (AppE (VarE $ mkName "genID") (TupE [])))
                        (InfixE
                            (Just (ConE $ mkName $ coordCon x))
                            (ConE $ mkName "CoordCons")
                            (Just
                            (InfixE
                                (Just (ConE $ mkName $ coordCon y))
                                (ConE $ mkName "CoordCons")
                                (Just (ConE $ mkName "CoordNil"))))))
                    (VarE $ mkName "v")))
                []]

-- e.g.: xyz_ v = OpCoordMulti (genID ()) (CoordX `CoordCons` (CoordY `CoordCons` (CoordZ `CoordCons` CoordNil))) v
gen3DCoordDecls :: Q [Dec]
gen3DCoordDecls = return $ fmap gen $ choose 3 ["x", "y", "z", "w"] where
    gen coords@[x, y, z] = 
        FunD
            (mkName $ concat coords ++ "_")
            [Clause
                [VarP $ mkName "v"]
                (NormalB
                (AppE
                    (AppE
                        (AppE
                            (ConE $ mkName "OpCoordMulti")
                            (AppE (VarE $ mkName "genID") (TupE [])))
                        (InfixE
                            (Just (ConE $ mkName $ coordCon x))
                            (ConE $ mkName "CoordCons")
                            (Just
                            (InfixE
                                (Just (ConE $ mkName $ coordCon y))
                                (ConE $ mkName "CoordCons")
                                (Just
                                    (InfixE
                                        (Just (ConE $ mkName $ coordCon z))
                                        (ConE $ mkName "CoordCons")
                                        (Just (ConE $ mkName "CoordNil"))))))))
                    (VarE $ mkName "v")))
                []]


getGlExprCons :: Q [(Name, Int)]
getGlExprCons = do
    (TyConI (DataD _ _ _ _ cons _)) <- reify ''GLExpr
    let get (ForallC _ _ (GadtC [name] pats _)) = (name, length pats)
        get x = trace (show x) undefined
    return $ map get cons

genGlExprId :: Q [Dec]
genGlExprId = (:[]) <$> FunD (mkName "getExprId") <$> 
    fmap genClause <$> getGlExprCons where
        genClause :: (Name, Int) -> Clause
        genClause (con, n) = Clause 
            [ConP con
            ([VarP (mkName "id")] ++ replicate (n-1) WildP)] 
            (NormalB (VarE (mkName "id"))) []
