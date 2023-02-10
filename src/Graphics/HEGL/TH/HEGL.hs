{-# LANGUAGE TemplateHaskell #-}

module Graphics.HEGL.TH.HEGL (
    gen2DCoordDecls,
    gen3DCoordDecls
) where

import Language.Haskell.TH
import Data.List (delete)


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

-- e.g.: xy_ v = GLGenExpr (genID ()) (OpCoordMulti (CoordX `CoordCons` (CoordY `CoordCons` CoordNil)) v)
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
                        (ConE $ mkName "GLGenExpr")
                        (AppE (VarE $ mkName "genID") (TupE [])))
                    (AppE
                        (AppE
                            (ConE $ mkName "OpCoordMulti")
                            (InfixE
                                (Just (ConE $ mkName $ coordCon x))
                                (ConE $ mkName "CoordCons")
                                (Just
                                (InfixE
                                    (Just (ConE $ mkName $ coordCon y))
                                    (ConE $ mkName "CoordCons")
                                    (Just (ConE $ mkName "CoordNil"))))))
                        (VarE $ mkName "v"))))
                []]

-- e.g.: xyz_ v = GLGenExpr (genID ()) (OpCoordMulti (CoordX `CoordCons` (CoordY `CoordCons` (CoordZ `CoordCons` CoordNil))) v)
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
                        (ConE $ mkName "GLGenExpr")
                        (AppE (VarE $ mkName "genID") (TupE [])))
                    (AppE
                        (AppE
                            (ConE $ mkName "OpCoordMulti")
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
                        (VarE $ mkName "v"))))
                []]
