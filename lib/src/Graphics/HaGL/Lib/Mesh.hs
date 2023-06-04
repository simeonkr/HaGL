module Graphics.HaGL.Lib.Mesh (
    Mesh(..),
    loadMesh
) where

import Text.Parsec
import Text.Parsec.Char
import Text.Read (readMaybe)

import Graphics.HaGL


data Mesh = Mesh {
    meshVertices :: [ConstExpr (Vec 3 Float)],
    meshNormals :: [ConstExpr (Vec 3 Float)],
    meshFaces :: [ConstExpr UInt]
}

loadMesh :: String -> IO Mesh
loadMesh file = do
    input <- readFile file
    case parse objParser file input of
        Right mesh -> return mesh
        Left err -> fail $ show err

objParser :: Parsec String () Mesh
objParser = do
    objLines <- objLineParser `endBy` (many $ char '\n')
    eof
    return $ buildMesh objLines

data ObjLine = 
    Vertex (ConstExpr (Vec 3 Float)) |
    Normal (ConstExpr (Vec 3 Float)) |
    Face (ConstExpr (Vec 3 UInt)) |
    Comment

buildMesh :: [ObjLine] -> Mesh
buildMesh xs = foldl f (Mesh [] [] []) xs where
    f mesh (Vertex v) = 
        mesh { meshVertices = meshVertices mesh ++ [v] }
    f mesh (Normal n) = 
        mesh { meshNormals = meshNormals mesh ++ [n] }
    f mesh (Face (decon -> (i, j, k))) = 
        mesh { meshFaces = meshFaces mesh ++ [i, j, k] }
    f mesh Comment = mesh 

objLineParser :: Parsec String () ObjLine
objLineParser = 
    parseVertex <|> 
    parseNormal <|> 
    parseFace <|> 
    parseComment

parseVertex :: Parsec String () ObjLine
parseVertex = do
    try $ string "v "
    many $ char ' ' 
    x <- parseFloat
    many1 $ char ' ' 
    y <- parseFloat
    many1 $ char ' ' 
    z <- parseFloat
    many $ char ' '
    return $ Vertex $ vec3 x y z

parseNormal :: Parsec String () ObjLine
parseNormal = do
    try $ string "vn"
    many1 $ char ' ' 
    x <- parseFloat
    many1 $ char ' ' 
    y <- parseFloat
    many1 $ char ' ' 
    z <- parseFloat
    many $ char ' '
    return $ Normal $ vec3 x y z

parseFace :: Parsec String () ObjLine
parseFace = do
    char 'f'
    many1 $ char ' ' 
    i <- parseInt
    many $ noneOf " "
    many1 $ char ' ' 
    j <- parseInt
    many $ noneOf " "
    many1 $ char ' ' 
    k <- parseInt
    many $ noneOf "\n"
    return $ Face $ vec3 (i-1) (j-1) (k-1)

parseComment :: Parsec String () ObjLine
parseComment = do
    char '#'
    many $ noneOf "\n"
    return Comment

parseInt :: Parsec String () (ConstExpr UInt)
parseInt = do
    int <- many digit
    return $ fromInteger $ read int

parseFloat :: Parsec String () (ConstExpr Float)
parseFloat = do
    str <- many1 $ oneOf "0123456789-.eE"
    case (readMaybe str :: Maybe Float) of
        Just float -> return $ (fromRational . toRational) float
        Nothing -> fail $ "Could not parse " ++ str ++ " as float" 
