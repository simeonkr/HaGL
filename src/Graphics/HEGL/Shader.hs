module Graphics.HEGL.Shader (
    Shader(..),
    ShaderFn(..),
    ShaderParam(..),
    ShaderDecl(..),
    ShaderStmt(..),
    ShaderExpr(..),
    VarName,
    addFn,
    addDecl,
    addStmt
) where

import Data.Char (isAlpha)
import Data.List (intercalate)

import Graphics.HEGL.GLType


data Shader = Shader [ShaderFn] [ShaderDecl] [ShaderStmt]

data ShaderFn =
    ShaderFn FnName ExprType [ShaderParam] 
        [ShaderStmt] ShaderExpr |
    ShaderLoopFn FnName ExprType [ShaderParam] 
        ShaderExpr ShaderExpr [ShaderStmt] [ShaderStmt] [ShaderStmt]

data ShaderParam =
    ShaderParam VarName ExprType 

data ShaderDecl = 
    UniformDecl VarName ExprType |
    InpDecl InpQual VarName ExprType |
    OutDecl VarName ExprType

data ShaderStmt = 
    VarAsmt VarName ShaderExpr |
    VarDecl VarName ExprType |
    VarDeclAsmt VarName ExprType ShaderExpr

data ShaderExpr where
    ShaderConst :: GLType t => t -> ShaderExpr
    ShaderVarRef :: VarName -> ShaderExpr
    ShaderExpr :: String -> [ShaderExpr] -> ShaderExpr

type InpQual = String
type ExprType = String
type FnName = String
type VarName = String

instance Show Shader where
    show (Shader fns decls stmts) =
        "#version 430 core\n\n" ++
        (if null decls then "" 
         else concatMap (\s -> show s ++ "\n") decls ++ "\n") ++
        concatMap (\s -> show s ++ "\n\n") fns ++
        "void main() {\n" ++
        concatMap (\s -> "  " ++ show s ++ "\n") stmts ++
        "}"

instance Show ShaderFn where
    show (ShaderFn name retType params stmts ret) =
        retType ++ " " ++ name ++ "(" ++
        intercalate ", " (map show params) ++ ") {\n" ++
        concatMap (\s -> "  " ++ show s ++ "\n") stmts ++
        "  return " ++ show ret ++ ";" ++
        "\n}"
    show (ShaderLoopFn name retType params cond ret condStmts retStmts updateStmts) =
        retType ++ " " ++ name ++ "(" ++
        intercalate ", " (map show params) ++ ") {\n" ++
        "  while (true) {\n" ++
        concatMap (\s -> "      " ++ show s ++ "\n") condStmts ++ 
        "      if (" ++ show cond ++ ") {\n" ++
        concatMap (\s -> "        " ++ show s ++ "\n") retStmts ++ 
        "        return " ++ show ret ++ ";\n" ++ 
        "      }\n" ++
        concatMap (\s -> "      " ++ show s ++ "\n") updateStmts ++
        "  }\n}"

instance Show ShaderParam where
    show (ShaderParam name exprType) =
        exprType ++ " " ++ name

instance Show ShaderDecl where
    show (UniformDecl varName exprType) = 
        "uniform " ++ exprType ++ " " ++ varName ++ ";"
    show (InpDecl qual varName exprType) = 
        qual ++ " in " ++ exprType ++ " " ++ varName ++ ";"
    show (OutDecl varName exprType) = 
        "out " ++ exprType ++ " " ++ varName ++ ";"

instance Show ShaderStmt where
    show (VarAsmt varName expr) = 
        varName ++ " = " ++ show expr ++ ";"
    show (VarDecl varName exprType) =
        exprType ++ " " ++ varName ++ ";" 
    show (VarDeclAsmt varName exprType expr) = 
        exprType ++ " " ++ varName ++ " = " ++ show expr ++ ";"

instance Show ShaderExpr where
    show (ShaderExpr funcName xs)
        | isAlpha (head funcName) = 
            funcName ++ "(" ++ intercalate ", " (map show xs) ++ ")"
        | head funcName == '.' = showCompSel funcName xs
        | funcName == "[]" = showSubscript xs
        | funcName == "?:" = showTernCond xs
        | otherwise = showInfix funcName xs
        where 
            showCompSel comp [x] = show x ++ comp
            showSubscript [arr, i] = show arr ++ "[" ++ show i ++ "]"
            showTernCond [x, y, z] = show x ++ " ? " ++ show y ++ " : " ++ show z
            showInfix op [x] = op ++ show x
            showInfix op xs = intercalate (" " ++ op ++ " ") (map show xs)

addFn :: ShaderFn -> Shader -> Shader
addFn fn (Shader fns decls stmts) =
    Shader (fns ++ [fn]) decls stmts

addDecl :: ShaderDecl -> Shader -> Shader
addDecl decl (Shader fns decls stmts) =
    Shader fns (decls ++ [decl]) stmts

addStmt :: ShaderStmt -> Shader -> Shader
addStmt stmt (Shader fns decls stmts) =
    Shader fns decls (stmts ++ [stmt])
