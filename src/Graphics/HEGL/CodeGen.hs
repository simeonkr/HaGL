module Graphics.HEGL.CodeGen (
    GLProgram(..),
    UniformVar(..), InpVar(..),
    genProgram
) where

import Control.Monad.State.Lazy (State, evalState, runState, get, gets, modify)
import Data.Map as Map
import Data.Set as Set
import Foreign.Storable

import Graphics.HEGL.ExprID
import Graphics.HEGL.GLType
import Graphics.HEGL.GLExpr
import Graphics.HEGL.GLAST
import Graphics.HEGL.GLObj
import Graphics.HEGL.Shader


-- GLProgram = output of code gen for a GLObj

data GLProgram = GLProgram {
    primitiveMode :: PrimitiveMode,
    indices :: Maybe [ConstExpr UInt],
    uniformVars :: Set.Set UniformVar,
    inputVars :: Set.Set InpVar,
    vertexShader :: Shader,
    fragmentShader :: Shader
}

data UniformVar where
    UniformVar :: GLType t => ExprID -> GLExpr HostDomain t -> UniformVar

data InpVar where
    InpVar :: (GLInputType t, GLType (GLElt t), Storable (GLElt t)) => 
        ExprID -> [GLExpr ConstDomain t] -> InpVar


-- Intermediate code gen state

data CGDat = CGDat {
    globalDefs :: Set.Set ExprID, 
    shaderScopes :: Map.Map ShaderDomain Scope,
    localScope :: Maybe Scope,
    funcs :: Set.Set ExprID,
    funcParams :: Set.Set ExprID,
    program :: GLProgram
}

initCGDat glObj = CGDat {
    globalDefs = Set.empty,
    shaderScopes = Map.fromList $ 
        [(dom, emptyScope) | dom <- shaderDomains],
    localScope = Nothing,
    funcs = Set.empty,
    funcParams = Set.empty,
    program = GLProgram {
        Graphics.HEGL.CodeGen.primitiveMode = 
            Graphics.HEGL.GLObj.primitiveMode glObj,
        Graphics.HEGL.CodeGen.indices = 
            Graphics.HEGL.GLObj.indices glObj,
        uniformVars = Set.empty,
        inputVars = Set.empty,
        vertexShader = Shader [] [] [],
        fragmentShader = Shader [] [] []
    }
}

data Scope = Scope {
    localDefs :: Set.Set ExprID,
    localStmts :: [ShaderStmt]
}

emptyScope :: Scope
emptyScope = Scope Set.empty []

type CGState = State CGDat

getScope :: ShaderDomain -> CGState Scope
getScope dom = Map.findWithDefault emptyScope dom <$> gets shaderScopes

modifyShader :: ShaderDomain -> (Shader -> Shader) -> CGState ()
modifyShader VertexDomain f = modify (\s -> s { 
    program = (program s) { vertexShader = f $ vertexShader $ program s } })
modifyShader FragmentDomain f = modify (\s -> s { 
    program = (program s) { fragmentShader = f $ fragmentShader $ program s } })


-- genProgram

genProgram :: GLObj -> GLProgram
genProgram glObj = evalState gen (initCGDat glObj) where 
    gen :: CGState GLProgram
    gen = do

        let position_ = toGLAST $ position glObj
            color_ = toGLAST $ color glObj

        traverseGLAST position_
        traverseGLAST color_

        modifyShader VertexDomain $ addStmt $
            VarAsmt "gl_Position" (ShaderVarRef $ varName $ position_)
        modifyShader FragmentDomain $ addDecl $
            OutDecl "fColor" "vec4"
        modifyShader FragmentDomain $ addStmt $
            VarAsmt "fColor" (ShaderVarRef $ varName $ color_)

        gets program


-- Traversal

traverseGLAST :: GLAST -> CGState ()
traverseGLAST (GLASTAtom _ _ _) = undefined
traverseGLAST (GLASTExpr id ti exprName subexprs) = undefined


-- Scope management

innerScope :: CGState () -> CGState [ShaderStmt]
innerScope = undefined

newScope :: CGState () -> CGState [ShaderStmt]
newScope = undefined


-- Shader expression construction

varName :: GLAST -> VarName
varName e = "x" ++ show (glastID e)

mkLocal :: ExprID -> CGState () -> CGState ()
mkLocal = undefined

mkGlobal :: ExprID -> CGState () -> CGState ()
mkGlobal = undefined

mkFn :: ExprID -> CGState () -> CGState ()
mkFn = undefined

-- if localScope == Nothing, directly add statement to the right main scope,
-- else add to localScope
mkStmt :: ShaderDomain -> ShaderStmt -> CGState ()
mkStmt dom stmt = undefined
