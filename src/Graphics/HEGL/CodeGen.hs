module Graphics.HEGL.CodeGen (
    GLProgram(..),
    UniformVar(..), InpVar(..),
    genProgram
) where

import Control.Monad.State.Lazy (State, evalState, runState, get, gets, modify, unless)
import Foreign.Storable (Storable)
import qualified Data.Map as Map
import qualified Data.Set as Set

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

instance HasExprID UniformVar where
    getID (UniformVar id _) = id
instance Eq UniformVar where
    x1 == x2 = getID x1 == getID x2
instance Ord UniformVar where
    compare x1 x2 = compare (getID x1) (getID x2)

data InpVar where
    InpVar :: GLInputType t => 
        ExprID -> [GLExpr ConstDomain t] -> InpVar

instance HasExprID InpVar where
    getID (InpVar id _) = id
instance Eq InpVar where
    x1 == x2 = getID x1 == getID x2
instance Ord InpVar where
    compare x1 x2 = compare (getID x1) (getID x2)


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

addUniformVar :: UniformVar -> CGState ()
addUniformVar unif = modify (\s -> s { 
    program = (program s) { uniformVars = Set.insert unif $ uniformVars $ program s } })

addInputVar :: InpVar -> CGState ()
addInputVar unif = modify (\s -> s { 
    program = (program s) { inputVars = Set.insert unif $ inputVars $ program s } })


-- genProgram

genProgram :: GLObj -> GLProgram
genProgram glObj = evalState gen (initCGDat glObj) where 
    gen :: CGState GLProgram
    gen = do

        let position_ = toGLAST $ position glObj
            color_ = toGLAST $ color glObj

        posRef <- traverseGLAST position_
        colorRef <- traverseGLAST color_

        modifyShader VertexDomain $ addStmt $
            VarAsmt "gl_Position" posRef
        modifyShader FragmentDomain $ addDecl $
            OutDecl "fColor" "vec4"
        modifyShader FragmentDomain $ addStmt $
            VarAsmt "fColor" colorRef

        gets program


-- Traversal

traverseGLAST :: GLAST -> CGState ShaderExpr
traverseGLAST (GLASTAtom _ _ (Const x)) = return $ ShaderConst x
traverseGLAST (GLASTAtom id ti (Uniform x)) = mkGlobal id $ do
    addUniformVar $ UniformVar id x
    modifyShader (shaderType ti) $ addDecl $ 
        UniformDecl (idLabel id) (exprType ti)
traverseGLAST (GLASTAtom id ti (Inp xs)) = mkGlobal id $ do
    addInputVar $ InpVar id xs
    modifyShader (shaderType ti) $ addDecl $ 
        InpDecl (idLabel id) (exprType ti)
traverseGLAST (GLASTAtom id ti (Frag vertExpr)) = mkGlobal id $ do
    vertName <- traverseGLAST $ toGLAST vertExpr
    modifyShader VertexDomain $ addStmt $
        VarAsmt (idLabel id) vertName
    modifyShader VertexDomain $ addDecl $
        OutDecl (idLabel id) (exprType ti)
    modifyShader FragmentDomain $ addDecl $
        InpDecl (idLabel id) (exprType ti)
traverseGLAST (GLASTAtom _ _ FuncParam) = undefined

traverseGLAST (GLASTFuncApp _ _ r args params) = undefined

traverseGLAST (GLASTExpr id (GLTypeInfo shaderType exprType) exprName subnodes) = 
    mkLocal shaderType id $ do
        subexprs <- mapM traverseGLAST subnodes
        mkStmt shaderType $ VarDeclAsmt (idLabel id) exprType $
            ShaderExpr exprName subexprs

-- Scope management

innerScope :: ShaderDomain -> CGState () -> CGState ()
innerScope dom action = do
    scopeBefore <- getCurScope dom
    modify $ \s -> s { localScope = Just $ Scope (localDefs scopeBefore) [] }
    action
    scopeAfter <- getCurScope dom
    mapM (mkStmt dom) (localStmts scopeAfter)
    modifyCurScope dom $ const scopeBefore


newScope :: ShaderDomain -> CGState () -> CGState ()
newScope dom action = innerScope dom $ do
    modifyCurScope dom $ const emptyScope
    action

getCurScope :: ShaderDomain -> CGState Scope
getCurScope dom = do
    ls <- gets localScope
    case ls of
        Just ls -> return ls
        Nothing -> do
            scopes <- gets shaderScopes
            return $ Map.findWithDefault emptyScope dom scopes

modifyCurScope :: ShaderDomain -> (Scope -> Scope) -> CGState ()
modifyCurScope dom f = do
    ls <- gets localScope
    case ls of
        Just ls -> modify $ \s -> s { localScope = Just $ f ls }
        Nothing -> do
            modify $ \s -> s { shaderScopes = Map.adjust f dom $ shaderScopes s  }


-- Shader expression construction

mkLocal :: ShaderDomain -> ExprID -> CGState () -> CGState ShaderExpr
mkLocal dom id initFn = do
    locals <- localDefs <$> getCurScope dom
    unless (id `Set.member` locals) $ do 
        modifyCurScope dom $ \scope -> 
            scope { localDefs = Set.insert id $ localDefs scope }
        initFn
    return $ ShaderVarRef $ idLabel id

mkGlobal :: ExprID -> CGState () -> CGState ShaderExpr
mkGlobal id initFn = do
    globals <- gets globalDefs
    unless (id `Set.member` globals) $ do
        modify $ \s -> s { globalDefs = Set.insert id $ globalDefs s }
        initFn
    return $ ShaderVarRef $ idLabel id

mkFn :: ExprID -> CGState () -> CGState ()
mkFn = undefined

mkStmt :: ShaderDomain -> ShaderStmt -> CGState ()
mkStmt dom stmt = modifyCurScope dom $ \scope -> 
    scope { localStmts = localStmts scope ++ [stmt] }
