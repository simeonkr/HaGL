module Graphics.HEGL.CodeGen (
    GLProgram(..),
    UniformVar(..), InpVar(..),
    genProgram
) where

import Control.Monad.State.Lazy (State, evalState, runState, get, gets, modify, unless)
import Foreign.Storable (Storable)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import Graphics.HEGL.ExprID
import Graphics.HEGL.GLType
import Graphics.HEGL.GLExpr
import Graphics.HEGL.GLAst
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
    funcStack :: [(ExprID, [ShaderParam])],
    program :: GLProgram
}

initCGDat glObj = CGDat {
    globalDefs = Set.empty,
    shaderScopes = Map.fromList $ 
        [(dom, emptyScope) | dom <- shaderDomains],
    localScope = Nothing,
    funcStack = [],
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
        posRef <- traverseGLAst . toGLAst $ position glObj
        colorRef <- traverseGLAst . toGLAst $ color glObj

        modifyShader VertexDomain $ addStmt $
            VarAsmt "gl_Position" posRef
        modifyShader FragmentDomain $ addDecl $
            OutDecl "fColor" "vec4"
        modifyShader FragmentDomain $ addStmt $
            VarAsmt "fColor" colorRef

        gets program


-- Traversal

traverseGLAst :: GLAst -> CGState ShaderExpr
traverseGLAst (GLAstAtom _ _ (Const x)) = return $ ShaderConst x
traverseGLAst (GLAstAtom id ti (Uniform x)) = mkGlobal id $ do
    addUniformVar $ UniformVar id x
    modifyShader (shaderType ti) $ addDecl $ 
        UniformDecl (idLabel id) (exprType ti)
traverseGLAst (GLAstAtom id ti (Inp xs)) = mkGlobal id $ do
    addInputVar $ InpVar id xs
    modifyShader (shaderType ti) $ addDecl $ 
        InpDecl "" (idLabel id) (exprType ti)
traverseGLAst (GLAstAtom id ti (Frag interpType vertExpr)) = mkGlobal id $ do
    vertName <- traverseGLAst $ toGLAst vertExpr
    modifyShader VertexDomain $ addStmt $
        VarAsmt (idLabel id) vertName
    modifyShader VertexDomain $ addDecl $
        OutDecl (idLabel id) (exprType ti)
    modifyShader FragmentDomain $ addDecl $
        InpDecl (show interpType) (idLabel id) (exprType ti)
traverseGLAst (GLAstAtom id _ FuncParam) = 
    return $ ShaderVarRef $ idLabel id
traverseGLAst (GLAstFunc fnID ti (GLAstExpr _ _ "?:" [condExpr, retExpr, 
  (GLAstFuncApp _ _ (GLAstFunc fnId' _ _ _) recArgs)]) paramIDs) =
    let paramNames = map glastToParamName paramIDs
    in mkFn (shaderType ti) fnID paramNames $ do
        ((condName, updateStmts, retName, retStmts), condStmts) <- newScope (shaderType ti) $ do
            condName <- traverseGLAst condExpr
            (_, updateStmts) <- innerScope (shaderType ti) $ do
                argNames <- mapM traverseGLAst recArgs
                mapM_ (\(ShaderParam paramName _, argName) -> mkStmt (shaderType ti) $ 
                    VarAsmt paramName argName) $ zip paramNames argNames
            (retName, retStmts) <- innerScope (shaderType ti) $ traverseGLAst retExpr
            return (condName, updateStmts, retName, retStmts)
        parentParamNames <- getParentParamNames
        modifyShader (shaderType ti) $ addFn $
            ShaderLoopFn (idLabel fnID) (exprType ti) 
                (parentParamNames ++ paramNames)
                condName
                retName
                condStmts
                retStmts
                updateStmts
traverseGLAst (GLAstFunc fnID ti r paramIDs) =
    let paramNames = map glastToParamName paramIDs
    in mkFn (shaderType ti) fnID (map glastToParamName paramIDs) $ do
        parentParamNames <- getParentParamNames
        (rName, scopeStmts) <- newScope (shaderType ti) $ traverseGLAst r
        modifyShader (shaderType ti) $ addFn $
            ShaderFn (idLabel fnID) (exprType ti)
                (parentParamNames ++ paramNames)
                scopeStmts 
                rName
traverseGLAst (GLAstFuncApp callID ti fn args) = 
    mkLocal (shaderType ti) callID $ do
        parentArgNames <- map (\(ShaderParam name _) -> ShaderVarRef name) <$> getParentParamNames
        argNames <- mapM traverseGLAst args
        _ <- traverseGLAst fn
        mkStmt (shaderType ti) $ VarDeclAsmt (idLabel callID) (exprType ti)
            (ShaderExpr (idLabel $ getID fn) (parentArgNames ++ argNames))
traverseGLAst (GLAstExpr id ti exprName subnodes) = 
    mkLocal (shaderType ti) id $ do
        subexprs <- mapM traverseGLAst subnodes
        mkStmt (shaderType ti) $ VarDeclAsmt (idLabel id) (exprType ti) $
            ShaderExpr exprName subexprs

glastToParamName (GLAstAtom id ti FuncParam) = ShaderParam (idLabel id) (exprType ti)

-- Scope management

innerScope :: ShaderDomain -> CGState a -> CGState (a, [ShaderStmt])
innerScope dom action = do
    scopeBefore <- getCurScope dom
    modify $ \s -> s { localScope = Just $ Scope (localDefs scopeBefore) [] }
    res <- action
    scopeAfter <- getCurScope dom
    modifyCurScope dom $ const scopeBefore
    return $ (res, localStmts scopeAfter)


newScope :: ShaderDomain -> CGState a -> CGState (a, [ShaderStmt])
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

getParentParamNames :: CGState [ShaderParam]
getParentParamNames = concatMap snd <$> gets funcStack


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

mkFn :: ShaderDomain -> ExprID -> [ShaderParam] -> CGState () -> CGState ShaderExpr
mkFn dom id params initFn = do
    fns <- gets funcStack
    if id `List.elem` (map fst fns) then do
        error "Unsupported recursive function call"
    else do
        modify $ \s -> s { funcStack = (id, params) : funcStack s }
        res <- mkGlobal id initFn
        modify $ \s -> s { funcStack = tail $ funcStack s }
        return res

mkStmt :: ShaderDomain -> ShaderStmt -> CGState ()
mkStmt dom stmt = modifyCurScope dom $ \scope -> 
    scope { localStmts = localStmts scope ++ [stmt] }
