module Graphics.HEGL.CodeGen (
    GLProgram(..),
    UniformVar(..), InpVar(..),
    genProgram
) where

import Prelude hiding (id)
import Control.Monad.State.Lazy (State, evalState, get, gets, modify, unless)
import Control.Exception (throw)
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

instance Show GLProgram where
    show glProg = {-}"\n" ++
        concatMap (\s -> show s ++ "\n") 
            (Set.toList $ inputVars glProg) ++ 
        "========\n\n" ++
        concatMap (\s -> show s ++ "\n") 
            (Set.toList $ uniformVars glProg) ++ 
        "========\n\n" ++-}
        List.intercalate "\n\n" (map show 
            [vertexShader glProg, 
             fragmentShader glProg])


-- Intermediate code gen state

data CGDat = CGDat {
    globalDefs :: Set.Set ExprID, 
    scopes :: Map.Map ScopeID Scope,
    funcStack :: [(ExprID, [ShaderParam])],
    program :: GLProgram
}

initCGDat glObj = CGDat {
    globalDefs = Set.empty,
    scopes = Map.fromList $
        [(MainScope dom, emptyScope) | dom <- shaderDomains] ++
        [(GlobalScope, emptyScope), (LocalScope, emptyScope)],
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

data ScopeID =
    MainScope ShaderDomain |
    GlobalScope |
    LocalScope
    deriving (Eq, Ord)

data Scope = Scope {
    scopeExprs :: Set.Set ExprID,
    scopeStmts :: [ShaderStmt]
}

emptyScope :: Scope
emptyScope = Scope Set.empty []

type CGState = State CGDat


-- genProgram

genProgram :: GLObj -> GLProgram
genProgram glObj = evalState gen (initCGDat glObj) where 
    gen :: CGState GLProgram
    gen = do
        posRef <- traverseGLExpr $ position glObj
        colorRef <- traverseGLExpr $ color glObj
        discardRef <- traverseGLExpr $ discardWhen glObj

        vertStmts <- scopeStmts <$> getScope (MainScope VertexDomain)
        mapM_ (modifyShader VertexDomain . addStmt) vertStmts
        modifyShader VertexDomain $ addStmt $
            VarAsmt "gl_Position" posRef

        fragStmts <- scopeStmts <$> getScope (MainScope FragmentDomain)
        mapM_ (modifyShader FragmentDomain . addStmt) fragStmts            
        modifyShader FragmentDomain $ addDecl $
            OutDecl "" "fColor" "vec4"
        modifyShader FragmentDomain $ addStmt $
            VarAsmt "fColor" colorRef
        modifyShader FragmentDomain $ addStmt $
            DiscardStmt discardRef

        gets program


-- Traversal

traverseGLExpr :: IsGLDomain d => GLExpr d t -> CGState ShaderExpr
traverseGLExpr glExpr = let glAst = toGLAst glExpr in
    traverseGLAst (MainScope $ getShaderType glExpr) glAst

traverseGLAst :: ScopeID -> GLAst -> CGState ShaderExpr
traverseGLAst _ (GLAstAtom _ _ (Const x)) = 
    return $ ShaderConst x
traverseGLAst _ (GLAstAtom id _ GenVar) = 
    return $ ShaderVarRef $ idLabel id
traverseGLAst _ (GLAstAtom id ti (Uniform x)) = 
    ifUndef GlobalScope id $ do
        addUniformVar $ UniformVar id x
        modifyShader (shaderType ti) $ addDecl $ 
            UniformDecl (idLabel id) (exprType ti)
traverseGLAst _ (GLAstAtom id ti (Inp xs)) = 
    ifUndef GlobalScope id $ do
        addInputVar $ InpVar id xs
        modifyShader (shaderType ti) $ addDecl $ 
            InpDecl "" (idLabel id) (exprType ti)
traverseGLAst _ (GLAstAtom id ti (Frag interpType x)) = 
    ifUndef GlobalScope id $ do
        vertExpr <- traverseGLAst (MainScope VertexDomain) $ toGLAst x
        scopedStmt (MainScope VertexDomain) $
            VarAsmt (idLabel id) vertExpr
        modifyShader VertexDomain $ addDecl $
            OutDecl (show interpType) (idLabel id) (exprType ti)
        modifyShader FragmentDomain $ addDecl $
            InpDecl (show interpType) (idLabel id) (exprType ti)
traverseGLAst _ (GLAstFunc fnID ti (GLAstExpr _ _ "?:" [cond, ret, 
  GLAstFuncApp _ _ (GLAstFunc fnID' _ _ _) recArgs]) params) | fnID == fnID' =
    defFn fnID params $ \parentParamExprs paramExprs -> do
        ((condExpr, updateStmts, retExpr, retStmts), condStmts) <- localScope $ do
            condExpr <- traverseGLAst LocalScope cond
            (_, updateStmts) <- innerScope $ do
                argExprs <- mapM (traverseGLAst LocalScope) recArgs
                mapM_ (\(ShaderParam paramName _, argName) -> scopedStmt LocalScope $ 
                    VarAsmt paramName argName) $ zip paramExprs argExprs
            (retExpr, retStmts) <- innerScope $ traverseGLAst LocalScope ret
            return (condExpr, updateStmts, retExpr, retStmts)
        modifyShader (shaderType ti) $ addFn $
            ShaderLoopFn (idLabel fnID) (exprType ti) 
                (parentParamExprs ++ paramExprs)
                condExpr
                retExpr
                condStmts
                retStmts
                updateStmts
traverseGLAst _ (GLAstFunc fnID ti r params) =
    defFn fnID params $ \parentParamExprs paramExprs -> do
        (rExpr, scopeStmts) <- localScope $ traverseGLAst LocalScope r
        modifyShader (shaderType ti) $ addFn $
            ShaderFn (idLabel fnID) (exprType ti)
                (parentParamExprs ++ paramExprs)
                scopeStmts 
                rExpr
traverseGLAst scopeID (GLAstFuncApp callID ti fn args) = 
    ifUndef scopeID callID $ do
        parentArgExprs <- map (\(ShaderParam name _) -> ShaderVarRef name) <$> 
            concatMap snd <$> gets funcStack
        argExprs <- mapM (traverseGLAst scopeID) args
        _ <- traverseGLAst LocalScope fn
        scopedStmt scopeID $ VarDeclAsmt (idLabel callID) (exprType ti)
            (ShaderExpr (idLabel $ getID fn) (parentArgExprs ++ argExprs))
traverseGLAst scopeID (GLAstExpr id ti exprName subnodes) =
    ifUndef scopeID id $ do
        subexprs <- mapM (traverseGLAst scopeID) subnodes
        scopedStmt scopeID $ VarDeclAsmt (idLabel id) (exprType ti) $
            ShaderExpr exprName subexprs


-- Scope management

localScope :: CGState a -> CGState (a, [ShaderStmt])
localScope action = innerScope $ do
    modifyScope LocalScope $ const emptyScope
    action

innerScope :: CGState a -> CGState (a, [ShaderStmt])
innerScope action = do
    scopeBefore <- getScope LocalScope
    modifyScope LocalScope $ \scope -> scope { scopeStmts = [] }
    res <- action
    scopeAfter <- getScope LocalScope
    modifyScope LocalScope $ const scopeBefore
    return (res, scopeStmts scopeAfter)

getScope :: ScopeID -> CGState Scope
getScope scopeID = do
    scopes <- gets scopes
    return $ Map.findWithDefault emptyScope scopeID scopes

modifyScope :: ScopeID -> (Scope -> Scope) -> CGState ()
modifyScope scopeID f = do
    modify $ \s -> s { scopes = Map.adjust f scopeID $ scopes s }


-- Shader expression construction

ifUndef :: ScopeID -> ExprID -> CGState () -> CGState ShaderExpr
ifUndef scopeID id initFn = do
    locals <- scopeExprs <$> getScope scopeID
    unless (id `Set.member` locals) $ do 
        modifyScope scopeID $ \scope -> 
            scope { scopeExprs = Set.insert id $ scopeExprs scope }
        initFn
    return $ ShaderVarRef $ idLabel id

defFn :: ExprID -> [GLAst] -> ([ShaderParam] -> [ShaderParam] -> CGState ()) -> CGState ShaderExpr
defFn id params initFn = do
    fns <- gets funcStack
    if id `List.elem` map fst fns then
        throw UnsupportedRecCall
    else do
        let parentParamExprs = concatMap snd fns
            glastToParamExpr (GLAstAtom id ti GenVar) = 
                ShaderParam (idLabel id) (exprType ti)
            paramExprs = map glastToParamExpr params
        modify $ \s -> s { funcStack = (id, paramExprs) : funcStack s }
        res <- ifUndef GlobalScope id (initFn parentParamExprs paramExprs)
        modify $ \s -> s { funcStack = tail $ funcStack s }
        return res

scopedStmt :: ScopeID -> ShaderStmt -> CGState ()
scopedStmt scopeID stmt = modifyScope scopeID $ \scope -> 
    scope { scopeStmts = scopeStmts scope ++ [stmt] }


-- Shader modification

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
