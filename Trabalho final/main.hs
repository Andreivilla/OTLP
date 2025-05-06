module Main where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Expr
import Data.Functor.Identity (Identity)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

-- Identificador de variável
type Id = String

-- Padrões (Pat)
data Pat = PVar Id
          | PLit Literal
          | PCon Id [Pat]
          deriving (Eq, Show)

-- Literais
data Literal = LitInt Integer
             | LitBool Bool
             deriving (Show, Eq)

-- Expressões (Expr)
data Expr = Var Id
          | Const Id
          | App Expr Expr
          | Lam Id Expr
          | Lit Literal
          | If Expr Expr Expr
          | Case Expr [(Pat, Expr)]
          | Let (Id, Expr) Expr
          | Tuple Expr Expr
          deriving (Eq, Show)

-- Tipos Simples (SimpleType)
data SimpleType = TVar Id
                | TArr SimpleType SimpleType
                | TCon String
                | TApp SimpleType SimpleType
                | TGen Int
                deriving (Eq)

instance Show SimpleType where
    show (TVar varId) = varId
    show (TArr t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"
    show (TCon s) = s
    show (TApp t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
    show (TGen n) = "t" ++ show n

-- Construtores iniciais (contexto inicial)
data Binding = Id :>: SimpleType deriving (Show, Eq)

iniCont :: [Binding]
iniCont = [
    "(,)" :>: TArr (TGen 0) (TArr (TGen 1) (TApp (TApp (TCon "(,)") (TGen 0)) (TGen 1))),
    "True" :>: TCon "Bool",
    "False" :>: TCon "Bool"
    ]

-- Definindo a linguagem
langDef :: Tok.LanguageDef ()
langDef = emptyDef {
    Tok.commentStart = "/*",
    Tok.commentEnd = "*/",
    Tok.commentLine = "//",
    Tok.identStart = letter,
    Tok.identLetter = alphaNum,
    Tok.reservedNames = ["let", "in", "case", "of", "if", "then", "else", "True", "False"],
    Tok.reservedOpNames = ["\\", "->", ":>:"]
}

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

-- Funções auxiliares para parsing
identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

braces :: Parser a -> Parser a
braces = Tok.braces lexer

integer :: Parser Integer
integer = Tok.integer lexer

symbol :: String -> Parser String
symbol = Tok.symbol lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

-- Parsers para literais
literal :: Parser Literal
literal = (LitInt <$> integer)
      <|> (LitBool True <$ reserved "True")
      <|> (LitBool False <$ reserved "False")

-- Parsers para padrões (Pat)
pat :: Parser Pat
pat = try (do
        constructor <- identifier
        args <- option [] (parens (commaSep pat))
        return (PCon constructor args))
      <|> (PVar <$> identifier)
      <|> (PLit <$> literal)

patExpr :: Parser (Pat, Expr)
patExpr = do
    p <- pat
    reservedOp "->"
    e <- expr
    return (p, e)

lPat :: Parser [(Pat, Expr)]
lPat = patExpr `sepBy` symbol ";"

-- Parsers para expressões (Expr)
expr :: Parser Expr
expr = buildExpressionParser table term

term :: Parser Expr
term = try (parens $ do
              e1 <- expr
              symbol ","
              e2 <- expr
              return $ Tuple e1 e2)
    <|> parens expr
    <|> (Var <$> identifier)
    <|> (Const <$> identifier)
    <|> (Lam <$> (reservedOp "\\" *> identifier) <*> (reservedOp "->" *> expr))
    <|> (Lit <$> literal)
    <|> (If <$> (reserved "if" *> expr)
            <*> (reserved "then" *> expr)
            <*> (reserved "else" *> expr))
    <|> (Let <$> (reserved "let" *> ((,) <$> identifier <*> (reservedOp "=" *> expr)))
             <*> (reserved "in" *> expr))
    <|> (Case <$> (reserved "case" *> expr)
              <*> (reserved "of" *> braces lPat))

table :: [[Operator String () Identity Expr]]
table = [[Infix (App <$ whiteSpace) AssocLeft]]

-- Função principal de parsing
parseExpr :: String -> Either ParseError Expr
parseExpr = parse (whiteSpace *> expr <* eof) "<stdin>"

-- Ambiente de Tipos
type TypeEnv = [(Id, SimpleType)]

-- Função de Unificação
type Subst = [(Id, SimpleType)]

nullSubst :: Subst
nullSubst = []

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = [(u, apply s2 t) | (u, t) <- s1] ++ s2

class Types a where
    apply :: Subst -> a -> a
    ftv :: a -> [Id]

instance Types SimpleType where
    apply _ (TCon a) = TCon a
    apply s t@(TVar a) = case lookup a s of
                           Nothing -> t
                           Just t' -> t'
    apply s (TArr t1 t2) = TArr (apply s t1) (apply s t2)
    apply s (TApp t1 t2) = TApp (apply s t1) (apply s t2)
    apply s (TGen n) = TGen n

    ftv (TVar a) = [a]
    ftv (TArr t1 t2) = ftv t1 ++ ftv t2
    ftv (TApp t1 t2) = ftv t1 ++ ftv t2
    ftv (TCon _) = []
    ftv (TGen _) = []

unify :: SimpleType -> SimpleType -> Infer Subst
unify (TArr l r) (TArr l' r') = do
    s1 <- unify l l'
    s2 <- unify (apply s1 r) (apply s1 r')
    return (s2 `composeSubst` s1)
unify (TVar u) t = varBind u t
unify t (TVar u) = varBind u t
unify (TCon a) (TCon b)
    | a == b = return nullSubst
unify (TApp l r) (TApp l' r') = do
    s1 <- unify l l'
    s2 <- unify (apply s1 r) (apply s1 r')
    return (s2 `composeSubst` s1)
unify t1 t2 = throwError $ "types do not unify: " ++ show t1 ++ " vs. " ++ show t2

varBind :: Id -> SimpleType -> Infer Subst
varBind u t
    | t == TVar u = return nullSubst
    | u `elem` ftv t = throwError $ "occur check fails: " ++ u ++ " vs. " ++ show t
    | otherwise = return [(u, t)]

-- Função de Inferência de Tipos
type Infer a = ExceptT String (ReaderT TypeEnv (StateT Int IO)) a

runInfer :: TypeEnv -> Infer a -> IO (Either String a)
runInfer env m = evalStateT (runReaderT (runExceptT m) env) 0

infer :: Expr -> Infer SimpleType
infer (Var x) = do
    env <- ask
    case lookup x env of
      Nothing -> throwError $ "Unbound variable: " ++ x
      Just sigma -> return sigma
infer (Lit (LitInt _)) = return $ TCon "Int"
infer (Lit (LitBool _)) = return $ TCon "Bool"
infer (Lam x e) = do
    tv <- fresh
    t <- local ((x, tv):) (infer e)
    return $ TArr tv t
infer (App e1 e2) = do
    t1 <- infer e1
    t2 <- infer e2
    tv <- fresh
    s1 <- unify (TArr t2 tv) t1
    return $ apply s1 tv
infer (If cond tr fl) = do
    tCond <- infer cond
    s1 <- unify tCond (TCon "Bool")
    tTr <- infer tr
    tFl <- infer fl
    s2 <- unify tTr tFl
    return $ apply s2 tTr
infer (Let (x, e1) e2) = do
    t1 <- infer e1
    t2 <- local ((x, t1):) (infer e2)
    return t2
infer (Tuple e1 e2) = do
    t1 <- infer e1
    t2 <- infer e2
    return $ TApp (TApp (TCon "(,)") t1) t2
infer (Case e cs) = do
    te <- infer e
    t <- inferCase cs
    return t

inferCase :: [(Pat, Expr)] -> Infer SimpleType
inferCase [] = throwError "empty case"
inferCase ((p, e):cs) = do
    env <- ask
    t <- infer e
    case p of
      PVar x -> do
          t' <- local ((x, t):) (inferCase cs)
          return t'
      PLit l -> do
          lType <- infer (Lit l)
          s1 <- unify lType t
          inferCase cs
      PCon con pats -> throwError "Constructor patterns not supported yet"

fresh :: Infer SimpleType
fresh = do
    n <- get
    modify (+1)
    return $ TVar $ "a" ++ show n

-- Função principal para testar o parser e o analisador de tipos
main :: IO ()
main = do
    putStrLn "Digite um termo:"
    input <- getLine
    case parseExpr input of
        Left err -> print err
        Right expr -> do
            result <- runInfer (map (\(id :>: ty) -> (id, ty)) iniCont) (infer expr)
            case result of
                Left err -> putStrLn $ "Type error: " ++ err
                Right ty -> putStrLn $ "Type: " ++ show ty
    main
