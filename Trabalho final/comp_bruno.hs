import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Combinator
import qualified Text.Parsec.Token as T
import Text.Parsec.Language
import System.IO

--Bruno Marchi Pires

type Id = String

data Tipo = TDouble | TInt | TString | TVoid deriving Show

data TCons = CDouble Double | CInt Int deriving Show

data Expr = Expr :+: Expr | Expr :-: Expr | Expr :*: Expr | Expr :/: Expr | Neg Expr | Const TCons | IdVar Id | Chamada Id [Expr] | Lit String deriving Show

data ExprR = Expr :==: Expr | Expr :/=: Expr | Expr :<: Expr | Expr :>: Expr | Expr :<=: Expr | Expr :>=: Expr deriving Show

data ExprL = ExprL :&: ExprL | ExprL :|: ExprL | Not ExprL | Rel ExprR deriving Show

data Var = Id :#: Tipo deriving Show

data Funcao = Id :->: ([Var], Tipo) deriving Show

data Programa = Prog [Funcao] [(Id, [Var], Bloco)] [Var] Bloco deriving Show

type Bloco = [Comando]

data Comando = If ExprL Bloco Bloco | While ExprL Bloco | Atrib Id Expr | Leitura Id | Imp Expr | Ret (Maybe Expr) | Proc Id [Expr] deriving Show

lingDef = emptyDef
          { T.commentStart      = "{-"
            , T.commentEnd      = "-}"
            , T.commentLine     = "--"
            , T.identStart      = letter <|> char '_'
            , T.identLetter     = alphaNum <|> char '_'
            , T.reservedNames   = ["while", "return", "if", "else", "print", "read","int", "string", "double", "void"]
            , T.reservedOpNames = ["+", "-", "/", "*", "==", "/=", "<", ">", "<=", ">=", "&&", "||", "!", "="]
          }  

lexico = T.makeTokenParser lingDef

symbol        = T.symbol lexico
parens        = T.parens lexico
reserved      = T.reserved lexico
reservedOp    = T.reservedOp lexico
natural       = T.natural lexico
float         = T.float lexico
stringLiteral = T.stringLiteral lexico
integer       = T.integer lexico
charLiteral   = T.charLiteral lexico
semi          = T.semi lexico
comma         = T.comma lexico
identifier    = T.identifier lexico

--Operações Relacionais
op = do { reservedOp "==" >> return (:==:)}
    <|> do { reservedOp "/=" >> return (:/=:)}
    <|> do { reservedOp "<=" >> return (:<=:)}
    <|> do { reservedOp ">=" >> return (:>=:)}
    <|> do { reservedOp "<" >> return (:<:)}
    <|> do { reservedOp ">" >> return (:>:)}
    <?> "op é inválido !"

exprR = try(do {e1<-expr; o <- op; e2<-expr; return (o e1 e2)}) 

--Facilitadores
binario  name fun assoc = Infix (do{reservedOp name; return fun }) assoc
prefix   name fun       = Prefix (do{reservedOp name; return fun })

--Tabela Operações Lógicas
tabelaLogic= [[prefix "!" (Not)]
            , [binario "&&" (:&:) AssocLeft]
            , [binario "||" (:|:) AssocLeft]
           ]

exprL = buildExpressionParser tabelaLogic logicTerms
        <?> "logic expression"

logicTerms =  parens exprL
          <|> do { e <- exprR; return (Rel e)}

--Tabela Operações Aritméticas
tabelaArit= [[prefix "-" (Neg)]
            , [binario "*" (:*:) AssocLeft, binario "/" (:/:) AssocLeft ]
            , [binario "+" (:+:) AssocLeft, binario "-" (:-:)   AssocLeft ]
           ]

fator = parens expr
       <|> do { str <- stringLiteral; return (Lit str)}
       <|> do { idv<-identifier; return (IdVar idv)}
       <|> try (do { db <- float; return (Const (CDouble db))})
       <|> try (do { n <- natural; return (Const (CInt (fromIntegral n)))})
       <?> "simple expression"

expr = buildExpressionParser tabelaArit fator
       <?> "expression"   

--Produção Programa
programa = do
  listaPrototipos <- many prototipo
  (declaracoes, lsCmd) <- blocoPrincipal
  listaFuncoes <- many funcao
  return (Prog listaPrototipos listaFuncoes declaracoes lsCmd)

--Produção Função
funcao = do
  tipoRetorno <- tipoRetorno
  nome <-identifier
  parametros <- parens declParametros
  (dcl, lscmd) <- blocoPrincipal
  return (nome, dcl, lscmd)
  
prototipo = do
  tipoRetorno <- tipoRetorno
  nome <- identifier
  parametros <- parens declParametros
  semi
  return (nome :->: (parametros, tipoRetorno))

--Produção Tipo
tipo = do {reserved "int"; return TInt}
        <|> do {reserved "string"; return TString}
        <|> do {reserved "double"; return TDouble}

tipoRetorno = do {reserved "int"; return TInt}
        <|> do {reserved "string"; return TString}
        <|> do {reserved "double"; return TDouble}
        <|> do {reserved "void"; return TVoid}

--Produção <Bloco>
bloco = do 
    symbol "{"
    cs <- listaCmd
    symbol "}"
    return cs

--Produção <Bloco Principal>
blocoPrincipal = do 
    symbol "{"
    bp'<- blocoPrincipal'
    symbol "}"
    return bp'

blocoPrincipal' = do
    dcl <- declaracoes
    lsCmd <- listaCmd
    return (dcl,lsCmd)

-- Produção <Declaracoes>
declaracoes = do { tp <- tipo; lst <- listaId; semi; resto <- declaracoes; return (map (\id -> id :#: tp) lst ++ resto)}
                <|> return []

-- Produção <ListaId>
listaId = do
  id <- identifier
  listaIdResto <- listaId'
  return (id : listaIdResto)

-- Produção <ListaId'>
listaId' = do
  symbol ","
  listaId
  <|> return []

--Produção <DeclParametros>
declParametros = option [] $ do --option reconhece vazio
    tipo <- tipo
    nome <- identifier
    resto <- parametros
    return $ (nome :#: tipo) : resto

--parametros :: Parsec String u [(Tipo, Id)]
parametros = option [] $ do
    p <- declParametros
    ps <- many (comma >> declParametros)
    return (p ++ concat ps)

--Produção <ListaCmd>
listaCmd = do { c <- comando; cs<-listaCmd; return (c:cs)}
            <|> return []

--Produção <Senao> 
elseCmd = do { reserved "else"; pbl <-bloco; return pbl}
          <|> return []

--Comandos    
comando = do {reserved "return"; e<- optionMaybe expr; semi; return (Ret e)}
            <|> do { reserved "while"; l <- parens exprL; b <- bloco; return (While l b)}
            <|> try (do { reserved "if"; cond <- parens exprL; pbl<- bloco; sbl <- elseCmd; return (If cond pbl sbl)})
            <|> do { reserved "print"; string <- parens expr; semi; return (Imp string)}
            <|> do { reserved "read"; v <- parens identifier; semi; return (Leitura v) }
            <|> try (do { v <- identifier; reserved "="; e <- expr; semi; return (Atrib v e)})
            <|> try (do { v <- identifier; reserved "="; (funcName, args) <- chamadaFuncao; semi; return (Proc funcName args)})
            <|> try (do { (funcName, args) <- chamadaFuncao; semi; return (Proc funcName args)})

chamadaFuncao = do
  id <- identifier
  args <- parens listaParametros
  return (id, args)

listaParametros = listaParametros' <|> return []

listaParametros' = do
  e <- expr
  es <- listaParametros''
  return (e : es)

listaParametros'' = do
  symbol ","
  listaParametros'
  <|> return []

partida :: Parsec String u Programa
partida = do {e <- programa; eof; return e}

parserE e = runParser partida [] "Expressoes" e

parserExpr s = case parserE s of
                     Left er -> print er
                     Right v -> (print v)
                     
main = do putStr "\n Lendo Arquivo...\n"
          handle <- openFile "entrada.txt" ReadMode
          contents <- hGetContents handle
          parserExpr contents
          hClose handle