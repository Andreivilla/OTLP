import Text.ParserCombinators.Parsec

data Type = TypeInt
        | TypeVar Name
        | TypeArrow Type Type
        deriving Show

type Name = String

type Unifier = [(Name, Type)]

typeint :: Parser Type
typeint = do
    string "Int"
    return(TypeInt)

typevar :: Parser Type
typevar = do
    name <- many1 lower
    return (TypeVar name)

typearrow :: Parser Type
typearrow = do
    t1 <- atom
    string "->"
    t2 <- typee
    return(TypeArrow t1 t2)

typee :: Parser Type
typee =
    try typearrow <|> atom

atom :: Parser Type
atom =
    typevar <|> typeint <|> paren

paren :: Parser Type
paren = do
    char '('
    t <- typee
    char ')'
    return t

main :: IO ()
main = do
    putStrLn "Digite um termo:"
    a <- getLine
    let Right ta = parse typee "<stdin>" a

    --
    putStrLn "Digite outro termo:"

    -- Bug do repl.it! Lê uma linha extra...
    --getLine -- Remova se compilar localmente...

    b <- getLine
    -- Assume que o parsing deu certo!
    let Right tb = parse typee "<stdin>" b
    --
    putStrLn "Unificação:"
    --print ta
    --print tb
    print $ unify ta tb


{-
  A função principal para unificação receberá dois tipos e tentará retornar um unificador mais geral na forma de Just mgu,
  retornando Nothing caso não seja possível unificar.
-}
unify :: Type -> Type -> Maybe Unifier
unify (TypeVar a) (TypeVar b) | a == b = Just []
unify TypeInt TypeInt = Just []
unify (TypeVar a) b = if occursCheck a b then Nothing else Just [(a, b)]
unify b (TypeVar a) = if occursCheck a b then Nothing else Just [(a, b)]
unify (TypeArrow a b) (TypeArrow x y) =
    case unify a x of
        Just t1 ->
            case unify (subst t1 b) (subst t1 y) of
                Just t2 ->
                    Just (compose t2 t1)
                Nothing ->
                    Nothing
        Nothing ->
            Nothing
unify _ _ = Nothing


{-
  A fim de se testar o sistema, uma função deve ser implementada capaz de aplicar uma substituição a um tipo arbitrário,
  retornando um novo tipo.
-}
subst :: Unifier -> Type -> Type
subst x TypeInt = TypeInt
subst x (TypeVar a) =
    case lookup a x of
    Just b ->
            b
    Nothing ->
            TypeVar a
subst x (TypeArrow a b) = TypeArrow a (subst x b)

{-
  A função occursCheck verifica se uma variável aparece livre em um tipo.
-}
occursCheck :: Name -> Type -> Bool
occursCheck x TypeInt = False
occursCheck x (TypeVar y) = x == y
occursCheck x (TypeArrow a b) = occursCheck x a || occursCheck x b

{-
  A função compose compõe duas unificações distintas.
-}
compose :: Unifier -> Unifier -> Unifier
compose xs ys = 
    xs ++ subsUni xs ys

subsUni :: Unifier -> Unifier -> Unifier
subsUni xs ys =
    let substOnTuple (name, term) =
            (name, subst xs term)
    in mapear substOnTuple ys

mapear :: (a -> b) -> [a] -> [b]
mapear f [] = []
mapear f (x:xs) = f x:mapear f xs