module Dibujo where

data Fig bas = Basica bas | Rotar (Fig bas) | Espejar (Fig bas) | Rot45 (Fig bas)
     | Apilar Int Int (Fig bas) (Fig bas)
     | Juntar Int Int (Fig bas) (Fig bas)
     | Encimar (Fig bas) (Fig bas)
    deriving (Show, Eq)

-- definir el lenguaje
type Dibujo a = Fig a

-- composición n-veces de una función con sí misma.
comp :: (a -> a) -> Int -> a -> a
comp f 0 = id
comp f n = f . (comp f (n-1))

-- rotaciones de múltiplos de 90
r180 :: Dibujo a -> Dibujo a
r180 = Rotar . Rotar

r270 :: Dibujo a -> Dibujo a
r270 = Rotar . Rotar . Rotar

-- Pone una figura sobre la otra, ambas ocupan el mismo espacio
(.-.) :: Dibujo a -> Dibujo a -> Dibujo a
(.-.) x y = Apilar 1 1 x y

-- Pone una figura al lado de la otra, ambas ocupan el mismo espacio
(///) :: Dibujo a -> Dibujo a -> Dibujo a
(///) x y = Juntar 1 1 x y

-- Superpone una figura con otra
(^^^) :: Dibujo a -> Dibujo a -> Dibujo a
(^^^) x y = Encimar x y

-- Dada una figura la repite en cuatro cuadrantes
cuarteto :: Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a
cuarteto x y z w = (.-.) ((///) x y) ((///) z w)

-- una figura repetida con las cuatro rotaciones, superimpuestas.
encimar4 :: Dibujo a -> Dibujo a
--encimar4 x = (^^^) (Rot45 (Rot45 (Rot45 x))) ((^^^) (Rot45 (Rot45 x)) ((^^^) (Rot45 x) x))
encimar4 x = (^^^) (erre 3) ((^^^) (erre 2) ((^^^) (erre 1) (erre 0)))
            where erre k = (comp Rot45 k) x

-- cuadrado con la misma figura rotada $i$ por $90$ para $i \in \{1..3\}$.
-- No confundir con encimar4!
ciclar :: Dibujo a -> Dibujo a
ciclar x = cuarteto x (Rotar x) (r180 x) (r270 x) 

-- ver un a como una figura
pureDibe :: a -> Dibujo a
pureDibe a = Basica a

-- map para nuestro lenguaje
mapDib :: (a -> b) -> Dibujo a -> Dibujo b
mapDib f (Basica x) = Basica (f x)
mapDib f (Rotar x) = Rotar (mapDib f x)
mapDib f (Espejar x) = Espejar (mapDib f x)
mapDib f (Rot45 x) = Rot45 (mapDib f x)
mapDib f (Apilar i j x y) = Apilar i j (mapDib f x) (mapDib f y)
mapDib f (Juntar i j x y) = Juntar i j (mapDib f x) (mapDib f y)
mapDib f (Encimar x y) = Encimar (mapDib f x) (mapDib f y)

cambia :: (a -> Dibujo b) -> Dibujo a -> Dibujo b
cambia f (Basica x) = f x
cambia f (Rotar x) = Rotar (cambia f x)
cambia f (Espejar x) = Espejar (cambia f x)
cambia f (Rot45 x) = Rot45 (cambia f x)
cambia f (Apilar i j x y) = Apilar i j (cambia f x) (cambia f y)
cambia f (Juntar i j x y) = Juntar i j (cambia f x) (cambia f y)
cambia f (Encimar x y) = Encimar (cambia f x) (cambia f y)

-- Indica la semantica de nuestras figuras
-- Aplica funciones según el tipo de figura
-- que tengamos
sem :: (a -> b) -> (b -> b) -> (b -> b) -> (b -> b) ->
    (Int -> Int -> b -> b -> b) -> 
    (Int -> Int -> b -> b -> b) -> 
    (b -> b -> b) ->
    Dibujo a -> b

sem f1 f2 f3 f4 f5 f6 f7 (Basica x) = f1 x
sem f1 f2 f3 f4 f5 f6 f7 (Rotar x) = f2 (sem f1 f2 f3 f4 f5 f6 f7 x)
sem f1 f2 f3 f4 f5 f6 f7 (Espejar x) = f3 (sem f1 f2 f3 f4 f5 f6 f7 x)
sem f1 f2 f3 f4 f5 f6 f7 (Rot45 x) = f4 (sem f1 f2 f3 f4 f5 f6 f7 x)
sem f1 f2 f3 f4 f5 f6 f7 (Apilar i j x y) = f5 i j (sem f1 f2 f3 f4 f5 f6 f7 x) (sem f1 f2 f3 f4 f5 f6 f7 y)
sem f1 f2 f3 f4 f5 f6 f7 (Juntar i j x y) = f6 i j (sem f1 f2 f3 f4 f5 f6 f7 x) (sem f1 f2 f3 f4 f5 f6 f7 y)
sem f1 f2 f3 f4 f5 f6 f7 (Encimar x y) = f7 (sem f1 f2 f3 f4 f5 f6 f7 x) (sem f1 f2 f3 f4 f5 f6 f7 y)

type Pred a = a -> Bool

-- Dado un predicado sobre basicas, cambiar todas las que satisfacen
-- el predicado por una figura vacia
limpia :: Pred a -> a -> Dibujo a -> Dibujo a
limpia f vacia x = mapDib (\x -> if (f x) then vacia else x) x

-- Alguna basica satisface el predicado
anyDib :: Pred a -> Dibujo a -> Bool
anyDib f x = sem f id id id (\i j x y -> x || y) 
            (\i j x y -> x || y) (\x y -> x || y) x

-- Todas las basicas satisfacen el predicado
allDib :: Pred a -> Dibujo a -> Bool
allDib f x = sem f id id id (\i j x y -> x && y) 
            (\i j x y -> x && y) (\x y -> x && y) x

-- describe la figura. Ejemplos: 
--   desc (Basica b) (const "b") = "b"
--   desc (Rotar fa) db = "rot (" ++ desc fa db ++ ")"
-- la descripción de cada constructor son sus tres primeros
-- símbolos en minúscula.
desc :: (a -> String) -> Dibujo a -> String
desc f x = sem f (\x -> "rot (" ++ x ++ ")")
        (\x -> "esp (" ++ x ++ ")") (\x -> "r45 (" ++ x ++ ")")
        (\i j x y -> "api (" ++ (show i) ++ " " ++ (show j) ++ " " ++ x ++ " " ++ y ++ ")")
        (\i j x y -> "jun (" ++ (show i) ++ " " ++ (show j) ++ " " ++ x ++ " " ++ y ++ ")") 
        (\x y -> "enc (" ++ x ++ " " ++ y ++ ")") x

-- junta todas las figuras básicas de un dibujo
every :: Dibujo a -> [a]
every x = sem (\x -> [x]) id id id (\i j x y -> x ++ y) (\i j x y -> x ++ y) (\x y -> x ++ y) x

-- cuenta la cantidad de veces que aparecen las básicas en una 
-- figura.
contar :: Eq a => Dibujo a -> [(a,Int)]
contar x = sem (\x -> (\xs -> f x xs)) id id id ff ff (\f g -> (\xs -> f $ g xs)) x $ []
    where 
        f x [] = [(x,1)]
        f x ((y,i):ys) = if x == y then (x,i+1) : ys else (y,i) : f x ys
        ff = (\i j f g -> (\xs -> f $ g xs))

-- hay 4 rotaciones seguidas (empezando en el tope)
esRot360 :: Pred (Dibujo a)
esRot360 (Rotar (Rotar (Rotar (Rotar x)))) = True
esRot360 (Basica x) = False
esRot360 (Rotar x) = esRot360 x
esRot360 (Espejar x) = esRot360 x
esRot360 (Rot45 x) = esRot360 x
esRot360 (Juntar _ _ x y) = esRot360 x || esRot360 y
esRot360 (Apilar _ _ x y) = esRot360 x || esRot360 y
esRot360 (Encimar x y) = esRot360 x || esRot360 y

-- hay 2 espejados seguidos (empezando en el tope)
esFlip2 :: Pred (Dibujo a)
esFlip2 (Espejar (Espejar x)) = True
esFlip2 (Basica x) = False
esFlip2 (Rotar x) = esFlip2 x
esFlip2 (Espejar x) = esFlip2 x
esFlip2 (Rot45 x) = esFlip2 x
esFlip2 (Juntar _ _ x y) = esFlip2 x || esFlip2 y
esFlip2 (Apilar _ _ x y) = esFlip2 x || esFlip2 y
esFlip2 (Encimar x y) = esFlip2 x || esFlip2 y

-- la cadena que se toma como parámetro es la descripción
-- del error.
check :: Pred (Dibujo a) -> String -> Dibujo a -> Either String (Dibujo a)
check f s x = if f x then Left s else Right x

-- Aplica todos los chequeos y acumula los errores,
-- solo devuelve la figura si no hubo ningun error
todoBien :: Dibujo a -> Either [String] (Dibujo a)
todoBien x = case check esRot360 s1 x of
                Left s1 -> case check esFlip2 s2 x of
                    Left s2 -> Left (s1:[s2])
                    Right x -> Left [s1]
                Right x -> case check esFlip2 s2 x of
                    Left s2 -> Left [s2]
                    Right x -> Right x
                where s1 = "No hay rotacion de 360"
                      s2 = "No hay un flip"

-- Borra 4 repeticions seguidas de Rotar
noRot360 :: Dibujo a -> Dibujo a
noRot360 (Rotar (Rotar (Rotar (Rotar x)))) = noRot360 x
noRot360 (Basica x) = Basica x
noRot360 (Rotar x) = Rotar (noRot360 x)
noRot360 (Espejar x) = Espejar (noRot360 x)
noRot360 (Rot45 x) = Rot45 (noRot360 x)
noRot360 (Juntar i j x y) = Juntar i j (noRot360 x) (noRot360 y)
noRot360 (Apilar i j x y) = Apilar i j (noRot360 x) (noRot360 y)
noRot360 (Encimar x y) = Encimar (noRot360 x) (noRot360 y)

-- Borra 2 repeticiones seguidas de Espejar
noFlip2 :: Dibujo a -> Dibujo a
noFlip2 (Espejar (Espejar x)) = noFlip2 x
noFlip2 (Basica x) = Basica x
noFlip2 (Rotar x) = Rotar (noFlip2 x)
noFlip2 (Espejar x) = Espejar (noFlip2 x)
noFlip2 (Rot45 x) = Rot45 (noFlip2 x)
noFlip2 (Juntar i j x y) = Juntar i j (noFlip2 x) (noFlip2 y)
noFlip2 (Apilar i j x y) = Apilar i j (noFlip2 x) (noFlip2 y)
noFlip2 (Encimar x y) = Encimar (noFlip2 x) (noFlip2 y)


