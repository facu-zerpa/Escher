module Basico.Escher where
import Dibujo
import Interp
import Graphics.Gloss.Data.Picture

type Escher = FC

blanko = Basica (FC Blanko Blanco)
v = Basica (FC Trian1 Violeta)
b = Basica (FC Trian2 Rojo)
x = Basica (FC TrianD Magenta)
y = Basica (FC Rectan Azul)
z = Basica (FC Efe Cyan)

interpBC = h

--ejemplo = squarelimit 3 ((^^^) x ((.-.) v b))
k = (^^^) y $ (^^^) ((^^^) ((.-.) v x) ((.-.) x v)) ((///) (Rot45 z) (Espejar (Rot45 z)))
ejemplo = squarelimit 3 k

fish2 p = Espejar (Rot45 p)
fish3 p = r270 (fish2 p)

squarelimit n p = noneto (esquina n p) (lado n p) (r270 (esquina n p)) (Rotar (lado n p)) (dibujo_u p) (r270 (lado n p)) (Rotar (esquina n p)) (r180 (lado n p)) (r180 (esquina n p))

-- Dibujo u
dibujo_u :: Dibujo Escher -> Dibujo Escher
dibujo_u p = (^^^) ((^^^) (fish2 p) (Rotar (fish2 p))) 
            ((^^^) (r180 (fish2 p)) (r270 (fish2 p)))

-- Dibujo t
dibujo_t :: Dibujo Escher -> Dibujo Escher
dibujo_t p = (^^^) p ((^^^) (fish2 p) (fish3 p))

-- dibujo v
dibujo_v p = ciclar (Rotar (dibujo_t p))

-- esquina con nivel de detalle en base a la figura p
esquina :: Int -> Dibujo Escher -> Dibujo Escher
esquina 0 p = blanko
esquina n p = cuarteto (esquina (n-1) p) (lado (n-1) p) (Rotar (lado (n-1) p)) (dibujo_u p) 

-- lado con nivel de detalle en base a la figura p
lado :: Int -> Dibujo Escher -> Dibujo Escher
lado 0 p = blanko
lado n p = cuarteto (lado (n - 1) p) (lado (n - 1) p) (Rotar (dibujo_t p)) (dibujo_t p)

-- combinador de nueve figuras dentro de un espacio de dibujo
noneto p q r s t u v w x = Apilar 1 2 (Juntar 1 2 p (Juntar 1 1 q r)) (Apilar 1 1 (Juntar 1 2 s (Juntar 1 1 t u)) (Juntar 1 2 v (Juntar 1 1 w x)))


 
