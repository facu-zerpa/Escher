module Interp where
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Data.Color
import qualified Graphics.Gloss.Data.Point.Arithmetic as V

import Dibujo
type FloatingPic = Vector -> Vector -> Vector -> Picture
type Output a = a -> FloatingPic

-- el vector nulo
zero :: Vector
zero = (0,0)

half :: Vector -> Vector
half = (0.5 V.*)

-- comprender esta función es un buen ejericio.
hlines :: Vector -> Float -> Float -> [Picture]
hlines v@(x,y) mag sep = map (hline . (*sep)) [0..]
  where hline h = line [(x,y+h),(x+mag,y+h)] 

-- Una grilla de n líneas, comenzando en v con una separación de sep y
-- una longitud de l (usamos composición para no aplicar este
-- argumento)
grid :: Int -> Vector -> Float -> Float -> Picture
grid n v sep l = pictures [ls,translate 0 (l*toEnum n) (rotate 90 ls)]
  where ls = pictures $ take (n+1) $ hlines v sep l

-- figuras adaptables comunes
trian1 :: FloatingPic
trian1 a b c = line $ map (a V.+) [zero, half b V.+ c , b , zero]

trian2 :: FloatingPic
trian2 a b c = line $ map (a V.+) [zero, c, b,zero]

trianD :: FloatingPic
trianD a b c = line $ map (a V.+) [c, half b , b V.+ c , c]

rectan :: FloatingPic
rectan a b c = line [a, a V.+ b, a V.+ b V.+ c, a V.+ c,a]

simple :: Picture -> FloatingPic
simple p _ _ _ = p

fShape :: FloatingPic
fShape a b c = line . map (a V.+) $ [ zero,uX, p13, p33, p33 V.+ uY , p13 V.+ uY 
                 , uX V.+ 4 V.* uY ,uX V.+ 5 V.* uY, x4 V.+ y5
                 , x4 V.+ 6 V.* uY, 6 V.* uY, zero]    
  where p33 = 3 V.* (uX V.+ uY)
        p13 = uX V.+ 3 V.* uY
        x4 = 4 V.* uX
        y5 = 5 V.* uY
        uX = (1/6) V.* b
        uY = (1/6) V.* c

-- Dada una función que produce una figura a partir de un a y un vector
-- producimos una figura flotante aplicando las transformaciones
-- necesarias. Útil si queremos usar figuras que vienen de archivos bmp.
transf :: (a -> Vector -> Picture) -> a -> Vector -> FloatingPic
transf f d (xs,ys) a b c  = translate (fst a') (snd a') .
                             scale (magV b/xs) (magV c/ys) .
                             rotate ang $ f d (xs,ys)
  where ang = radToDeg $ argV b
        a' = a V.+ half (b V.+ c)

interp :: Output a -> Output (Dibujo a)
interp f (Basica x) = f x 
interp f (Rotar x) = rotar (interp f x)
interp f (Espejar x) = espejar (interp f x)
interp f (Rot45 x) = rot45 (interp f x)
interp f (Apilar n m x y) = apilar (toEnum n) (toEnum m) (interp f x) (interp f y)
interp f (Juntar n m x y) = juntar (toEnum n) (toEnum m) (interp f x) (interp f y)
interp f (Encimar x y) = encimar (interp f x) (interp f y)

rotar :: FloatingPic -> Vector -> Vector -> Vector -> Picture
rotar p a b c = p (a V.+ b) c (zero V.- b)

rot45 :: FloatingPic -> Vector -> Vector -> Vector -> Picture
rot45 p a b c = p (a V.+ (half (b V.+ c))) (half (b V.+ c)) (half (c V.- b))

espejar :: FloatingPic -> Vector -> Vector -> Vector -> Picture
espejar p a b c = p (a V.+ b) (zero V.- b) c

encimar :: FloatingPic -> FloatingPic -> Vector -> Vector -> Vector -> Picture
encimar p q a b c =  pictures [p a b c, q a b c]

juntar :: Float -> Float -> FloatingPic -> FloatingPic -> Vector -> Vector -> Vector -> Picture
juntar m n p q a b c = pictures [p a b' c, q (a V.+ b') (r' V.* b) c]
                    where r' = n / (m + n)
                          r = m / (m + n)
                          b' = r V.* b

apilar :: Float -> Float -> FloatingPic -> FloatingPic -> Vector -> Vector -> Vector -> Picture
apilar m n p q a b c = pictures [p (a V.+ c') b (r V.* c), q a b c']
                        where r' = n / (m + n)
                              r = m / (m + n)
                              c' = r' V.* c

-- formas y colores
data Formas = Trian1 | Trian2 | TrianD | Rectan | Efe | Blanko
data Colores = Rojo | Azul | Verde | Amarillo | Cyan | Magenta | Blanco | Negro | Rosa | Violeta | Celeste | Aqua | Chartre | Naranja

formas Trian1 = trian1
formas Trian2 = trian2
formas TrianD = trianD
formas Rectan = rectan
formas Efe = fShape
formas Blanko = simple blank

colores Rojo = red
colores Azul = blue
colores Verde = green
colores Amarillo = yellow
colores Cyan = cyan
colores Magenta = magenta
colores Blanco = white
colores Negro = black
colores Rosa = rose
colores Violeta = violet
colores Celeste = azure
colores Aqua = aquamarine
colores Chartre = chartreuse
colores Naranja = orange


data FC = FC {
    forma :: Formas
  , colour :: Colores
}

g :: FloatingPic -> Color -> Vector -> Vector -> Vector -> Picture
g t c x y z = color c $ t x y z

h :: FC -> FloatingPic
h fc = g (formas $ forma fc) (colores $ colour fc)
------------------------------------------------------
