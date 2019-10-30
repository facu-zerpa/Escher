module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Display
import Graphics.UI.GLUT.Begin
import Dibujo
import Interp
import qualified Basico.Ejemplo as E
import qualified Basico.Escher as Escher

data Conf a = Conf {
    basic :: Output a
  , fig  :: Dibujo a
  , width :: Float
  , height :: Float
  }

ej x y = Conf {
                basic = E.interpBas
              , fig = E.ejemplo
              , width = x
              , height = y
              }

escher x y = Conf {
                    basic = Escher.interpBC
                  , fig = Escher.ejemplo
                  , width = x
                  , height = y
                  }
-- Dada una computación que construye una configuración, mostramos por
-- pantalla la figura de la misma de acuerdo a la interpretación para
-- las figuras básicas. Permitimos una computación para poder leer
-- archivos, tomar argumentos, etc.
initial :: IO (Conf Escher.Escher) -> IO ()
initial cf = cf >>= \cfg ->
                  let x  = width cfg
                      y  = height cfg
                  in display win white . withGrid $ interp (basic cfg) (fig cfg) (0,0) (x,0) (0,y)
  where withGrid p = pictures [p, color grey $ grid 10 (0,0) 100 10]
        grey = makeColorI 120 120 120 120

win = InWindow "Paradigmas" (200, 200) (0, 0)
--main :: IO ()
--main = display win white $ return (escher 100 100)
main = initial $ return (escher 100 100)
