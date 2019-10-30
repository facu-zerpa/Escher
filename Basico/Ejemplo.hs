module Basico.Ejemplo where
import Dibujo
import Interp

type Basica = Integer
ejemplo :: Dibujo Basica
ejemplo = Basica 1

interpBas :: Output Basica
interpBas 1 = trian1
