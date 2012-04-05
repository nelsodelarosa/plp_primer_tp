module Árbol23 where

data Árbol23 a b = Hoja a | Dos b (Árbol23 a b) (Árbol23 a b) | Tres b b (Árbol23 a b) (Árbol23 a b) (Árbol23 a b)

{- Funciones para mostrar el árbol. -}

instance (Show a, Show b) => Show (Árbol23 a b) where
    show x = concatMap (++"\n") (padTree 0 x)

padTree:: (Show a, Show b) => Int -> (Árbol23 a b)-> [String]
padTree i (Hoja x) = [(pad i) ++  (show x)]
padTree i (Dos x a1 a2) = [(pad i) ++  (show x) ] ++ (concatMap (padTree (i + 4)) [a1,a2])
padTree i (Tres x y a1 a2 a3) = [(pad i) ++  (show x) ++ (' ':(show y))] ++ (concatMap (padTree (i + 4)) [a1,a2,a3])

pad:: Int -> String
pad i = replicate i ' '

{- Funciones pedidas. -}

--foldA23::

--Lista en preorden de los internos del árbol.
--internos::Árbol23 a b->[b]

--Lista las hojas de izquierda a derecha.
--hojas::Árbol23 a b->[a]

--esHoja::Árbol23 a b->Bool

--mapA23::(a->c)->(b->d)->Árbol23 a b->Árbol23 c d

--Ejemplo de uso de mapA23.
--Incrementa en 1 el valor de las hojas.
incrementarHojas::Num a =>Árbol23 a b->Árbol23 a b
incrementarHojas = mapA23 (+1) id


--Trunca el árbol hasta un determinado nivel. Cuando llega a 0, reemplaza el resto del árbol por una hoja con el valor indicado.
--Funciona para árboles infinitos.
--truncar::a->Integer->Árbol23 a b->Árbol23 a b

--Evalúa las funciones tomando los valores de los hijos como argumentos.
--En el caso de que haya 3 hijos, asocia a izquierda.
--evaluar::Árbol23 a (a->a->a)->a

{- Árboles de ejemplo. -}
arbolito1::Árbol23 Char Int
arbolito1 = Tres 0 1
	      (Dos 2 (Hoja 'a') (Hoja 'b'))
	      (Tres 3 4 (Hoja 'c') (Hoja 'd') (Dos 5 (Hoja 'e') (Hoja 'f')))
	      (Dos 6 (Hoja 'g') (Dos 7 (Hoja 'h') (Hoja 'i')))

arbolito2::Árbol23 Int Bool
arbolito2 = Dos True (Hoja (-1)) (Tres False True (Hoja 0) (Hoja (-2)) (Hoja 4))

arbolito3::Árbol23 Int (Int->Int->Int)
arbolito3 = Dos (+) (Tres (*) (-) (Hoja 1) (Hoja 2) (Hoja 3)) (incrementarHojas arbolito3)

--Ejemplos:
--internos arbolito1 = [0,1,2,3,4,5,6,7]
--hojas arbolito1 = "abcdefghi"
--hojas (incrementarHojas arbolito2) = [0,1,-1,5]
--internos arbolito2 = [True,False,True]
--take 10 (hojas arbolito3) = [1,2,3,2,3,4,3,4,5,4]
--hojas (truncar 0 6 arbolito3) = [1,2,3,2,3,4,3,4,5,4,5,6,0,0,0,0,0]
--evaluar (truncar 0 6 arbolito3) = 22 = (1*2-3)+(2*3-4)+(3*4-5)+(4*5-6)
