module Arbol23 where
 
import Char

data Arbol23 a b = Hoja a | Dos b (Arbol23 a b) (Arbol23 a b) | Tres b b (Arbol23 a b) (Arbol23 a b) (Arbol23 a b) 

{- Funciones para mostrar el Arbol. -}

instance (Show a, Show b) => Show (Arbol23 a b) where
    show x = concatMap (++"\n") (padTree 0 x)

padTree:: (Show a, Show b) => Int -> (Arbol23 a b)-> [String]
padTree i (Hoja x) = [(pad i) ++  (show x)]
padTree i (Dos x a1 a2) = [(pad i) ++  (show x) ] ++ (concatMap (padTree (i + 4)) [a1,a2])
padTree i (Tres x y a1 a2 a3) = [(pad i) ++  (show x) ++ (' ':(show y))] ++ (concatMap (padTree (i + 4)) [a1,a2,a3])

pad:: Int -> String
pad i = replicate i ' '

-- Funciones pedidas. --

--función de plegado de arboles23 un parámetro de tipo función por cada
--constructor del tipo abstracto de datos, además de un arbol 
-- los pasos recursivos del plegado se aplican a los "subarboles" arboles de constructor(en caso de haberlos).
--Notese que solo en este caso usamos recursion explicita(porque se nos permite y 
--por que no podria hacerse de otra manera).
foldA23::(a->c)->(b->c->c->c)->(b->b->c->c->c->c)->Arbol23 a b->c
foldA23 h d t a = case a of
	Hoja x -> h x
	Dos x y z -> d x (rec y) (rec z)
	Tres v w x y z -> t v w  (rec x) (rec y) (rec z)
	where rec = foldA23 h d t


--Lista en preorden de los internos del Arbol.
internos::Arbol23 a b->[b]
internos a23 = foldA23 (\_->[]) (\x y z->x:y++z) (\v w x y z->v:w:x++y++z) a23


--Lista las hojas de izquierda a derecha.
--Solo nos interesa el caso de constructor "Hoja a" entonces, allí es donde prestaremos atenciín, el resto
--simplemente concatenamos los casos recursivos bajo la hipotesis que la funcion de plegado se encarga 
--apropiadamente de aplicar las funciones.

hojas::Arbol23 a b->[a]
hojas a23 = foldA23 (\x->[x]) (\x y z->y++z) (\v w x y z->x++y++z) a23

--Esta funcion nos dice si un árbol es hoja, en caso de construirse solo con "Hoja a" lo es, caso contrario no.
esHoja::Arbol23 a b->Bool
esHoja a23 = case a23 of {Hoja x->True; otherwise->False}

---Como solo las hojas son de tipo "a", aplicaremos la función del primer parametro del map 
--a las mismas; Para el resto usaremos la función de segundo parametro del map. 
mapA23::(a->c)->(b->d)->Arbol23 a b->Arbol23 c d
mapA23 f g = foldA23 (\x -> Hoja (f x)) (\x y z -> Dos (g x) y z) (\v w x y z->Tres (g v) (g w) x y z)

--Ejemplo de uso de mapA23.
--Incrementa en 1 el valor de las hojas.
incrementarHojas::Num a =>Arbol23 a b->Arbol23 a b
incrementarHojas = mapA23 (+1) id

--Trunca el Arbol hasta un determinado nivel. Cuando llega a 0, reemplaza el resto del Arbol por una hoja con el valor indicado.
--Funciona para Arboles infinitos.
--En el esquema de la función de plegado el tipo "b" seria Arbol23->Int ( entonces le podemos pasar el nivel).

truncar::a->Integer->Arbol23 a b->Arbol23 a b
truncar h j a23  = foldA23 (\x k -> case k of {(0)->  (Hoja h) ; otherwise -> (Hoja x) }) 
					( \x m l k -> case k of { (0)-> (Hoja h); (1)->(Dos x (Hoja h) (Hoja h));
						otherwise->Dos x (m (k-1)) (l (k-1)) })
					( \v w n m l k -> case k of { (0)-> (Hoja h); 
						(1)-> (Tres v w (Hoja h) (Hoja h) (Hoja h));
						otherwise->Tres v w (n (k-1)) (m (k-1)) (l (k-1)) }) 
					a23 j
--Evalúa las funciones tomando los valores de los hijos como argumentos.
--En el caso de que haya 3 hijos, asocia a izquierda.

-- En el caso que es hoja el tipo (a->a->a) no tiene sentido entonces devolvemos el mismo valor usando id.
-- para el resto de los constructores aplicamos las funciones que en realidad son los los nodos internos.
--Debemos tener cuidada de asociar de izq a derecha en el caso que el arbo lsea de "tipo 3"
evaluar::Arbol23 a (a->a->a)->a
evaluar a23 = foldA23 id (\f a b ->  f a b) (\f g a b c -> g (f a b) c) a23 


-- Arboles de ejemplo. --
arbolito1::Arbol23 Char Int
arbolito1 = Tres 0 1
	      (Dos 2 (Hoja 'a') (Hoja 'b'))
	       (Tres 3 4 (Hoja 'c') (Hoja 'd') (Dos 5 (Hoja 'e') (Hoja 'f')))
	       (Dos 6 (Hoja 'g') (Dos 7 (Hoja 'h') (Hoja 'i')))
	       
arbolito2::Arbol23 Int Bool
arbolito2 = Dos True (Hoja (-1)) (Tres False True (Hoja 0) (Hoja (-2)) (Hoja 4)) 

arbolito3::Arbol23 Int (Int->Int->Int)
arbolito3 = Dos (+) (Tres (*) (-) (Hoja 1) (Hoja 2) (Hoja 3)) (incrementarHojas arbolito3)     

arbolito4::Arbol23 Int Char 
arbolito4 = Dos 'p'( Dos 'l' ((Dos 'g' (Hoja 5) (Hoja 2))) (Tres 'r' 'a' (Hoja 0) (Hoja 1) (Hoja 12))) ( Dos 'p' (Tres 'n' 'd' (Hoja (-3)) (Hoja 4) (Hoja 9)) ( (Dos 'e' (Hoja 20) (Hoja 7)) ))     

---prueba para nodos internos y hojas del arbol
testab1::Bool
testab1 = internos arbolito1 == [0,1,2,3,4,5,6,7]

testab2::Bool
testab2 = hojas arbolito1 ==  "abcdefghi"

testab3::Bool
testab3 = hojas (incrementarHojas arbolito2) == [0,1,-1,5]

testab4::Bool
testab4 = internos arbolito2 == [True,False,True]

---prueba para hojas de arbol infinito
testab5::Bool
testab5 = take 10 (hojas arbolito3) == [1,2,3,2,3,4,3,4,5,4]

--pruebas de truncado arbles infinitos
testab6::Bool
testab6 = hojas (truncar 0 6 arbolito3) == [1,2,3,2,3,4,3,4,5,4,5,6,0,0,0,0,0]

---prueba de evaluado
testab7::Bool
testab7 = evaluar (truncar 0 6 arbolito3) == 22 

testab8::Arbol23 Int Char
testab8 = mapA23 (+4) (toUpper) arbolito4

testab9::[Char]
testab9 =  internos arbolito4

testab10::Arbol23 Int Char
testab10 = truncar 4 2 arbolito4


arbolito5::Arbol23 Int Int
arbolito5 = Dos (2) (Tres (3) (4) (Hoja 0) (Hoja 2) (Hoja 22)) (incrementarHojas arbolito5)




-- Arboles de ejemplo. --
-- arbolito1::Arbol23 Char Int
-- arbolito1 = Tres 0 1
	      --(Dos 2 (Hoja 'a') (Hoja 'b'))
	      -- (Tres 3 4 (Hoja 'c') (Hoja 'd') (Dos 5 (Hoja 'e') (Hoja 'f')))
	       --(Dos 6 (Hoja 'g') (Dos 7 (Hoja 'h') (Hoja 'i')))
-- 
-- arbolito2::Arbol23 Int Bool
-- arbolito2 = Dos True (Hoja (-1)) (Tres False True (Hoja 0) (Hoja (-2)) (Hoja 4))
-- 
-- arbolito3::Arbol23 Int (Int->Int->Int)
-- arbolito3 = Dos (+) (Tres (*) (-) (Hoja 1) (Hoja 2) (Hoja 3)) (incrementarHojas arbolito3)

--Ejemplos:
--internos arbolito1 = [0,1,2,3,4,5,6,7]
--hojas arbolito1 = "abcdefghi"
--hojas (incrementarHojas arbolito2) = [0,1,-1,5]
--internos arbolito2 = [True,False,True]
--take 10 (hojas arbolito3) = [1,2,3,2,3,4,3,4,5,4]
--hojas (truncar 0 6 arbolito3) = [1,2,3,2,3,4,3,4,5,4,5,6,0,0,0,0,0]
--evaluar (truncar 0 6 arbolito3) = 22 = (1*2-3)+(2*3-4)+(3*4-5)+(4*5-6)
