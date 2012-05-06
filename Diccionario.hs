module Diccionario (Diccionario, vacio, definir, definirVarias, obtener, claves) where

import Maybe
import List
import Arbol23

{- Definiciones de tipos. -}

type Comp clave = clave->clave->Bool 
type Estr clave valor = Arbol23 (clave,valor) clave

data Diccionario clave valor = Dicc {cmp :: Comp clave, estructura :: Maybe (Estr clave valor)}

--El comparador es por menor.

{- Funciones provistas por la cAtedra. -}

--Inserta un nuevo par clave, valor en una estructura que ya tiene al menos un dato.
insertar::clave->valor->Comp clave->Estr clave valor-> Estr clave valor
insertar c v comp a23 = interceptar (insertarYPropagar c v comp a23) id (\s1 (c1, s2)->Dos c1 s1 s2)

--Maneja el caso de que la segunda componente sea Nothing.
interceptar::(a,Maybe b)->(a->c)->(a->b->c)->c
interceptar (x,y) f1 f2 = case y of
			       Nothing -> f1 x
			       Just z -> f2 x z

{- Inserta una clave con su valor correspondiente. Si se actualiza el índice, el cambio se propaga hacia arriba
   para mantener balanceado el Arbol.
   Usamos recursión explícita porque este tipo de recursión no es estructural (no se aplica a todos los hijos). -}
insertarYPropagar::clave->valor->Comp clave->Estr clave valor-> (Estr clave valor, Maybe (clave, Estr clave valor))
insertarYPropagar c v comp a23 = let rec = insertarYPropagar c v comp in case a23 of
	--Si es hoja, elegimos la mAxima de las claves y propagamos el balanceo hacia arriba.
	Hoja (ch,vh) -> if comp c ch 
				then (Hoja (c,v), Just (ch, Hoja (ch,vh)))
				else (Hoja (ch, vh), Just (c, Hoja (c,v)))
	{- Si el actual es Nodo-Dos, o se queda en forma Nodo-Dos o se transforma en 
	   Nodo-Tres; no puede ocurrir que haya propagación hacia arriba (retornamos Nothing). -}
	Dos c1 a1 a2 -> (if comp c c1
				then 
				-- La clave c va del lado izquierdo.
					interceptar (rec a1) 
						(\s1 -> Dos c1 s1 a2)
						(\s1 (c3, s2) -> Tres c3 c1 s1 s2 a2)
				else 
				-- La clave c va del lado derecho.
					interceptar (rec a2) 
						(\s1 -> Dos c1 a1 s1)
						(\s1 (c3, s2) -> Tres c1 c3 a1 s1 s2), Nothing)
	{- Nodo-tres sólo propaga si de abajo propagan, los tres casos son muy similares
	   Sólo cambia en que Arbol se inserta. -}
	Tres c1 c2 a1 a2 a3 -> if comp c c1
				then 
					-- La clave debe ir en el primer Arbol.
					interceptar (rec a1) 
						(\s1 -> (Tres c1 c2 s1 a2 a3, Nothing))
						(\s1 (c3, s2) -> (Dos c3 s1 s2, Just(c1, Dos c2 a2 a3)))
				else if comp c c2
				then 
					-- La clave debe ir en el Arbol del medio.
					interceptar (rec a2) 
						(\s1 -> (Tres c1 c2 a1 s1 a3, Nothing))
						(\s1 (c3, s2) -> (Dos c1 a1 s1, Just(c3, Dos c2 s2 a3)))
				else 
					--La clave debe ir en el último Arbol.
					interceptar (rec a3) 
						(\s1 -> (Tres c1 c2 a1 a2 s1, Nothing))
						(\s1 (c3, s2) -> (Dos c1 a1 a2, Just(c2, Dos c3 s1 s2)))

--Se asume que la lista no tiene claves repetidas.
definirVarias::[(clave,valor)]->Diccionario clave valor->Diccionario clave valor
definirVarias = (flip.foldr.uncurry) definir

{- Funciones a implementar. -}


---Función que define diccioanrio vacio
vacio::Comp clave->Diccionario clave valor
vacio f  = Dicc f Nothing


--Define una clave y un valor o signficado, utiliza la función definir1
--baja un nivel de abstraccion en la estructura interna del diccionario trabajando
--sobre Maybe Estr
definir::clave->valor->Diccionario clave valor->Diccionario clave valor
definir c v (Dicc cmp estr) = case estr of
	Nothing -> Dicc cmp (Just (Hoja (c,v)))
	Just a23 -> Dicc cmp (Just (insertar c v cmp a23))

--cuando definimos en un diccionario vacio El arbol debe tener una hoja sola, 
--caso contrario debemos insertar la clave usando la funcion de la catedra que hace las rotaciones.

--definir1::clave->valor->Comp clave->Maybe (Estr clave valor)->Diccionario clave valor
--definir1 c v f Nothing = Dicc f (Just (Hoja (c,v)))
--definir1 c v f (Just a23) = Dicc f (Just (insertar c v f a23))

--De manera análoga a definir esta funcion usa obtener1 que trabaja sobre 
--un nivel de abstraccion mas bajo y luego la llama con los proyectores adecuados, del 
--diccionario pasado como parámetro.
obtener::Eq clave=>clave->Diccionario clave valor->Maybe valor
obtener c d = obtener2 c (cmp d) (estructura d)

--Como la estructura interna del diccionario es un arbol23 usamos la funcion de plegado de 
--arboles23 que hicimos antes y vamos comparando clave y significado.
--En caso de ser hoja comparamos si la clave coincide, si es asi devolvemos el valor sino Nothing
-- En los casos recursivos usamos la función de comparación y continuamos buscando subarboles derechos, izq o medio
--asumiendo que el arbold23 tiene un invariante de representación invArbol23 => avl=> abb, 
--esto nos permite hacer la bósqueda en orden logarótmico.
obtener2::Eq clave=>clave->Comp clave->Maybe (Estr clave valor)->Maybe valor
obtener2 c f Nothing = Nothing
obtener2 c f (Just a23) = foldA23 (\p->if fst p == c then Just (snd p) else Nothing)
							(\x y z->if f c x then y else z)
							(\v w x y z->if f c v then x else (if f c w then y else z)) a23


claves:: Diccionario clave valor->[clave]
claves d = (claves1 (estructura d))

--Utilizamos la tecnica de antes y bajamos un nivel de abdtracción
--para trabajar sobre el árbol. En caso de una hoja tomo la primer componente, caso contrario concateno
--los casos recursivos
claves1::Maybe (Estr clave valor)->[clave]
claves1 Nothing = []
claves1 (Just a23) = foldA23 (\x->[fst x]) (\x y z->[x]++y++z) (\v w x y z->[v]++[w]++x++y++z) a23
--claves1 (Just a23) = foldA23 (\x->[fst x]) (\x y z->y++z) (\v w x y z->x++y++z) a23

{- Diccionarios de prueba: -}
--Las pruebas del diccioanrio estan en el archivo Main.hs


dicc1::Diccionario Int String
dicc1 = definirVarias [(0,"Hola"),(-10,"Chau"),(15,"Felicidades"),(2,"etc."),(9,"a")] (vacio (<))

dicc2::Diccionario String String 
dicc2 = definirVarias [("inicio","casa"),("auto","flores"),("calle","auto"),("casa","escalera"),("ropero","alfajor"),("escalera","ropero")] (vacio (<))

dicc3::Diccionario Int String
dicc3 = definirVarias [(0,"Hola"),(-10,"Chau"),(15,"Felicidades"),(2,"etc."),(9,"a")] (vacio (\x y->x `mod` 5 < y `mod` 5))

