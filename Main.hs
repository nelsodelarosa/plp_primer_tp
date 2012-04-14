import Diccionario
import Maybe
import Random

import System.Random
import Control.Monad
import System.Environment
 



--Este módulo sirve para utilizar el diccionario sin acceder a su estructura interna. Pueden agregar otras funciones o casos de prueba.

{- Función a implementar. -}

busquedaDelTesoro::Eq a=>a->(a->Bool)->Diccionario a a->Maybe a
busquedaDelTesoro indicio check d = foldr (\x y->if isNothing x then Nothing else if check (fromJust x) then x else y) Nothing (iterate (\x->case x of {(Just c)->obtener c d; Nothing -> Nothing}) (Just indicio))


concatenarVeces:: Int -> Char-> String
concatenarVeces 0 x = []
concatenarVeces k x = (x:concatenarVeces (k-1) x)

genParString::Int-> (String,String)
genParString  k = (concatenarVeces k 'a' ,concatenarVeces (k+1) 'a')    

proxPar::(String,String) -> (String,String)
proxPar s = genParString ( length (snd s)) 
 
---funcion para generar pares de string para la busqueda del tesoro

generarPares::(String,String) -> [(String,String)]
--generarPares s = [s]++(generarPares (proxPar s))
generarPares s = iterate proxPar s

generarParesKPares:: Int->(String,String) -> [(String,String)]
generarParesKPares 0 s = []
generarParesKPares k s = ((proxPar s): generarParesKPares (k-1) (proxPar s))

--generarPares = foldr (\x y->(take x (repeat 'a'), take (x+1) (repeat 'a')):y) [] [1..]


diccV::Diccionario String String
diccV = definirVarias [("inicio","casa")] (vacio (<))



dicc1::Diccionario Int String
dicc1 = definirVarias [(0,"Hola"),(-10,"Chau"),(15,"Felicidades"),(2,"etc."),(9,"a")] (vacio (<))

dicc2::Diccionario String String
dicc2 = definirVarias [("inicio","casa"),("auto","flores"),("calle","auto"),("casa","escalera"),("ropero","alfajor"),("escalera","ropero")] (vacio (<))

dicc4::Diccionario String String
dicc4 = definirVarias [("inicio","casa"),("auto","flores"),("calle","auto"),("casa","escalera"),("ropero","alfajor"),("escalera","casa")] (vacio (<))

dicc5::Diccionario String String
--dicc5= definirVarias (take 1000 (generarPares ("a","aa")))(vacio (<))
dicc5= definirVarias (generarParesKPares 100 ("a","aa"))(vacio (<))

---Pruebas para busqueda del tesoro
testDic6:: Bool
testDic6  = busquedaDelTesoro "aa" ((\x-> (length x==4))) dicc5 == Just "aaaa" 

testDic7:: Bool
testDic7  = busquedaDelTesoro "aa" ((\x-> (length x==25))) dicc5 == Just "aaaaaaaaaaaaaaaaaaaaaaaaa" 

testDic8:: Bool
testDic8  = busquedaDelTesoro "bb" ((\x-> (length x==25))) dicc5 == Nothing 


---Pruebas para obtener
testDic9:: Bool
testDic9  = obtener "bb" dicc5 == Nothing 

testDic10:: Bool
testDic10  = obtener "aa" dicc5 == Just "aaa" 

testDic11:: Bool
testDic11  = obtener "aaaa" dicc5 == Just "aaaaa" 

testDic12:: Bool
testDic12  = obtener "inicio" diccV == Just "casa" 



dicc3::Diccionario Int String
dicc3 = definirVarias [(0,"Hola"),(-10,"Chau"),(15,"Felicidades"),(2,"etc."),(9,"a")] (vacio (\x y->x `mod` 5 < y `mod` 5))

--busquedaDelTesoro "aa" ((length x == 25)) dicc5

--definirTodo::[(String,String)] -> Diccionario String String ->Diccionario String String 
--definirTodo (x:xs) d =  definirTodo xs (definir (fst x ) (snd x ) d)


--definir::clave->valor->Diccionario clave valor->Diccionario clave valor
--definir c v d = definir1 c v (cmp d) (estructura d)

--Ejemplo: búsquedaDelTesoro "inicio" ((=='a').head) dicc2 = Just "alfajor"


