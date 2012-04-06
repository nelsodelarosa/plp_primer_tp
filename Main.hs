import Diccionario
import Maybe

--Este módulo sirve para utilizar el diccionario sin acceder a su estructura interna. Pueden agregar otras funciones o casos de prueba.

{- Función a implementar. -}

busquedaDelTesoro::Eq a=>a->(a->Bool)->Diccionario a a->Maybe a
busquedaDelTesoro indicio check d = foldr (\x y->if isNothing x then Nothing else if check (fromJust x) then x else y) Nothing (iterate (\x->case x of {(Just c)->obtener c d; Nothing -> Nothing}) (Just indicio))

{- Diccionarios de prueba: -}

dicc1::Diccionario Int String
dicc1 = definirVarias [(0,"Hola"),(-10,"Chau"),(15,"Felicidades"),(2,"etc."),(9,"a")] (vacio (<))

dicc2::Diccionario String String
dicc2 = definirVarias [("inicio","casa"),("auto","flores"),("calle","auto"),("casa","escalera"),("ropero","alfajor"),("escalera","ropero")] (vacio (<))

dicc3::Diccionario Int String
dicc3 = definirVarias [(0,"Hola"),(-10,"Chau"),(15,"Felicidades"),(2,"etc."),(9,"a")] (vacio (\x y->x `mod` 5 < y `mod` 5))

--Ejemplo: búsquedaDelTesoro "inicio" ((=='a').head) dicc2 = Just "alfajor"
