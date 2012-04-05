import Diccionario
import Maybe

--Este módulo sirve para utilizar el diccionario sin acceder a su estructura interna. Pueden agregar otras funciones o casos de prueba.

{- Función a implementar. -}

--búsquedaDelTesoro::Eq a=>a->(a->Bool)->Diccionario a a->Maybe a 

{- Diccionarios de prueba: -}

dicc1::Diccionario Int String
dicc1 = definirVarias [(0,"Hola"),(-10,"Chau"),(15,"Felicidades"),(2,"etc."),(9,"a")] (vacío (<))

dicc2::Diccionario String String
dicc2 = definirVarias [("inicio","casa"),("auto","flores"),("calle","auto"),("casa","escalera"),("ropero","alfajor"),("escalera","ropero")] (vacío (<))

dicc3::Diccionario Int String
dicc3 = definirVarias [(0,"Hola"),(-10,"Chau"),(15,"Felicidades"),(2,"etc."),(9,"a")] (vacío (\x y->x `mod` 5 < y `mod` 5))

--Ejemplo: búsquedaDelTesoro "inicio" ((=='a').head) dicc2 = Just "alfajor"