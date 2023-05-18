-- Completar con los datos del grupo
--
-- Nombre de Grupo: -
-- Integrante 1: Chapana Puma, Joselin Miriam, yoselin.chapana@gmail.com, 1197/21
-- Integrante 2: Fernández Ana Celeste, anacelestefernandez@gmail.com, 41/19
-- Integrante 3: Peralta Diessler Bernardo, bernardodiessler@gmail.com, 1395/21
-- Integrante 4: Galli Casado Sastre Lucas Federico, lucasgalli01@gmail.com, 739/21

module Resoluciones where

import Auxiliares 

--- | Ejericio 1 |-----------------------------------------------------------------------------------------------------------------------------
-- Cumple test-catedra y testeo propio
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios r = proyectarNombres (usuarios r)

proyectarNombres :: [Usuario] -> [String]
proyectarNombres [] = []
proyectarNombres (us:uss) = (pi2 us): proyectarNombres uss

--- | Ejericio 2 |-----------------------------------------------------------------------------------------------------------------------------
-- Cumple test-catedra
-- Recibe como parametros una RedSocial y un Usuario de la misma. Devuelve una lista conteniendo a todos los usuarios de la red con
-- los cuales el Usuario ingresado tiene una relacion de amistad
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe (us,rs,ps) u = pruebaRelacion rs us u 

-- Recibe lista de Relaciones, Lista de Usuarios y un Usuario fijo U. Devuelve una lista de los usuarios que tienen relacion con U
pruebaRelacion :: [Relacion] -> [Usuario] -> Usuario -> [Usuario]
pruebaRelacion rs [] u = []
pruebaRelacion rs (us:uss) u | ((pertenece (us,u) rs) == True) || ((pertenece (u,us) rs) == True) =  us:(pruebaRelacion rs uss u)
                             | otherwise = pruebaRelacion rs uss u


--- | Ejericio 3 |-----------------------------------------------------------------------------------------------------------------------------
-- Cumple test-catedra
-- Recibe un usuario y devuelve la cantidad de amigos que tiene
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos r u = length(amigosDe r u)


--- | Ejericio 4 |-----------------------------------------------------------------------------------------------------------------------------
-- Cumple test-catedra y testeo propio
-- Devuelve el usuario con mayor cantidad de amigos
-- Estoy al tanto de que la recursion no cumple con el requiere. Lo consulte y me dijeron que no pasa nada 
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos ((u:[]),r,p) = u
usuarioConMasAmigos ((u:us),r,p) | cantidadDeAmigos ((u:us),r,p) u > cantidadDeAmigos ((u:us),r,p) (head us) = usuarioConMasAmigos ((u:tail us),r,p)
                                 | otherwise = usuarioConMasAmigos (us,r,p)


--- | Ejericio 5 |-----------------------------------------------------------------------------------------------------------------------------
-- Recibe una red social y devuelve true si un usuario de la red social tiene más de un millón de amigos
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos ([],r,p) = False
estaRobertoCarlos ((u:us),r,p) | cantidadDeAmigos ((u:us),r,p) u <= 1000000 = estaRobertoCarlos (us,r,p)
                               | cantidadDeAmigos ((u:us),r,p) u > 1000000 = True

-- Creamos una función auxiliar de estaRobertoCarlos para poder probar si está bien planteada 
estaRobertoCarlosTest :: RedSocial -> Bool
estaRobertoCarlosTest ([],r,p) = False
estaRobertoCarlosTest ((u:us),r,p) | cantidadDeAmigos ((u:us),r,p) u <= 10 = estaRobertoCarlosTest (us,r,p)
                                   | cantidadDeAmigos ((u:us),r,p) u > 10 = True

--- | Ejericio 6 |-----------------------------------------------------------------------------------------------------------------------------

-- Recibe una red social y un usuario. Devuelve una lista de todos las publiaciones del usuario en cuestion.
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe rs u = todasLasPublicacionesDe (publicaciones rs) u 

todasLasPublicacionesDe :: [Publicacion] -> Usuario -> [Publicacion]
todasLasPublicacionesDe [] u = []  
todasLasPublicacionesDe (p:ps) u | (usuarioDePublicacion p == u) = p : todasLasPublicacionesDe ps u 
                                 | otherwise = todasLasPublicacionesDe ps u 



--- | Ejericio 7 |-----------------------------------------------------------------------------------------------------------------------------
-- Cumple test-catedra y testeo propio
-- Recibe una Red y un Usuario, devuelve una lista de las publicaciones a las que le ha dado like el usuario en cuestion
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA (us,r,[]) u = []
publicacionesQueLeGustanA (us,r,(p:ps)) u | pertenece u (likesDePublicacion p) = p: publicacionesQueLeGustanA (us,r,ps) u 
                                          | otherwise = publicacionesQueLeGustanA (us,r,ps) u


--- | Ejericio 8 |-----------------------------------------------------------------------------------------------------------------------------
-- Cumple test-catedra y testeo propio
-- Dados dos usuarios nos da True si le han dado like a las mismas publicaciones
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones r u1 u2 = mismosElementos (publicacionesQueLeGustanA r u1) (publicacionesQueLeGustanA r u2)


--- | Ejericio 9 |-----------------------------------------------------------------------------------------------------------------------------
-- Cumple test Catedra y testeo propio
-- Recibe una red social y un Usuario U, devuelve True si existe al menos un usuario que le haya dado like a todas las publicaciones de U 
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel r u | length(interseccionLikes (publicacionesDe r u)) > 0 = True
                        | otherwise = False

interseccionLikes :: [Publicacion] -> [Usuario]
interseccionLikes (p:[]) = likesDePublicacion p
interseccionLikes (p:ps) = interseccion (interseccion (likesDePublicacion p) (likesDePublicacion (head ps))) (interseccionLikes ps)


--- | Ejericio 10 |-----------------------------------------------------------------------------------------------------------------------------
-- Cumple test Catedra y testeo propio
-- Recibe una red social y dos usuarios de la misma. Devuelve True si existe una cadena de amigos entre los 2
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos r u1 u2 = recursion10 r (amigosDe r u1) u2  (length (usuarios r))

recursion10 :: RedSocial -> [Usuario] -> Usuario -> Int -> Bool
recursion10 r u u2 0 = False
recursion10 r u u2 n | pertenece u2 (amigosDeLista r u) == True = True
                     | otherwise = (recursion10 r (amigosDeLista r u) u2 (n-1))

amigosDeLista :: RedSocial -> [Usuario] -> [Usuario]
amigosDeLista r [] = []
amigosDeLista r (u:us) = (amigosDe r u) ++ amigosDeLista r us


