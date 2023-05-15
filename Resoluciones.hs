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
-- Cumple test-catedra
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios r = proyectarNombres (usuarios r)

proyectarNombres :: [Usuario] -> [String]
proyectarNombres [] = []
proyectarNombres (us:uss) = (pi2 us): proyectarNombres uss

--- | Ejericio 2 |-----------------------------------------------------------------------------------------------------------------------------
-- Cumple test-catedra
-- Recibe como parametros una RedSocial y un Usuario de la misma. Devuelve una lista contendiendo a todos los usuarios de la red con
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
-- describir qué hace la función: .....
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos = undefined


--- | Ejericio 6 |-----------------------------------------------------------------------------------------------------------------------------
-- describir qué hace la función: .....
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe = undefined



--- | Ejericio 7 |-----------------------------------------------------------------------------------------------------------------------------
-- Cumple test-catedra 
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

-- describir qué hace la función: .....
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel = undefined


--- | Ejericio 10 |-----------------------------------------------------------------------------------------------------------------------------

-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos = undefined

