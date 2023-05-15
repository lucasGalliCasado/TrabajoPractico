-- Completar con los datos del grupo
--
-- Nombre de Grupo: -
-- Integrante 1: Chapana Puma, Joselin Miriam, yoselin.chapana@gmail.com, 1197/21
-- Integrante 2: Fernández Ana Celeste, anacelestefernandez@gmail.com, 41/19
-- Integrante 3: Peralta Diessler Bernardo, bernardodiessler@gmail.com, 1395/21
-- Integrante 4: Galli Casado Sastre Lucas Federico, lucasgalli01@gmail.com, 739/21



type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])
-- | Observacion : String = [Char]

-- Funciones basicas

usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us

-- Ejercicios

import auxiliares.hs

--- | Ejericio 1 |-----------------------------------------------------------------------------------------------------------------------------
-- describir qué hace la función: .....
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios = undefined


--- | Ejericio 2 |-----------------------------------------------------------------------------------------------------------------------------
-- PASA EL TEST CATEDRA

-- Aca use el que estaba en el codigo de jos, pero modifique las condiciones de pruebaRelacion pq por lo menos a mi me daba error


-- Recibe como parametros una RedSocial y un Usuario de la misma. Devuelve una lista contendiendo a todos los usuarios de la red con los cuales el Usuario ingresado tiene una relacion de amistad
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe (us,rs,ps) u = pruebaRelacion rs us u 

-- Recibe lista de Relaciones, Lista de Usuarios y un Usuario fijo U. Devuelve una lista de los usuarios que tienen relacion con U
pruebaRelacion :: [Relacion] -> [Usuario] -> Usuario -> [Usuario]
pruebaRelacion rs [] u = []
pruebaRelacion rs (us:uss) u | (pertenece (us, u) rs) || (pertenece (u, us) rs) = us : pruebaRelacion rs uss u
                             | otherwise = pruebaRelacion rs uss u

--- | Ejericio 3 |-----------------------------------------------------------------------------------------------------------------------------
-- PASA EL TEST CATEDRA

-- Comente esta pq en teoria hay un requiere que evita toda la condicion
-- cantidadDeAmigos :: RedSocial -> Usuario -> Int
-- cantidadDeAmigos red u  | redSocialValida red && usuarioValido u && pertenece u (usuarios red) = longitud (amigosDe red u)
--                         | otherwise = 0

-- Devuelve cantidad de amigos que tiene un usuario en una red social
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red u = longitud (amigosDe red u)



--- | Ejericio 4 |-----------------------------------------------------------------------------------------------------------------------------
-- describir qué hace la función: .....
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos = undefined


--- | Ejericio 5 |-----------------------------------------------------------------------------------------------------------------------------
-- describir qué hace la función: .....
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos = undefined


--- | Ejericio 6 |-----------------------------------------------------------------------------------------------------------------------------
-- describir qué hace la función: .....
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe = undefined



--- | Ejericio 7 |-----------------------------------------------------------------------------------------------------------------------------
-- PASA EL TEST CATEDRA

-- Indica las publicaciones que le gustan a un Usuario
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA red u = filtrarPublicaciones red u (publicaciones red)

-- Selecciona las publicaciones las cuales tienen el like del usuario
filtrarPublicaciones :: RedSocial -> Usuario -> [Publicacion] -> [Publicacion]
filtrarPublicaciones _ _ [] = []
filtrarPublicaciones red u (p:ps) | leGustaPublicacion u p = p : filtrarPublicaciones red u ps
                                  | otherwise = filtrarPublicaciones red u ps

-- Indica si un usuario le dio like a una publicacion
leGustaPublicacion :: Usuario -> Publicacion -> Bool
leGustaPublicacion u (_, _, likes) = pertenece u likes


--- | Ejericio 8 |-----------------------------------------------------------------------------------------------------------------------------

-- describir qué hace la función: .....
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones = undefined


--- | Ejericio 9 |-----------------------------------------------------------------------------------------------------------------------------

-- describir qué hace la función: .....
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel = undefined


--- | Ejericio 10 |-----------------------------------------------------------------------------------------------------------------------------

-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos = undefined
