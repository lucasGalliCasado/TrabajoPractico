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


--- | Ejericio 1 |-----------------------------------------------------------------------------------------------------------------------------
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios = proyectarNombres (usuarios RedSocial)

proyectarNombres :: [Usuario] -> [String]
proyectarNombres [] = []
proyectarNombres (us:uss) = us[1]: proyectarNombres uss

--- | Ejericio 2 |-----------------------------------------------------------------------------------------------------------------------------

{-
Aca lo hice sin funciones auxiliares, pero su compilacion es mas dudosa. Despues cuando testeamos habria que ver si anda.

amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe ([],rs,ps) u = []
amigosDe ((us:uss),rs,ps) u | (pertenece [us,u] rs || pertenece [u,us] rs) = us: amigosDe (uss,rs,ps) u
                          | otherwise = amigosDe (uss,rs,ps) u

-}


-- Recibe como parametros una RedSocial y un Usuario de la misma. Devuelve una lista contendiendo a todos los usuarios de la red con
-- los cuales el Usuario ingresado tiene una relacion de amistad
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe (us,rs,ps) u = pruebaRelacion rs us u 

-- Recibe lista de Relaciones, Lista de Usuarios y un Usuario fijo U. Devuelve una lista de los usuarios que tienen relacion con U
pruebaRelacion :: [Relacion] -> [Usuario] -> Usuario -> [Usuario]
pruebaRelacion rs [] u = []
pruebaRelacion rs (us:uss) u | ((pertenece [us, u] rs) || (pertenece [us, u] rs) == True) =  us:pruebaRelacion rs uss u



--- | Ejericio 3 |-----------------------------------------------------------------------------------------------------------------------------
-- Recibe una red social y un usuario. Devuelve la cantidad de amigos que tiene el usuario
cantidadDeAmigos r u = length(amigosDe r s)




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

-- describir qué hace la función: .....
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA = undefined


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
