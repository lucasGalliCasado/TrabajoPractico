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
-- describir qué hace la función: .....
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe = undefined

--- | Ejericio 3 |-----------------------------------------------------------------------------------------------------------------------------
-- describir qué hace la función: .....
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red u  | redSocialValida red && usuarioValido u && pertenece u (usuarios red) = longitud (amigosDe red u)
                        | otherwise = 0



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
publicacionesQueLeGustanA red u | not (redSocialValida red) || not (usuarioValido u) || not (pertenece u (usuarios red)) = []
                                | otherwise = publicacionesQueLeGustanARec red u (publicaciones red) []

publicacionesQueLeGustanARec :: RedSocial -> Usuario -> [Publicacion] -> [Publicacion] -> [Publicacion]
publicacionesQueLeGustanARec _ _ [] res = sinRepetidos2 res
publicacionesQueLeGustanARec red u (p:ps) res   | pertenece u (likesDePublicacion p) && pertenece p (publicaciones red) = publicacionesQueLeGustanARec red u ps (p:res)
                                                | otherwise = publicacionesQueLeGustanARec red u ps res

sinRepetidos2 :: Eq a => [a] -> [a]
sinRepetidos2 [] = []
sinRepetidos2 (x:xs)
  | pertenece x xs = sinRepetidos2 xs
  | otherwise   = x : sinRepetidos2 xs


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
