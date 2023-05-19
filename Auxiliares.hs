module Auxiliares where



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


---- Funciones Auxiliares

interseccion :: (Eq t) => [t] -> [t] -> [t]
interseccion _ [] = []
interseccion [] _ = []
interseccion (a:as) b | (pertenece a b == True) = a:interseccion as b  
                      | otherwise = interseccion as b



invertirLista :: (Eq t) => [t] -> [t]
invertirLista (t:[]) = [t]
invertirLista (t:ts) = (invertirLista ts) ++ [t] 


pi1 (x,y) = x

pi2 (x,y) = y

-- Verifica si un "x" pertenece a una lista
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece n (x:xs)  | n == x = True
                    | n /= x = pertenece n xs

-- Verifica si dos listas contienen los mismos elementos
mismosElementos :: Eq t => [t] -> [t] -> Bool
mismosElementos [] [] = True
mismosElementos [] _ = False
mismosElementos _ [] = False
mismosElementos (x:xs) y    | pertenece x y = mismosElementos xs (borrar x y)
                            | otherwise = False
                            
-- Si un elemento forma parte de la lista lo borra UNA SOLA VEZ
borrar :: (Eq t) => t -> [t] -> [t]
borrar _ [] = []
borrar x (y:ys) | x == y = ys
                | otherwise = y : borrar x ys

-- Si un elemento forma parte de la lista lo saca TODAS LAS VECES QUE APARECE
sacarRepetidos :: (Eq t) => t -> [t] -> [t]
sacarRepetidos _ [] = []
sacarRepetidos x (y:ys) | x == y = sacarRepetidos x ys
                        | otherwise = y : sacarRepetidos x ys


-- Verifica si dada la lista de usuarios todos tienen un nombre de usuario no vacio y distintos ID -- ¿NO HAY PROBLEMA CON MISMOS USUARIOS?
usuariosValidos :: [Usuario] -> Bool
usuariosValidos [] = True
usuariosValidos (x:xs) = usuarioValido x && usuariosValidos xs && noHayIdsRepetidos (x:xs)

-- Verifica que ID > 0 y que el nombre de usuario no esta vacio
usuarioValido :: Usuario -> Bool
usuarioValido (x,y) | x > 0 && y /= [] = True
                    | otherwise = False

-- Si en la lista de usuarios, hay dos o mas con el mismo ID devuelve False
noHayIdsRepetidos :: [Usuario] -> Bool
noHayIdsRepetidos [] = True
noHayIdsRepetidos [_] = True
noHayIdsRepetidos ((x,_):xs) = not (comparaID x xs) && noHayIdsRepetidos xs

-- Verifica si el x (ID) ingresado se repite en el resto de la lista
comparaID :: Integer -> [Usuario] -> Bool
comparaID _ [] = False
comparaID x ((y,_):ys) = x == y || comparaID x ys


--Recibe una lista de Relaciones y otra de Usuarios. Da True si todas las Relaciones esten definidas entre Usuarios de la lista 
usuariosDeRelacionValidos :: [Usuario] -> [Relacion] -> Bool
usuariosDeRelacionValidos u [] = True
usuariosDeRelacionValidos u (r:rs) = (pertenece (pi1 r) u) && (pertenece (pi2 r) u) && (usuariosDeRelacionValidos u rs)


-- Recibe una lista de Relaciones y retorna True si no hay relaciones repetidas, notese que cuenta una permutacion como una relacion distinta                             
noHayRelacionesRepetidas :: [Relacion] -> Bool
noHayRelacionesRepetidas [] = True
noHayRelacionesRepetidas (r:rs) | (pertenece r rs == True) = False
                                | otherwise = (True && noHayRelacionesRepetidas rs)


publicacionesValidas :: [Usuario] -> [Publicacion] -> Bool
publicacionesValidas u p = (usuariosDePublicacionSonUsuariosDeRed u p) && (noHayPublicacionesRepetidas p)

usuariosDePublicacionSonUsuariosDeRed :: [Usuario] -> [Publicacion] -> Bool
usuariosDePublicacionSonUsuariosDeRed u [] = True
usuariosDePublicacionSonUsuariosDeRed u (p:ps) = (pertenece (usuarioDePublicacion p) u) && usuariosDePublicacionSonUsuariosDeRed u ps

-- Recibe un lista de Publicaciones y devuelve True si no hay ninguna repetida
noHayPublicacionesRepetidas :: [Publicacion] -> Bool
noHayPublicacionesRepetidas (p:[]) = True
noHayPublicacionesRepetidas ((us,tx,l):ps) = not (pertenece (idDeUsuario us,tx) (iDyTexto ps)) && noHayPublicacionesRepetidas ps
                                           
-- Auxiliar de noHayPublicacionesRepetidas
-- Recibe una lista de Publicaciones y devuelve una lista de los ID's y textos de las mismas respetando el orden
iDyTexto :: [Publicacion] -> [(Integer,String)]
iDyTexto [] = []
iDyTexto ((us,tx,l):p) = (idDeUsuario us,tx) : iDyTexto p


cadenaDeAmigos :: [Usuario] -> RedSocial -> Bool
cadenaDeAmigos [us] r = True
cadenaDeAmigos (u:us) r = relacionadosDirecto u (head us) r && cadenaDeAmigos us r

relacionadosDirecto :: Usuario -> Usuario -> RedSocial -> Bool
relacionadosDirecto u1 u2 (us,rel,pub) = (pertenece (u1,u2) rel) || (pertenece (u2,u1) rel)


sonDeLaRed :: RedSocial -> [Usuario] -> Bool
sonDeLaRed (u,x,y) [uss] = pertenece uss u
sonDeLaRed (u,x,y) (us:uss) = pertenece us u && sonDeLaRed (u,x,y) uss


-- Devuelve True en el caso de que el primero elemento de la lista sea x
empiezaCon :: Eq t => t -> [t] -> Bool
empiezaCon x (y:ys) | x == y = True
                    | otherwise = False

-- Devuelve True en el caso de que el ultimo elemento de la lista sea x
terminaCon :: Eq t => t -> [t] -> Bool
terminaCon _ [] = False
terminaCon x [y] = x == y
terminaCon x (y:ys) = terminaCon x ys


-- Devuelve True si ningun elemento se repite en la lista
sinRepetidos :: Eq t => [t] -> Bool
sinRepetidos x = not (conRepetidos x)
            where 
             conRepetidos [] = False
             conRepetidos (x:xs) = pertenece x xs || conRepetidos xs
