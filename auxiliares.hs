

type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])

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
                            
-- Si un elemento forma parte de la lista lo borra
borrar :: Eq t => t -> [t] -> [t]
borrar _ [] = []
borrar x (y:ys) | x == y = ys
                | otherwise = y : borrar x ys

-- redSocialValida :: RedSocial -> Bool

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
noHayIdsRepetidos ((x,_):xs) = not (compara x xs) && noHayIdsRepetidos xs

-- Verifica si el x (ID) ingresado se repite en el resto de la lista
compara :: Integer -> [Usuario] -> Bool
compara _ [] = False
compara x ((y,_):ys) = x == y || compara x ys

-- relacionesValidas :: [Usuario] -> [Relacion] -> Bool

-- usuariosDeRelacionValidos :: [Usuario] -> [Relacion] -> Bool

-- relacionesAsimetricas :: [Relacion] -> Bool

-- noHayRelacionesRepetidas :: [Relacion] -> Bool

-- publicacionesValidas :: [Usuario] -> [Publicacion] -> Bool

-- usuariosDePublicacionSonUsuariosDeRed :: [Usuario] -> [Publicacion] -> Bool

-- noHayPublicacionesRepetidas :: [Publicacion] -> Bool

-- cadenaDeAmigos :: [Usuario] -> RedSocial -> Bool

-- relacionadosDirecto :: Usuario -> Usuario -> RedSocial -> Bool

-- sonDeLaRed :: RedSocial -> [Usuario] -> Bool


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
