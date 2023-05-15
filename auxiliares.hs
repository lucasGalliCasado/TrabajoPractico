type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])

---------------------------------------------------------------------------------------------------------------------------------------------------------------

--Auxiliares

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
mismosElementos (x:xs) y    | longitud (x:xs) == longitud y && pertenece x y = mismosElementos xs (borrar x y)
                            | otherwise = False

-- Devuelve la cantidad de elementos en una lista
longitud :: Eq t => [t] -> Int
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

-- Si un elemento forma parte de la lista lo borra
borrar :: Eq t => t -> [t] -> [t]
borrar _ [] = []
borrar x (y:ys) | x == y = ys
                | otherwise = y : borrar x ys

-- redSocialValida :: RedSocial -> Bool
redSocialValida :: RedSocial -> Bool
redSocialValida (u,r,p) = usuariosValidos u && relacionesValidas u r && publicacionesValidas u p 

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

-- relacionesValidas :: [Usuario] -> [Relacion] -> Bool
-- Dada una lista de Usuarios y otra de Relaciones, devuelve True si son validas
relacionesValidas :: [Usuario] -> [Relacion] -> Bool
relacionesValidas us rel = usuariosDeRelacionValidos us rel && relacionesAsimetricas rel && noHayRelacionesRepetidas rel

-- usuariosDeRelacionValidos :: [Usuario] -> [Relacion] -> Bool  CHECKEARCHECKEARCHECKEARCHECKEARCHECKEARCHECKEARCHECKEARCHECKEAR
-- CHECKEARCHECKEARCHECKEARCHECKEARCHECKEARCHECKEARCHECKEARCHECKEARCHECKEARCHECKEARCHECKEARCHECKEARCHECKEARCHECKEARCHECKEARCHECKEARCHECKEAR
--Recibe una lista de Relaciones y otra de Usuarios. Da True si todas las Relaciones esten definidas entre Usuarios de la lista 
usuariosDeRelacionValidos :: [Usuario] -> [Relacion] -> Bool
usuariosDeRelacionValidos _ [] = True
usuariosDeRelacionValidos u (r:rs) = (pertenece (fst r) u) && (pertenece (snd r) u) && (fst r /= snd r) && (usuariosDeRelacionValidos u rs)

-- relacionesAsimetricas :: [Relacion] -> Bool
-- Recibe una lista de Relaciones y devuelve True si para toda relacion, su simetria no esta en la lista
relacionesAsimetricas :: [Relacion] -> Bool
relacionesAsimetricas [] = True
relacionesAsimetricas (r:rs) = not (pertenece (invertirRelacion r) rs) && relacionesAsimetricas rs
 
invertirRelacion :: Relacion -> Relacion
invertirRelacion (u1, u2) = (u2, u1)

-- noHayRelacionesRepetidas :: [Relacion] -> Bool
-- Recibe una lista de Relaciones y retorna True si no hay relaciones repetidas, notese que cuenta una permutacion como una relacion distinta                             
noHayRelacionesRepetidas :: [Relacion] -> Bool
noHayRelacionesRepetidas [] = True
noHayRelacionesRepetidas (r:rs) | (pertenece r rs == True) = False
                                | otherwise = (True && noHayRelacionesRepetidas rs)


-- publicacionesValidas :: [Usuario] -> [Publicacion] -> Bool
publicacionesValidas :: [Usuario] -> [Publicacion] -> Bool
publicacionesValidas u p = (usuariosDePublicacionSonUsuariosDeRed u p) && (noHayPublicacionesRepetidas p)

-- usuariosDePublicacionSonUsuariosDeRed :: [Usuario] -> [Publicacion] -> Bool
usuariosDePublicacionSonUsuariosDeRed :: [Usuario] -> [Publicacion] -> Bool
usuariosDePublicacionSonUsuariosDeRed u [] = True
usuariosDePublicacionSonUsuariosDeRed u (p:ps) = (pertenece (usuarioDePublicacion p) u) && usuariosDePublicacionSonUsuariosDeRed u ps

-- noHayPublicacionesRepetidas :: [Publicacion] -> Bool
-- Recibe un lista de Publicaciones y devuelve True si no hay ninguna repetida
noHayPublicacionesRepetidas :: [Publicacion] -> Bool
noHayPublicacionesRepetidas (p:[]) = True
noHayPublicacionesRepetidas ((us,tx,l):ps) = not (pertenece (idDeUsuario us,tx) (iDyTexto ps)) && noHayPublicacionesRepetidas ps
                                           
-- Auxiliar de noHayPublicacionesRepetidas
-- Recibe una lista de Publicaciones y devuelve una lista de los ID's y textos de las mismas respetando el orden
iDyTexto :: [Publicacion] -> [(Integer,String)]
iDyTexto [] = []
iDyTexto ((us,tx,l):p) = (idDeUsuario us,tx) : iDyTexto p

-- cadenaDeAmigos :: [Usuario] -> RedSocial -> Bool
cadenaDeAmigos :: [Usuario] -> RedSocial -> Bool
cadenaDeAmigos [] _ = True
cadenaDeAmigos [_] _ = True
cadenaDeAmigos (u1:u2:us) red = relacionadosDirecto u1 u2 red && cadenaDeAmigos (u2:us) red

-- 
relacionadosDirecto :: Usuario -> Usuario -> RedSocial -> Bool
relacionadosDirecto u1 u2 (us,rel,pub) = (pertenece (u1,u2) rel) || (pertenece (u2,u1) rel)

-- Verifica que la lista de Usuarios pertenezca a los usuarios de la red social
sonDeLaRed :: RedSocial -> [Usuario] -> Bool
sonDeLaRed _ [] = True
sonDeLaRed x (y:ys) = pertenece y (usuarios x) && sonDeLaRed x ys

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
