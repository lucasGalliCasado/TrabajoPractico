module Test where


import Test.HUnit
import Solucion



main = runTestTT tests
run1 = runTestTT testEjercicio1
run2 = runTestTT testEjercicio2
run3 = runTestTT testEjercicio3
run4 = runTestTT testEjercicio4
run5 = runTestTT testEjercicio5
run6 = runTestTT testEjercicio6
run7 = runTestTT testEjercicio7
run8 = runTestTT testEjercicio8
run9 = runTestTT testEjercicio9
run10 = runTestTT testEjercicio10

tests = test [
    " nombresDeUsuarios 1" ~: (nombresDeUsuarios redA) ~?= ["Juan","Natalia","Pedro","Mariela"],

    " amigosDe 1" ~: (amigosDe redA usuario1) ~?= [usuario2, usuario4],

    " cantidadDeAmigos 1" ~: (cantidadDeAmigos redA usuario1) ~?= 2,

    " usuarioConMasAmigos 1" ~: expectAny (usuarioConMasAmigos redA) [usuario2, usuario4],

    " estaRobertoCarlos 1" ~: (estaRobertoCarlos redA) ~?= False,

    " publicacionesDe 1" ~: (publicacionesDe redA usuario2) ~?= [publicacion2_1, publicacion2_2],

    " publicacionesQueLeGustanA 1" ~: (publicacionesQueLeGustanA redA usuario1) ~?= [publicacion2_2, publicacion4_1],

    " lesGustanLasMismasPublicaciones 2" ~: (lesGustanLasMismasPublicaciones redB usuario1 usuario3) ~?= True,

    " tieneUnSeguidorFiel 1" ~: (tieneUnSeguidorFiel redA usuario1) ~?= True,

    " existeSecuenciaDeAmigos 1" ~: (existeSecuenciaDeAmigos redA usuario1 usuario3) ~?= True

   ]


testEjercicio1= test [
    " nombresDeUsuarios 1" ~: (nombresDeUsuarios redA) ~?= ["Juan","Natalia","Pedro","Mariela"],
    " nombresDeUsuarios 2" ~: (nombresDeUsuarios redAn) ~?= ["Ana","Mia","Camilo","Gaara"]
 ]

testEjercicio2= test [
    -- Casos donde el usuario tiene al menos un amigo
    " amigosDe Varios-1" ~: (amigosDe redA usuario1) ~?= [usuario2, usuario4],    
    " amigosDe Varios-2" ~: (amigosDe redFriend usuario14) ~?= [usuario10, usuario13, usuario15, usuario16],
    --Caso donde el usuario no tienen ningun amigo
    " amigosDe Empty" ~: (amigosDe redFriend usuario17) ~?= []
 ]

testEjercicio3 = test [
    -- Caso en el que usuario1 no tiene amigos en la red social
    " cantidadDeAmigos Empty" ~: (cantidadDeAmigos redF usuario1) ~?= 0,
    -- Caso en el que usuario1 tiene un UNICO amigo
    " cantidadDeAmigos Unico" ~: (cantidadDeAmigos redD usuario1) ~?= 1,
    -- Caso en el que usuario1 tiene mas de un amigo
    " cantidadDeAmigos Varios" ~: (cantidadDeAmigos redE usuario1) ~?= 2 
 ]


testEjercicio4 = test [
    -- En el test de la catedra hay varios usuarios que comparten la propiedad de maxima cantidad de amigos
    " usuarioConMasAmigos 1" ~: expectAny (usuarioConMasAmigos redA) [usuario2, usuario4],
   -- Existe un uncio usuario con la maxima cantidad de amigos
    "usuarioConMasAmigos-Unico" ~: expectAny (usuarioConMasAmigos redC) [usuario1]

 ]

testEjercicio5 = test [
    -- Testeo de la Catedra
    " estaRobertoCarlos-False" ~: (estaRobertoCarlos redA) ~?= False,
   -- Usamos la función auxiliar estaRobertoCarlosTest para probar el caso True
    " estaRobertoCarlosTest-True" ~: (estaRobertoCarlosTest redR) ~?= True

 ]

testEjercicio6 = test [
    -- Testeo de la Catedra
    " publicacionesDe 1" ~: (publicacionesDe redA usuario2) ~?= [publicacion2_1, publicacion2_2],
    --Caso donde el usuario hizo algunas publicaciones
    " publicacionesDe-Varias" ~: (publicacionesDe redFriend1 usuario14) ~?= [publicacion14_1, publicacion14_2, publicacion14_3],
    --Caso donde el usuario no hace ninguna publicación
    " publicacionesDe-Empty" ~: (publicacionesDe redFriend1 usuario17) ~?= []

 ]


testEjercicio7 = test [
   --Caso donde al usuario no le gusta NINGUNA publicacion
    " publicacionesQueLeGustanA 2" ~: (publicacionesQueLeGustanA red2 usuario1) ~?= [], 
    --Caso donde al usuario le gusta UNA publicacion    
    " publicacionesQueLeGustanA 3" ~: (publicacionesQueLeGustanA red2 usuario19) ~?= [publicacion7_1], 
    -- Caso donde al usuario le gustan VARIAS publicaciones    
    " publicacionesQueLeGustanA 4" ~: (publicacionesQueLeGustanA red3 usuario18) ~?= [publicacion7_2, publicacion7_3]
 ]
   

testEjercicio8 = test [
    -- Caso en el que ninguno de los usuarios le han dado like a nada
    " lesGustanLasMismasPublicaciones True-Empty" ~: (lesGustanLasMismasPublicaciones redC usuario1 usuario2) ~?= True, 
    -- Caso en el que ambos han likeado las mismas publicaciones, habiendo likeado al menos una cada uno 
    " lesGustanLasMismasPublicaciones True-NotEmpty" ~: (lesGustanLasMismasPublicaciones redC usuario7 usuario8) ~?= True,
    -- Caso en el no han likeado las mismas publicaciones, y uno de los usuarios no le ha dado like a nada
    " lesGustanLasMismasPublicaciones False-Empty" ~: (lesGustanLasMismasPublicaciones redC usuario7 usuario1) ~?= False,
    -- Caso en el que ambos usuarios han likeado al menos una publicacion, pero no las mismas.
    " lesGustanLasMismasPublicaciones True-NotEmpty" ~: (lesGustanLasMismasPublicaciones redA usuario2 usuario4) ~?= False
 ]
 
testEjercicio9 = test [
    -- Caso en donde el usuario solo tiene un seguidor fiel (Testeo de la Catedra)
    " tieneUnSeguidorFiel 1" ~: (tieneUnSeguidorFiel redA usuario1) ~?= True,
    -- Caso en donde el usuario no tiene ningun seguidor fiel
    " tieneUnSeguidorFielFalse" ~: (tieneUnSeguidorFiel redC usuario1) ~?= False,
    -- Caso en donde el usuario tiene multiples seguidores fieles
    " tieneMultiplesSeguidoresFieles" ~: (tieneUnSeguidorFiel redC usuario6) ~?= True
 ]
 
testEjercicio10 = test [
    -- Testeo de la Catedra
    " existeSecuenciaDeAmigos-Long" ~: (existeSecuenciaDeAmigos redA usuario1 usuario3) ~?= True,
    -- Caso en donde NO existe un secuencia de amigos
    "existeSecuenciaDeAmigos-False" ~: (existeSecuenciaDeAmigos redC usuario1 usuario7) ~?= False,
    -- El caso de la catedra prueba una cadena 'larga' (osea los usuarios en cuestion no son amigos), este caso prueba el caso 'corto', i.e. los usuario son amigos
    "existeSecuenciaDeAmigos-Short" ~: (existeSecuenciaDeAmigos redC usuario1 usuario6) ~?= True

 ]


expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)


-- Usuarios 'Default'
usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")

-- Usuarios Agredados
usuario6 = (6, "Lucas")
usuario7 = (7, "Connie")
usuario8 = (8, "Susy")
usuario9 = (9, "Alfonso")

usuario10 = (10, "Ana")
usuario11 = (11, "Mia")
usuario12 = (12, "Camilo")
usuario13 = (13, "Gaara")

usuario14 = (14, "Jos")
usuario15 = (15, "Tavo")
usuario16 = (16, "Iván")
usuario17 = (17, "Yami")

usuario18 =(18, "Juan")
usuario19 = (19, "María")
usuario20 = (20, "Pedro")

-- Relaciones "Default"
relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) -- Notar que el orden en el que aparecen los usuarios es indistinto
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)

-- Relaciones agregadas
relacion1_5 = (usuario1, usuario5)
relacion1_6 = (usuario1, usuario6)
relacion1_7 = (usuario1, usuario7)
relacion1_8 = (usuario1, usuario8)
relacion1_9 = (usuario1, usuario9) 
relacion1_10 = (usuario1, usuario10)
relacion1_11 = (usuario1, usuario11)
relacion1_12 = (usuario1, usuario12)
relacion14_13 = (usuario14, usuario13)
relacion14_15 = (usuario14, usuario15)
relacion14_16 = (usuario14, usuario16)
relacion10_14 = (usuario14, usuario10)
relacion18_19 = (usuario18, usuario19)
relacion18_20 = (usuario18, usuario20) 


-- Publicaciones "Default"
publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2, usuario4])
publicacion1_2 = (usuario1, "Este es mi segundo post", [usuario4])
publicacion1_3 = (usuario1, "Este es mi tercer post", [usuario2, usuario5])
publicacion1_4 = (usuario1, "Este es mi cuarto post", [])
publicacion1_5 = (usuario1, "Este es como mi quinto post", [usuario5])

publicacion2_1 = (usuario2, "Hello World", [usuario4])
publicacion2_2 = (usuario2, "Good Bye World", [usuario1, usuario4])

publicacion3_1 = (usuario3, "Lorem Ipsum", [])
publicacion3_2 = (usuario3, "dolor sit amet", [usuario2])
publicacion3_3 = (usuario3, "consectetur adipiscing elit", [usuario2, usuario5])

publicacion4_1 = (usuario4, "I am Alice. Not", [usuario1, usuario2])
publicacion4_2 = (usuario4, "I am Bob", [])
publicacion4_3 = (usuario4, "Just kidding, i am Mariela", [usuario1, usuario3])

-- Publicaciones Agregadas
publicacion6_1 = (usuario6, "Mr. Postman look and see",[usuario7,usuario8])
publicacion6_2 = (usuario6, "Is there a letter in your bag for me?",[usuario7,usuario8])
publicacion6_3 = (usuario6, "I've been waiting, I lost my mind", [usuario7,usuario8])
publicacion6_4 = (usuario6, "Me olvide el resto de la letra",[])

publicacion14_1 = (usuario14, "Justo que pensaba en vos nena, caí muerto",[usuario10, usuario13, usuario15])
publicacion14_2 = (usuario14, "Yo te amo tanto que no puedo despertarme sin amar",[usuario13, usuario16])
publicacion14_3 = (usuario14, "Te amo ya y ya es mañana", [usuario13, usuario15])

publicacion7_1 = (usuario18, "Hello", [usuario19])
publicacion7_2 = (usuario19, "Goodbye", [usuario18, usuario19])
publicacion7_3 = (usuario20, "World", [usuario18])

-- Redes "Default"
usuariosA = [usuario1, usuario2, usuario3, usuario4]
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]
publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2]
redA = (usuariosA, relacionesA, publicacionesA)

usuariosB = [usuario1, usuario2, usuario3, usuario5]
relacionesB = [relacion1_2, relacion2_3]
publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redB = (usuariosB, relacionesB, publicacionesB)
--

-- Redes Agregadas

-- Info Red C: El usuario con mas amigos es unico (usuario1): Usamos este dato para probar usuarioConMasAmigos, ademas
--             prueba si funciona lesGustanLasmismasPubliaciones en dos casos, uno con dos usuarios que no le han dado like a ninguna publicacion(usuarios 1 y 2),
--             y otro en donde los usuarios le han dado like a al menos una publicacion (usuarios 7  y 8). 
usuariosC = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7]
relacionesC = [relacion1_2,relacion1_3,relacion1_4,relacion1_5,relacion1_6]
publicacionesC = [publicacion6_1,publicacion6_2,publicacion6_3,publicacion1_4]
redC = (usuariosC, relacionesC, publicacionesC)

-- Creamos una red con 10 usuarios para poder probar estaRobertoCarlosTest
usuariosR = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario10, usuario11, usuario12]
relacionesR = [relacion1_2,relacion1_3,relacion1_4,relacion1_5,relacion1_6,relacion1_7,relacion1_8,relacion1_9,relacion1_10,relacion1_11,relacion1_12]
redR = (usuariosR, relacionesR, [])
---Creamos una red para poder ver si devuelve los nombres de los usuarios
usuariosAn = [usuario10, usuario11, usuario12, usuario13]
redAn = (usuariosAn,[] ,[])


usuariosD = [usuario1, usuario2]
relacionesD = [(usuario1, usuario2)]
redD = (usuariosD, relacionesD, [])

usuariosE = [usuario1, usuario2, usuario3]
relacionesE = [(usuario1, usuario2), (usuario1, usuario3)]
redE = (usuariosE, relacionesE,[])

redF = ([usuario1], [], [])

usuarios2 = [usuario18, usuario19]
red2 = (usuarios2, [relacion18_19], [publicacion7_1])
usuarios3 = [usuario18, usuario19, usuario20]
relaciones3 = [relacion18_19, relacion18_20]
publicaciones3 = [publicacion7_1, publicacion7_2, publicacion7_3]
red3 = (usuarios3, relaciones3, publicaciones3)

-- Creamos una red que contenga un usuario que se relacione con la mayoría de mis usuarios creados y, a su vez, que contenga
-- un usuario que no se relacione con nadie para ver cómo funciona 
usuariosFriend = [usuario10, usuario13, usuario14, usuario15, usuario16, usuario17]
relacionesFriend = [relacion10_14, relacion14_13, relacion14_15, relacion14_16]
redFriend = (usuariosFriend, relacionesFriend, [])
redFriend1 = (usuariosFriend, relacionesFriend, [publicacion14_1,publicacion14_2,publicacion14_3])





