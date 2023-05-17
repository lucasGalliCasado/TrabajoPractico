module Test where


import Test.HUnit
import Resoluciones



main = runTestTT tests
run1 = runTestTT testEjercicio1
--run2 = runTestTT testEjercicio2
run3 = runTestTT testEjercicio3
run4 = runTestTT testEjercicio4
--run5 = runTestTT testEjercicio5
--run6 = runTestTT testEjercicio6
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
    " nombresDeUsuarios 2" ~: (nombresDeUsuarios redC) ~?= ["Juan","Natalia","Pedro","Mariela","Natalia","Lucas","Connie"]
 ]

--testEjercicio2= test []

testEjercicio3 = test [
    " cantidadDeAmigos 2" ~: (cantidadDeAmigos redF usuario1) ~?= 0, -- Caso en el que usuario1 no tiene amigos en la red social

    " cantidadDeAmigos 3" ~: (cantidadDeAmigos redD usuario1) ~?= 1, -- Caso en el que usuario1 tiene un UNICO amigo

    " cantidadDeAmigos 4" ~: (cantidadDeAmigos redE usuario1) ~?= 2, -- Caso en el que usuario1 tiene mas de un amigo

    " cantidadDeAmigos 5" ~: (cantidadDeAmigos redD usuario3) ~?= 0  -- Caso en el que usuario3 no esta en la red social
 ]


testEjercicio4 = test [
    "usuarioConMasAmigos 2" ~: expectAny (usuarioConMasAmigos redA) [usuario1, usuario4],
    "usuarioConMasAmigos 3" ~: expectAny (usuarioConMasAmigos redC) [usuario1]

 ]

--testEjercicio5 = test []

--testEjercicio6 = test []

testEjercicio7 = test [
    -- Caso donde al usuario no le gusta NINGUNA publicacion
    " publicacionesQueLeGustanA 2" ~: (publicacionesQueLeGustanA red2 usuario1) ~?= [], 
    -- Caso donde al usuario le gusta UNA publicacion
    " publicacionesQueLeGustanA 3" ~: (publicacionesQueLeGustanA red3 usuario2) ~?= [((1, "Juan"), "Hello", [(2, "María")])], 
    -- Caso donde al usuario le gustan VARIAS publicaciones
    " publicacionesQueLeGustanA 4" ~: (publicacionesQueLeGustanA red3 usuario1) ~?= [((2, "María"), "Goodbye", [(1, "Juan"), (3, "Pedro")]), ((3, "Pedro"), "World", [(1, "Juan")])] 
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
    -- Caso en donde el usuario solo tiene un seguidor fiel
    " tieneUnSeguidorFiel 1" ~: (tieneUnSeguidorFiel redA usuario1) ~?= True,
    -- Caso en donde el usuario no tiene ningun seguidor fiel
    " tieneUnSeguidorFielFalse" ~: (tieneUnSeguidorFiel redC usuario1) ~?= False,
    -- Caso en donde el usuario tiene multiples seguidores fieles
    " tieneMultiplesSeguidoresFieles" ~: (tieneUnSeguidorFiel redC usuario6) ~?= True
 ]
 
testEjercicio10 = test [
    " existeSecuenciaDeAmigos 1" ~: (existeSecuenciaDeAmigos redA usuario1 usuario3) ~?= True,
    -- Caso en donde NO existe un secuencia de amigos
    "existeSecuenciaDeAmigos 2" ~: (existeSecuenciaDeAmigos redC usuario1 usuario7) ~?= False,
    -- El caso de la catedra prueba una cadena 'larga' (osea los usuarios en cuestion no son amigos), este caso prueba el caso 'corto' los usuario son amigos
    "existeSecuenciaDeAmigos 2" ~: (existeSecuenciaDeAmigos redC usuario1 usuario6) ~?= True

 ]


expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)

-- IMPORTANTE!!!! 

-- NO AGREAGUEN ''LIKES'' EN PUBLIACIONES QUE NO SEAN CREADAS POR USTEDES
-- NO MODIFIQUEN LAS REDES DEFAULT NI LAS QUE HAYAN HECHO OTRAS PERSONAS
-- NO PASA NADA SI USAN UNA RED CREADA POR OTRA PERSONA EN SU TESTEO, PERO NO LA MODIFIQUEN!!!!!
-- PARA CREAR REALCIONES NUEVAS, USEN SOLAMENTE LOS USUARIOS DEFAULT Y LOS SUYOS, DE LO CONTRARIO SE PUEDEN SOLAPAR


-- Usuarios 'Default'
usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")

-- Usuarios creados por Lucas para testear los ejercicios 4 y 8
usuario6 = (6, "Lucas")
usuario7 = (7, "Connie")
usuario8 = (8, "Susy")
usuario9 = (9, "Alfonso")


-- Usuarios creados por Ana para testear los ejercicios 1 y 5
usuario10 = (10, "Ana")
usuario11 = (11, "Mia")

-- Usuarios creados por Jos para testear los ejercicios ... 


-- Relaciones "Default"
relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) -- Notar que el orden en el que aparecen los usuarios es indistinto
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)

-- Relaciones creadas por Lucas
relacion1_5 = (usuario1, usuario5)
relacion1_6 = (usuario1, usuario6)
relacion1_7 = (usuario1, usuario7)
relacion1_8 = (usuario1, usuario8)
relacion1_9 = (usuario1, usuario9)

-- Relaciones creadas por Ana 


-- Relaciones creadas por Berny 


-- Relaciones creadas por Jos




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

-- Publicaciones de Lucas
publicacion6_1 = (usuario6, "Mr. Postman look and see",[usuario7,usuario8])
publicacion6_2 = (usuario6, "Is there a letter in your bag for me?",[usuario7,usuario8])
publicacion6_3 = (usuario6, "I've been waiting, I lost my mind", [usuario7,usuario8])
publicacion6_4 = (usuario6, "Me olvide el resto de la letra",[])

-- Publicaciones de Ana

-- Publicaciones de Berny

-- Publicaciones de Jos


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

-- Redes de Lucas

-- Motivacion Red C: El usuario con mas amigos es unico (usuario1): Usamos este dato para probar usuarioConMasAmigos, ademas
--                   prueba si funciona lesGustanLasmismasPubliaciones en dos casos, uno con dos usuarios que no le han dado like a ninguna publicacion(usuarios 1 y 2),
--                   y otro en donde los usuarios le han dado like a al menos una publicacion (usuarios 7  y 8). 
usuariosC = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7]
relacionesC = [relacion1_2,relacion1_3,relacion1_4,relacion1_5,relacion1_6]
publicacionesC = [publicacion6_1,publicacion6_2,publicacion6_3,publicacion1_4]
redC = (usuariosC, relacionesC, publicacionesC)



-- Redes de Ana

-- Redes de Berny


redD = ([usuario1, usuario2], [(usuario1, usuario2)], [])
redE = ([usuario1, usuario2, usuario3], [(usuario1, usuario2), (usuario1, usuario3)], [])
redF = ([usuario1], [], []) -- antes era redC (lo tuve que cambiar porque sobrelapaban los nombres)
redVacia = ([], [], [])

red2 = (usuarios2, relaciones2, publicaciones2)
usuarios2 = [(1, "Juan"), (2, "María")]
relaciones2 = [((1, "Juan"), (2, "María"))]
publicaciones2 = [((1, "Juan"), "Hello", [(2, "María")])]

red3 = (usuarios3, relaciones3, publicaciones3)
usuarios3 = [(1, "Juan"), (2, "María"), (3, "Pedro")]
relaciones3 = [((1, "Juan"), (2, "María")), ((1, "Juan"), (3, "Pedro"))]
publicaciones3 = [((1, "Juan"), "Hello", [(2, "María")]), ((2, "María"), "Goodbye", [(1, "Juan"), (3, "Pedro")]), ((3, "Pedro"), "World", [(1, "Juan")])]


-- Redes de Jos





