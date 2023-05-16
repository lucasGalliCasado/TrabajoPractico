import Test.HUnit
import Resoluciones

main = runTestTT tests

tests = test [
    " cantidadDeAmigos 1" ~: (cantidadDeAmigos redA usuario1) ~?= 2, -- test-catedra.hs

    " cantidadDeAmigos 2" ~: (cantidadDeAmigos redC usuario1) ~?= 0, -- Caso en el que usuario1 no tiene amigos en la red social

    " cantidadDeAmigos 3" ~: (cantidadDeAmigos redD usuario1) ~?= 1, -- Caso en el que usuario1 tiene un UNICO amigo

    " cantidadDeAmigos 4" ~: (cantidadDeAmigos redE usuario1) ~?= 2, -- Caso en el que usuario1 tiene mas de un amigo

    " cantidadDeAmigos 5" ~: (cantidadDeAmigos redD usuario3) ~?= 0  -- Caso en el que usuario3 no esta en la red social
            ]





usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")

relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) -- Notar que el orden en el que aparecen los usuarios es indistinto
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)

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

usuariosA = [usuario1, usuario2, usuario3, usuario4]
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]
publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2]
redA = (usuariosA, relacionesA, publicacionesA)

usuariosB = [usuario1, usuario2, usuario3, usuario5]
relacionesB = [relacion1_2, relacion2_3]
publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redB = (usuariosB, relacionesB, publicacionesB)

redC = ([usuario1], [], [])
redD = ([usuario1, usuario2], [(usuario1, usuario2)], [])
redE = ([usuario1, usuario2, usuario3], [(usuario1, usuario2), (usuario1, usuario3)], [])