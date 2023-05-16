import Test.HUnit
import Resoluciones

main = runTestTT tests

tests = test [
    " publicacionesQueLeGustanA 1" ~: (publicacionesQueLeGustanA redVacia usuario1) ~?= [], -- Caso donde se da una red social vacia

    " publicacionesQueLeGustanA 2" ~: (publicacionesQueLeGustanA red2 usuario1) ~?= [], -- Caso donde al usuario no le gusta NINGUNA publicacion

    " publicacionesQueLeGustanA 3" ~: (publicacionesQueLeGustanA red3 usuario2) ~?= [((1, "Juan"), "Hello", [(2, "María")])], -- Caso donde al usuario le gusta UNA publicacion

    " publicacionesQueLeGustanA 4" ~: (publicacionesQueLeGustanA red3 usuario1) ~?= [((2, "María"), "Goodbye", [(1, "Juan"), (3, "Pedro")]), ((3, "Pedro"), "World", [(1, "Juan")])] -- Caso donde al usuario le gustan VARIAS publicaciones

            ]


redVacia = ([], [], [])
usuario1 = (1, "Juan")
usuario2 = (2, "María")


red2 = (usuarios2, relaciones2, publicaciones2)
usuarios2 = [(1, "Juan"), (2, "María")]
relaciones2 = [((1, "Juan"), (2, "María"))]
publicaciones2 = [((1, "Juan"), "Hello", [(2, "María")])]


red3 = (usuarios3, relaciones3, publicaciones3)
usuarios3 = [(1, "Juan"), (2, "María"), (3, "Pedro")]
relaciones3 = [((1, "Juan"), (2, "María")), ((1, "Juan"), (3, "Pedro"))]
publicaciones3 = [((1, "Juan"), "Hello", [(2, "María")]), ((2, "María"), "Goodbye", [(1, "Juan"), (3, "Pedro")]), ((3, "Pedro"), "World", [(1, "Juan")])]