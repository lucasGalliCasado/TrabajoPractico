-- Aca se encuentran las funciones auxiliares de caracter mas generico, dicho de otro modo: que no son exclusivas a ejericios del TP


pertenece :: t -> [t] -> Bool
pertenece t [] =  False
pertenece t (ts,tss) | t == ts = True
                     | otherwise = pertenece t tss


                     