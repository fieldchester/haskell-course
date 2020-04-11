module Main where

-- import Lib -- Linken braucht zu lange


-- Frage 1
-- -------
-- Sieht man oft, mit return am Ende
res = [0,1,2,3] >>= ( \x -> 
          [0,1] >>= (\y ->
 return $ x + y)  )

-- Wieso nicht mit flip fmap am Ende ?
res1 = [0,1,2,3] >>= ( \x -> 
 flip fmap [0,1]  (\y ->
          x  +y)  ) -- schneller, da kein return
          
 -- Antwort: Da kann nichts mehr angehängt werden

-- Frage 2
-- -------
-- Functor für IO () : Er ist nicht vorhanden, gibts einen Grund
-- ausser dass er vielleicht nicht oft verwendet wird
-- a = fmap reverse $ putStrLn "Hello"
-- compiler error: exprected Tyep [a] found ()




-- Fragen - Ende
-- -------------
-- -------------



-- flip fmap mit ()
res2 = [0,1,2,3] >>= ( \x -> 
 flip fmap [(),()]  (\y ->
              x)  ) -- ok
