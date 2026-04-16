-- III.

-- - Írjunk egy Haskell-függvényt, amely egy String típusú listából meghatározza azokat a szavakat, amelyek karakterszáma a legkisebb. Például ha a lista a következő szavakat tartalmazza:  function class Float higher-order monad tuple variable Maybe recursion  akkor az eredmény-lista a következőkből áll: class Float monad tuple Maybe
-- - Írjunk egy talalat Haskell-függvényt, amely meghatározza azt a listát, amely a bemeneti listában megkeresi egy megadott elem előfordulási pozícióit.
--   Például a következő függvényhívások esetében az első az 5-ös előfordulási pozícióit, míg a második az e előfordulási pozícióinak listáját határozza meg.

--   ```haskell
--   > talalat 5 [3, 13, 5, 6, 7, 12, 5, 8, 5]
--   [2, 6, 8]
--   > talalat 'e' "Bigeri-vizeses"
--   [3,10,12]
--   ```
-- - Írjunk egy osszegT Haskell-függvényt, amely meghatározza egy (String, Int)értékpárokból álló lista esetében az értékpárok második elemeiből képzett összeget.
--   Például:

--   ```haskell
--   > ls = [("golya",120),("fecske",85),("cinege",132)]
--   > osszegT ls
--   337
--   ```
-- - Írjunk egy atlagTu Haskell-függvényt, amely egy kételemű, tuple elemtípusú lista esetében átlagértékeket számol a második elem szerepét betöltő listaelemeken. Az eredmény egy tuple elemtípusú lista legyen, amelynek kiíratása során a tuple-elemeket formázzuk, és külön sorba írjuk őket.
--   Például:

--   ```haskell
--   > :set +m
--   > ls = [("mari",[10, 6, 5.5, 8]), ("feri",[8.5, 9.5]),
--   | ("zsuzsa",[4.5, 7.9, 10]),("levi", [8.5, 9.5, 10, 7.5])]
--   > atlagTu ls
--   mari 7.375
--   feri 9.0
--   zsuzsa 7.466666666666666
--   levi 8.875
--   ```

lsNevjegy = [("mari", [10, 6, 5.5, 8]), ("feri", [8.5, 9.5]), ("zsuzsa", [4.5, 7.9, 10]), ("levi", [8.5, 9.5, 10, 7.5])]

atlagTu :: [(String, [Double])] -> [(String, Double)]
atlagTu ls = [(nev, atlag jegyek) | (nev, jegyek) <- ls]
  where
    atlag ls2 = sum ls2 / fromIntegral (length ls2)

atlagTu2 ls = mapM_
  where
    ls2 = [(nev, atlag jegyek) | (nev, jegyek) <- ls]
    atlag ls2 = sum ls2 / fromIntegral (length ls2)

main = do
  let lsNevjegy :: [(String, [Double])]
      lsNevjegy =
        [ ("mari", [10, 6, 5.5, 8]),
          ("feri", [8.5, 9.5]),
          ("zsuzsa", [4.5, 7.9, 10]),
          ("levi", [8.5, 9.5, 10, 7.5])
        ]
  mapM_ (\(nev, atlagJegyek) -> putStrLn (nev ++ " " ++ show atlagJegyek)) (atlagTu lsNevjegy)
