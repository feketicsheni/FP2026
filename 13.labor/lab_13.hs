-- -- -- # 13. labor
-- -- -- I. A tudosok.json állomány JSON szerkezetű, tudósok adatait tárolja: vezetéknév, nemzetiség, születési év és elhalálozási év. Egy ilyen szerkezetű állomány tartalma a következő lehet:
-- -- -- ```json
-- -- -- {"tudosok":[
-- -- --   {"nev" :"Euler",
-- -- --   "nemzetiseg" :"svajc",
-- -- --   "szEv" :1707,
-- -- --   "hEv" :1783
-- -- --   },
-- -- --   {"nev" :"Bolyai Janos",
-- -- --   "nemzetiseg" :"magyar",
-- -- --   "szEv" :1802,
-- -- --   "hEv" :1860
-- -- --   },
-- -- --   {"nev" :"Perelman",
-- -- --   "nemzetiseg" :"orosz",
-- -- --   "szEv" :1966
-- -- --   }
-- -- -- ]}
-- -- -- ```
-- -- -- - Írjunk egy Haskell programot, amely
-- -- -- - az állományban levő adatok alapján létrehoz egy Tudosok adatszerkezetet a következő két adatszerkezetet használva:
-- -- --   ```haskell
-- -- --   data Tudos = Tudos {
-- -- --     nev :: String,
-- -- --     nemzetiseg :: String,
-- -- --     szEv :: Int,
-- -- --     hEv :: Maybe Int
-- -- --   } deriving (Show, Read, Generic, FromJSON, ToJSON)
-- -- --   newtype Tudosok = Tudosok {
-- -- --     tudosok :: [Tudos]
-- -- --   } deriving (Show, Read, Generic, FromJSON, ToJSON)
-- -- --   ```
-- -- -- - meghatározza a tudósok születési év szerint rendezett sorrendjét,
-- -- -- - meghatározza a tudósok életkorát és abban az esetben, ha nem jelenik meg egy tudósnál elhalálozási év, az életkor helyett a kortars szót tünteti fel,
-- -- -- - meghatározza a tudósok életkor szerinti rendezett sorrendjét,
-- -- -- - egy állományba kiírja JSON formában adott nemzetiségű tudósok listáját.
-- -- {-# LANGUAGE DeriveGeneric #-}
-- -- {-# LANGUAGE OverloadedStrings #-}
-- -- //import json
-- -- import Data.ByteString.Lazy qualified as B
-- -- import Data.List
-- -- import Data.Maybe
-- -- import Data.Ord
-- -- import GHC.Generics
-- -- -- Adatszerkezetek
-- -- data Tudos = Tudos
-- --   { nev :: String,
-- --     nemzetiseg :: String,
-- --     szEv :: Int,
-- --     hEv :: Maybe Int
-- --   }
-- --   deriving (Show, Read, Generic)
-- -- instance FromJSON Tudos
-- -- instance ToJSON Tudos
-- -- newtype Tudosok = Tudosok
-- --   { tudosok :: [Tudos]
-- --   }
-- --   deriving (Show, Read, Generic)
-- -- instance FromJSON Tudosok
-- -- instance ToJSON Tudosok
-- -- -- Életkor számítás
-- -- eletkor :: Tudos -> String
-- -- eletkor t =
-- --   case hEv t of
-- --     Just ev -> show (ev - szEv t)
-- --     Nothing -> "kortars"
-- -- main :: IO ()
-- -- main = do
-- --   tartalom <- B.readFile "tudosok.json"
-- --   let Just adat = decode tartalom :: Maybe Tudosok
-- --   let lista = tudosok adat
-- --   putStrLn "Szuletesi ev szerint rendezve:"
-- --   let rendezettSz = sortBy (comparing szEv) lista
-- --   mapM_ print rendezettSz
-- --   putStrLn "\nEletkorok:"
-- --   mapM_ (\t -> putStrLn (nev t ++ ": " ++ eletkor t)) lista
-- --   putStrLn "\nEletkor szerint rendezve:"
-- --   let rendezettKor =
-- --         sortBy
-- --           ( comparing
-- --               ( \t ->
-- --                   case hEv t of
-- --                     Just ev -> ev - szEv t
-- --                     Nothing -> maxBound :: Int
-- --               )
-- --           )
-- --           lista
-- --   mapM_ print rendezettKor
-- --   putStrLn "\nAdj meg egy nemzetiseget:"
-- --   n <- getLine
-- --   let szurt = filter (\t -> nemzetiseg t == n) lista
-- --   B.writeFile "nemzetiseg.json" (encode (Tudosok szurt))
-- --   putStrLn "A fajl letrejott."
-- -- -- II. Az autok.json állomány JSON szerkezetű, személygépkocsik adatait tárolja: gyártmány (String), modell (String), évjárat (Int). Írjunk egy Haskell-programot, amely
-- -- -- - kiírja a képernyőre a személygépkocsik adatait, az évjárat szerinti mező alapján rendezve, minden sorba egy gyártmány, modell, illetve évjárat értéket írva,
-- -- -- - létrehoz egy gyartmany.json JSON formátumú állományt, amelybe átírja megadott gyártmányú személygépkocsik adatait, pontosabban a modell és évjárat értékeket, ahol a keresett gyártmány értékét a billentyűzetről olvassuk be,
-- -- -- - létrehoz egy autokJavitva.json JSON formátumú állományt, amelybe a személygépkocsik adatait úgy írja át, hogy minden gyártmánynév, illetve modellnév esetében ha kisbetűvel kezdődik, akkor a kezdőbetűt átalakítja nagybetűvé.
-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE OverloadedStrings #-}

-- import Data.Aeson
-- import Data.ByteString.Lazy qualified as B
-- import Data.Char
-- import Data.List
-- import Data.Ord
-- import GHC.Generics

-- data Auto = Auto
--   { gyartmany :: String,
--     modell :: String,
--     evjarat :: Int
--   }
--   deriving (Show, Generic)

-- instance FromJSON Auto

-- instance ToJSON Auto

-- newtype Autok = Autok
--   { autok :: [Auto]
--   }
--   deriving (Show, Generic)

-- instance FromJSON Autok

-- instance ToJSON Autok

-- -- Első betű nagybetűs

-- nagyKezdo :: String -> String
-- nagyKezdo [] = []
-- nagyKezdo (x : xs) = toUpper x : xs

-- javit :: Auto -> Auto
-- javit a =
--   Auto
--     (nagyKezdo (gyartmany a))
--     (nagyKezdo (modell a))
--     (evjarat a)

-- main :: IO ()
-- main = do
--   tartalom <- B.readFile "autok.json"

--   let Just adat = decode tartalom :: Maybe Autok
--   let lista = autok adat

--   putStrLn "Autok evjarat szerint rendezve:\n"

--   let rendezett = sortBy (comparing evjarat) lista

--   mapM_
--     ( \a ->
--         putStrLn $
--           gyartmany a
--             ++ " "
--             ++ modell a
--             ++ " "
--             ++ show (evjarat a)
--     )
--     rendezett

--   putStrLn "\nAdj meg egy gyartmanyt:"
--   gy <- getLine

--   let szurt =
--         map
--           (\a -> (modell a, evjarat a))
--           (filter (\a -> gyartmany a == gy) lista)

--   B.writeFile "gyartmany.json" (encode szurt)

--   let javitott = map javit lista

--   B.writeFile "autokJavitva.json" (encode (Autok javitott))

--   putStrLn "A fajlok letrejottek."

-- -- -- III. A betegek.json állomány JSON szerkezetű, betegek adatait tárolja: név (String), ország (String), születési év (Int), betegségek ([String]). Írjunk egy Haskell-programot, amely feldolgozza az állományban levő adatokat és

-- -- -- - kiírja a képernyőre egy adott országon belül a betegségeket és a betegségek számát, ahol az országnevet a billentyűzetről olvassuk be,
-- -- -- - meghatározza, hogy melyik országban van a legtöbb fajta betegség,
-- -- -- - létrehoz egy orszag.json JSON formátumú állományt, amelybe átírja megadott országú betegek adatait, pontosabban a nevet, születési évet és a betegségeket, ahol a keresett ország nevét a billentyűzetről olvassuk be.

-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE OverloadedStrings #-}

-- import qualified Data.ByteString.Lazy as B
-- import Data.List
-- import Data.Ord
-- import qualified Data.Map as Map

-- data Beteg = Beteg {
--     nev :: String,
--     orszag :: String,
--     szEv :: Int,
--     betegsegek :: [String]
-- } deriving (Show, Generic)

-- instance FromJSON Beteg
-- instance ToJSON Beteg

-- newtype Betegek = Betegek {
--     betegek :: [Beteg]
-- } deriving (Show, Generic)

-- instance FromJSON Betegek
-- instance ToJSON Betegek

-- -- Betegségek darabszáma

-- betegsegDb :: [String] -> [(String, Int)]
-- betegsegDb lista =
--     Map.toList $
--     Map.fromListWith (+) [(x,1) | x <- lista]

-- main :: IO ()
-- main = do
--     tartalom <- B.readFile "betegek.json"

--     let Just adat = decode tartalom :: Maybe Betegek
--     let lista = betegek adat

--     putStrLn "Adj meg egy orszagot:"
--     o <- getLine

--     let orszagBetegek =
--             filter (\b -> orszag b == o) lista

--     let osszesBet =
--             concatMap betegsegek orszagBetegek

--     putStrLn "\nBetegsegek es darabszamuk:\n"

--     mapM_
--         (\(b,db) ->
--             putStrLn (b ++ ": " ++ show db))
--         (betegsegDb osszesBet)

--     -- legtöbb fajta betegség

--     let orszagok =
--             nub (map orszag lista)

--     let fajtak orsz =
--             length . nub . concatMap betegsegek $
--             filter (\b -> orszag b == orsz) lista

--     let legtobb =
--             maximumBy
--                 (comparing snd)
--                 [(o, fajtak o) | o <- orszagok]

--     putStrLn "\nLegtobb fajta betegseg:"
--     print legtobb

--     -- ország.json

--     putStrLn "\nAdj meg egy orszagot az exporthoz:"
--     keresett <- getLine

--     let szurt =
--             map
--                 (\b -> (nev b, szEv b, betegsegek b))
--                 (filter (\b -> orszag b == keresett) lista)

--     B.writeFile "orszag.json" (encode szurt)

--     putStrLn "Az orszag.json fajl letrejott."