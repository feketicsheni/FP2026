-- III. Egy szövegállományban egy adott személyről következő adatok vannak eltárolva: vezetéknév, keresztnév, születési dátum.
-- Hozzuk létre a következő típusú adatszerkezeteket, majd olvassuk ki az adatokat az állományból és állapítsuk meg mindegyik személyről,
--  hogy a hét milyen napján született és mikor van a névnapja.
-- A névnapok megállapításához használhatjuk a [névnapokat](https://www.ms.sapientia.ro/~mgyongyi/Funk_Log/nevnapok.txt)
-- tartalmazó szövegállományt.

-- ```haskell
-- data Datum = Datum {
--   nap :: Int,
--   honap:: Int,
--   ev :: Int
-- } deriving (Show)

-- data Szemely = Szemely {
--   vnev :: [Char],
--   knev :: [Char],
--   szdatum :: Datum
-- } deriving (Show)
-- ```

import Data.List
import Data.Time

-- =========================
-- Adatszerkezetek
-- =========================

data Datum = Datum
  { nap :: Int,
    honap :: Int,
    ev :: Int
  }
  deriving (Show)

data Szemely = Szemely
  { vnev :: String,
    knev :: String,
    szdatum :: Datum
  }
  deriving (Show)

type Nevnap = (String, String)

-- =========================
-- Beolvasás
-- =========================

readPeople :: FilePath -> IO [Szemely]
readPeople file = do
  content <- readFile file
  return (map parsePerson (lines content))

parsePerson :: String -> Szemely
parsePerson line =
  let [vn, kn, n, h, e] = words line
   in Szemely vn kn (Datum (read n) (read h) (read e))

-- =========================
-- Hét napja
-- =========================

dayOfWeekName :: Datum -> String
dayOfWeekName (Datum d m y) =
  show (dayOfWeek (fromGregorian (fromIntegral y) m d))

-- =========================
-- Névnapok
-- =========================

readNamedays :: FilePath -> IO [Nevnap]
readNamedays file = do
  content <- readFile file
  return (map parse (lines content))
  where
    parse line =
      let [nev, datum] = words line
       in (nev, datum)

findNameday :: [Nevnap] -> String -> Maybe String
findNameday db name = lookup name db

-- =========================
-- Kiírás
-- =========================

printPerson :: [Nevnap] -> Szemely -> IO ()
printPerson db p = do
  putStrLn (vnev p ++ " " ++ knev p)
  putStrLn ("Szuletesi nap: " ++ dayOfWeekName (szdatum p))

  case findNameday db (knev p) of
    Just d -> putStrLn ("Nevnap: " ++ d)
    Nothing -> putStrLn "Nincs nevnap"

  putStrLn ""

-- =========================
-- MAIN
-- =========================

main :: IO ()
main = do
  people <- readPeople "people.txt"
  namedays <- readNamedays "nevnapok.txt"

  mapM_ (printPerson namedays) people