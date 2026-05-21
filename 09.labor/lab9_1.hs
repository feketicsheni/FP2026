-- # 9. labor

-- I. Formázzuk egy adott szövegállomány tartalmát a következőképpen: azok után az írásjelek után,
-- amelyek benne vannak a $\{.,!?;\}$ halmazban szigorúan egy szóközt tegyünk, hagyjunk.

import Data.Char

specials :: [Char]
specials = ".,!?;"

formatText :: String -> String
formatText [] = []
formatText [x] = [x]
formatText (x : y : xs)
  | x `elem` specials && y /= ' ' =
      x : ' ' : formatText (y : xs)
  | x `elem` specials && y == ' ' =
      x : ' ' : formatText (dropWhile (== ' ') xs)
  | otherwise =
      x : formatText (y : xs)

main :: IO ()
main = do
  content <- readFile "input.txt"
  writeFile "output.txt" (formatText content)

-- III. Egy szövegállományban egy adott személyről következő adatok vannak eltárolva: vezetéknév, keresztnév, születési dátum. Hozzuk létre a következő típusú adatszerkezeteket, majd olvassuk ki az adatokat az állományból és állapítsuk meg mindegyik személyről, hogy a hét milyen napján született és mikor van a névnapja. A névnapok megállapításához használhatjuk a [névnapokat](https://www.ms.sapientia.ro/~mgyongyi/Funk_Log/nevnapok.txt) tartalmazó szövegállományt.

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
