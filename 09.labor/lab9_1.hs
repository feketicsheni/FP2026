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

-- II. Az [iban.txt](https://www.ms.sapientia.ro/~mgyongyi/Funk_Log/iban.txt) állomány IBAN kódokat tartalmaz. Írjunk egy-egy Haskell függvényt, amely

-- - beolvassa, majd rendezi az állományban levő adatokat ábécé sorrendbe,
-- - bináris keresést alkalmazva ellenőrzi, hogy egy megadott IBAN kód szerepel-e az adatok között,
-- - átírja egy okIban.txt állományba azokat az IBAN kódokat, amelyek megfelelő formátumúak. Egy IBAN kód akkor tekinthető megfelelő formátumúnak
--   - ha csak számjegyeket és angol ábécébeli nagybetűket tartalmaz,
--   - ha az IBAN kód hossza megegyezik az országhoz tartozó hosszal, ahol az országhoz tartozó hosszérték az [ibanLength.txt](https://www.ms.sapientia.ro/~mgyongyi/Funk_Log/ibanLength.txt) állományból olvasható ki,
--   - ha az átcsoportosítás és a helyettesítés után kapott egész szám 97-el való osztási maradéka egyenlő eggyel, ahol
--     - átcsoportosítás: az IBAN kód első négy karakterét kitöröljük a kód elejéről és a kód végéhez fűzzük,
--     - helyettesítés:
--       - az alfanumerikus karaktereket helyettesítsük a következő kódokkal: $$A \to 10,\ B \to 11,\ \ldots,\ Z \to 35$$
--       - az így kapott karakterláncot egész számnak tekintjük

--   Például:
--   legyen az IBAN kód: $$\texttt{GB82WEST12345698765432}$$
--   - hossz: $$22$$
--   - átcsoportosítás:
--     $$\texttt{WEST12345698765432}\ \texttt{GB82}$$
--   - helyettesítés:
--     $$32142829\quad 12345698765432\quad 1611\quad 82$$
--   - ellenőrzés: $$3214282912345698765432161182 \bmod 97 = 1$$

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
