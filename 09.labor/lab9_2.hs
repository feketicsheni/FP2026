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

import Data.Char
import Data.List

-- =========================
-- 1. Beolvasás + rendezés
-- =========================

readAndSortIbans :: FilePath -> IO [String]
readAndSortIbans file = do
  content <- readFile file
  return (sort (lines content))

-- =========================
-- 2. Bináris keresés
-- =========================

binarySearch :: (Ord a) => [a] -> a -> Bool
binarySearch [] _ = False
binarySearch xs target
  | midVal == target = True
  | target < midVal = binarySearch left target
  | otherwise = binarySearch right target
  where
    mid = length xs `div` 2
    midVal = xs !! mid
    left = take mid xs
    right = drop (mid + 1) xs

-- =========================
-- 3. IBAN validáció
-- =========================

validChars :: String -> Bool
validChars = all (\c -> isDigit c || isUpper c)

rearrange :: String -> String
rearrange iban = drop 4 iban ++ take 4 iban

charToNum :: Char -> String
charToNum c
  | isDigit c = [c]
  | isUpper c = show (ord c - ord 'A' + 10)
  | otherwise = ""

convertIBAN :: String -> String
convertIBAN = concatMap charToNum

mod97 :: String -> Integer
mod97 = foldl (\acc c -> (acc * 10 + toInteger (digitToInt c)) `mod` 97) 0

-- =========================
-- 4. Ország hosszok
-- =========================

type CountryLen = (String, Int)

readCountryLens :: FilePath -> IO [CountryLen]
readCountryLens file = do
  content <- readFile file
  return (map parse (lines content))
  where
    parse line =
      let [c, l] = words line
       in (c, read l)

validLength :: [CountryLen] -> String -> Bool
validLength lens iban =
  case lookup (take 2 iban) lens of
    Just l -> length iban == l
    Nothing -> False

-- =========================
-- 5. Teljes IBAN ellenőrzés
-- =========================

isValidIBAN :: [CountryLen] -> String -> Bool
isValidIBAN lens iban =
  validChars iban
    && validLength lens iban
    && mod97 (convertIBAN (rearrange iban)) == 1

-- =========================
-- 6. okIban.txt írás
-- =========================

writeValidIbans :: FilePath -> FilePath -> FilePath -> IO ()
writeValidIbans ibanFile lenFile outFile = do
  ibans <- readFile ibanFile
  lens <- readCountryLens lenFile

  let valid = filter (isValidIBAN lens) (lines ibans)

  writeFile outFile (unlines valid)

-- =========================
-- 7. MAIN
-- =========================

main :: IO ()
main = do
  putStrLn "IBAN lista betoltese es rendezese..."

  sorted <- readAndSortIbans "iban.txt"
  mapM_ putStrLn sorted

  putStrLn "\nBináris keresés példa:"
  let test = "GB82WEST12345698765432"
  print (binarySearch sorted test)

  putStrLn "\nValid IBAN-ok kiírása..."
  writeValidIbans "iban.txt" "ibanLength.txt" "okIban.txt"

  putStrLn "Keszen van!"