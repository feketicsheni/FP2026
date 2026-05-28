import Data.ByteString.Char8 qualified as B
import Data.List
import System.IO

data Film = Film
  { ev :: B.ByteString,
    nev :: B.ByteString,
    hossz :: B.ByteString,
    tipus :: B.ByteString,
    indexe :: B.ByteString,
    dij :: B.ByteString,
    szinesz :: B.ByteString,
    szineszno :: B.ByteString,
    rendezo :: B.ByteString
  }
  deriving (Show)

parseFilm :: B.ByteString -> Film
parseFilm sor =
  let d = B.split '\t' sor
   in Film
        (d !! 0)
        (d !! 1)
        (d !! 2)
        (d !! 3)
        (d !! 4)
        (d !! 5)
        (d !! 6)
        (d !! 7)
        (d !! 8)

-- Ugyanabban az évben készültek?
azonosEv :: Film -> Film -> Bool
azonosEv f1 f2 = ev f1 == ev f2

-- Rendezés év szerint
rendezEv :: [Film] -> [Film]
rendezEv = sortBy (\a b -> compare (ev a) (ev b))

-- Különböző színészek
kulonbozoSzineszek :: [Film] -> [B.ByteString]
kulonbozoSzineszek lista =
  nub [szinesz x | x <- lista, szinesz x /= B.pack "Unknown"]

-- Színészek száma
szineszDb :: [Film] -> Int
szineszDb lista =
  length (kulonbozoSzineszek lista)

szinesznoDb :: [Film] -> Int
szinesznoDb lista =
  length (nub [szineszno x | x <- lista, szineszno x /= B.pack "Unknown"])

-- Azonos évű filmek csoportosítása
csoportosit :: [Film] -> [[Film]]
csoportosit lista =
  groupBy
    (\a b -> ev a == ev b)
    (rendezEv lista)

-- Egy adott színész filmjei
filmekSzinesszel :: B.ByteString -> [Film] -> [Film]
filmekSzinesszel nev lista =
  filter (\x -> szinesz x == nev) lista

main :: IO ()
main = do
  tartalom <- B.readFile "film.txt"

  let filmek = map parseFilm (B.lines tartalom)

  -- rendezett filmnevek kiírása
  let rendezett = rendezEv filmek

  B.writeFile
    "rendFilm.txt"
    (B.unlines [nev x | x <- rendezett])

  -- színészek kiírása
  B.writeFile
    "szineszek.txt"
    (B.unlines (kulonbozoSzineszek filmek))

  putStrLn ("Színészek száma: " ++ show (szineszDb filmek))
  putStrLn ("Színésznők száma: " ++ show (szinesznoDb filmek))

  let csoportok = csoportosit filmek
  print csoportok

  putStrLn "Színész neve:"
  nevSz <- B.getLine

  print (filmekSzinesszel nevSz filmek)