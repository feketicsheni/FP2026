import System.Win32 (pAGE_EXECUTE, xBUTTON1)

szjosszeg n
  | n < 0 = szjosszeg (abs n)
  | n < 10 = n
  | otherwise = mod n 10 + szjosszeg (div n 10)

szjosszeg2 n res
  | n < 0 = szjosszeg2 (abs n) res
  | n < 10 = res + n
  | otherwise = szjosszeg2 (div n 10) (res + mod n 10)

-- szám számjegyeinek száma

szjszam n res
  | n < 0 = szjszam (abs n) res
  | n < 10 = res + 1
  | otherwise = szjszam (div n 10) (res + 1)

szjszam2 n
  | n < 0 = szjszam2 (abs n)
  | n < 10 = 1
  | otherwise = 1 + szjszam2 (div n 10)

-- adott számjegy összege

szjSzamOsszeg n szj
  | szj > 9 = error "nem szj."
  | n < 10 = if n == szj then szj else 0
  | otherwise =
      if mod n 10 == szj
        then szj + szjSzamOsszeg (div n 10) szj
        else szjSzamOsszeg (div n 10) szj

szjszamossz2 n szj elof
  | szj > 9 = error "nem szj."
  | n < 10 = if n == szj then (elof + 1) * szj else elof * szj
  | otherwise =
      if mod n 10 == szj
        then szjszamossz2 (div n 10) szj (elof + 1)
        else szjszamossz2 (div n 10) szj elof

-- páros számjegyek száma

parosSzamSzj n
  | n < 0 = parosSzamSzj (abs n)
  | n < 10 = if even n then 1 else 0
  | otherwise =
      if even (mod n 10)
        then 1 + parosSzamSzj (div n 10)
        else parosSzamSzj (div n 10)

parosszamszj2 n res
  | n < 0 = parosszamszj2 (abs n) res
  | n < 10 = if even n then res + 1 else res
  | otherwise =
      if even (mod n 10)
        then parosszamszj2 (div n 10) (res + 1)
        else parosszamszj2 (div n 10) res

lgszj n ln
  | n < 0 = lgszj (abs n) ln
  | n < 10 = if n > ln then n else ln
  | otherwise =
      if mod n 10 > ln
        then lgszj (div n 10) (mod n 10)
        else lgszj (div n 10) ln

bszamrdszj n b d
  | n < 0 = bszamrdszj (abs n) b d
  | n < b = if n == d then 1 else 0
  | otherwise =
      if mod n b == d
        then 1 + bszamrdszj (div n b) b d
        else bszamrdszj (div n b) b d

fibo a b res n
  | n == 0 = res
  | otherwise = fibo b res (res + b) (n - 1)

fiboN n = fibo 0 1 0

fiboN2 n = fiboSg 0 1 0 n
  where
    fiboSg a b res n = fiboSg b res (res + b) (n - 1)
    fiboSg a b res n = fiboSg b res (res + b) (n - 1)

fibols n = map (fibo 0 1 0 n) [0 .. n]

ls3 = [(7673573, 10, 7), (1024, 2, 1), (1023, 2, 1), (345281, 16, 4)]

atlag :: (Floating a) => [a] -> a
atlag ls = (sum ls) / fromIntegral (length ls)

-- 4. ora

-- meghivas: myLength2 [1..100] 0

mylength [] = 0
mylength (x : xs) = 1 + mylength xs

myLength2 (x : xs) res = myLength2 xs (res + 1)

myLength3 ls = foldr (\x -> (+) 1) 0 ls

myLength4 ls = foldl (\db x -> (+) 1 db) 0 ls

myLength5 ls res = foldl (\x res -> (+) 1 res) res ls

myProduct [] = 1
myProduct (x : xs) = x * myProduct xs

myProduct2 [] res = res
myProduct2 (x : xs) res = myProduct2 xs (res * x)

myProduct3 ls = foldr1 (*) ls

myProduct4 ls = product ls

---lista legkissebb eleme (MyMinimum)
myMinimum [] = error "ures lisra"
myMinimum [x] = x
myMinimum (x1 : x2 : xs)
  | x1 < x2 = myMinimum (x1 : xs)
  | otherwise = myMinimum (x2 : xs)

myMinimum2 [] = error "ures lista"
myMinimum2 [x] = x
myMinimum2 (x1 : x2 : xs) =
  if x1 < x2
    then myMinimum2 (x1 : xs)
    else myMinimum2 (x2 : xs)

myMinimum3 ls = foldl1 min ls

myMinimum4 ls = minimum ls

myMaximum [] = error "ures lista"
myMaximum [x] = x
myMaximum (x1 : x2 : xs)
  | x1 > x2 = myMaximum (x1 : xs)
  | otherwise = myMaximum (x2 : xs)

myMaximum2 ls = foldr1 max ls

myMaximum3 ls = maximum ls

---lista n-edik eleme:

listaN ls n = ls !! n

listaN2 ls n
  | ls == [] = error "ures lista"
  | n < 0 = error "neg. index"
  | length ls <= n = error "tul nagy index"
  | otherwise = ls !! n

--- egymas utan fuzi a parameterenkent meggadott ket listat

lsFuz ls1 ls2 = ls1 ++ ls2

-- lista palindrom-e--true vagy false ertekeket terit vissza
palindrom ls = ls == reverse ls

palindrom2 ls =
  if ls == reverse ls
    then "palindrom"
    else "nem palindrom"

palindrom3 [] = True
palindrom3 [x] = True
palindrom3 ls = head ls == last ls && palindrom3 ((init . tail) ls)

-- egesz szam szamjegyeinek listaja

szjls x
  | x < 0 = szjls (abs x)
  | x < 10 = [x]
  | otherwise = szjls (div x 10) ++ [mod x 10]

-- a lista vegere koltozteti az elso elemet

elsoUtolso ls = tail ls ++ [head ls]

elsoUtolso2 (x : xs) = xs ++ [x]

---lista elemek atlagerteke romai 1-esbe is megvan

---10 szmrendszbeli szam p szamrendszer beli alakja

decP x p
  | x < 0 = error "negativ szam"
  | x < p = [x]
  | otherwise = decP (div x p) p ++ [mod x p]

pdec ls p = foldl (\hatvany x -> x + (p * hatvany)) 0 ls

pdec2 x p =
  let szamjegyek x
        | x < 10 = [x]
        | otherwise = mod x 10 : szamjegyek (div x 10)

      szjIdx = zip (szamjegyek x) [0 ..]
   in sum [i * (p ^ hatvany) | (i, hatvany) <- szjIdx]

--- III . alkalmazzuk a map fuggvenyt

ls1 = [[1, 2, 3], [4, 5]]

myLengthLs = map mylength ls1

listaNMap ls = map (\x -> listaN x 0) ls

listaNMap2 = map (uncurry listaN) ls2

ls2 = [([1, 2, 3], 0), ([1 .. 10], 5)]

-- ls3 = [[1..10] , [5,66]]

ls4 = [[23, 56, 7], [213, 56], [67, 32]]

-- lsFuzMap= map lsFuz (zip ls3 ls4)

x0 = 2

poli [] x = 0
poli (a : als) x = a + x * poli als x
