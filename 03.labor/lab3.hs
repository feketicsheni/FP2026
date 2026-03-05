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
