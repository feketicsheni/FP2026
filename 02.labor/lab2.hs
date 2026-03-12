-- -egy szám számjegyeinek szorzatát (2 módszerrel),
szjSzorat 0 = 1

szjSzorzat n = mod n 10 * szjSzorat (div n 10)

szjSzorat2 n
  | n < 0 = error "meg.szam"
  | div n 10 == 0 = n
  | otherwise = mod n 10 * szjSzorat2 (div n 10)

szjSzorat3 n res
  | n < 0 = error "neg szam"
  | div n 10 == 0 = res * n
  | otherwise = szjSzorat3 (div n 10) (res * (mod n 10))

-- egy szám számjegyeinek összegét (2 módszerrel),

-- egy szám számjegyeinek számát (2 módszerrel),