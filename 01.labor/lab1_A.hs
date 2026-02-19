-- - két szám összegét, különbségét, szorzatát, hányadosát, osztási maradékát,
--osszeg :: Num a => a -> a -> a
osszeg :: Int -> Int -> Int
osszeg a b = a+b

-- egy elsofoku egyenlet gyoke
-- a*x + b =0 -> x=(-b)/a

elsof a b = (-b) /a

--szam abszolut erteke
abszolut :: (Ord a, Num a) => a -> a
abszolut a = if a < 0 then -a else a 

abszolut2 a 
    | a < 0= -a
    |otherwise = a 



--egy szam elojele

elojel a = if a<0 then "negativ " else if a>0 then "pozitiv" else "nulla" 

elojel2 a 
    |a<0 = "negativ"
    |a>0 = "pozitiv"
    |otherwise ="nulla"


-- ket argumentum kozul a maximumot 
--ket argum kozul a min

--masodfoku egyenlet gyokei
-- a *x**2 + b*x +c =0 -> a,b,c bemeneti arg
--delta= b**2 -4*a*c
--gy1= -b +sqrt delta  /2*a
 --gy2= -b -sqrt delta  /2*a

masodF a b c
    |delta <0 = error "komplex szamok"
    |otherwise = (gy1,gy2)
    where 
        delta = b ** 2 - 4 * a * c
        gy1 = ( -b + sqrt delta )  / ( 2 * a )
        gy2 = ( -b - sqrt delta ) / ( 2 * a )
    
