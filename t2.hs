module T2 where

import Data.List (permutations)
import Data.Char (digitToInt)

-- 2.1. Дано расстояние в сантиметрах. Найти число полных метров в нем.
smToMeter :: Integral a => a -> a
smToMeter = flip div 100

-- 2.2. Дана масса в килограммах. Найти число полных центнеров в ней.
kgToCentum :: Integral a => a -> a
kgToCentum = flip div 100

-- 2.3. Дана масса в килограммах. Найти число полных тонн в ней.
kgToTone :: Integer -> Integer
kgToTone = flip div 1000

-- 2.4. Дано расстояние в метрах. Найти число полных километров в нем.
meterToKm :: Integer -> Integer
meterToKm = flip div 1000

-- 2.5. С некоторого момента прошло 234 дня. Сколько полных недель прошло за этот период?
daysToWeeks :: Integer -> Integer
daysToWeeks = flip div 7

-- 2.6. С начала суток прошло n секунд. Определить:
-- а) сколько полных часов прошло с начала суток;
-- б) сколько полных минут прошло с начала очередного часа;
-- в) сколько полных секунд прошло с начала очередной минуты.
secToHourMinutesSec :: Integral t => t -> (t, t, t)
secToHourMinutesSec n = (h, m, s) where
    h = div n 360
    m = div n 60
    s = n - m * 60

-- 2.8. Дано целое число k (1 ≤ k ≤ 365). Присвоить целочисленной величине n значение 1, 2, ..., 6 или 0 в зависимости от того, на какой день недели (понедельник, вторник, ..., субботу или воскресенье) приходится k-й день года, в котором 1 января:
-- а) понедельник;
-- б) вторник;
-- в)* d-й день недели (если 1 января — понедельник, то d 1 , если вторник — d 2 , ..., если воскресенье — d 7 ).
dayOfYearToDayOfWeek :: Integral a => a -> a -> a
dayOfYearToDayOfWeek k d = mod (k+d-1) 7

-- 2.9. С начала 1990 года по некоторый день прошло n месяцев и 2 дня. Присвоить целочисленной величине x значение 1, 2, ..., 11 или 12 в зависимости от того, каким месяцем (январем, февралем и т. п.) является месяц этого дня. Например, при n 3 значение х равно 4.
whatMounth :: Integral a => a -> a
whatMounth n = mod n 12 + 1

-- 2.10. Дано двузначное число. Найти:
-- а) число десятков в нем;
-- б) число единиц в нем;
-- в) сумму его цифр;
-- г) произведение его цифр.
doubleDigit :: Integral t => t -> (t, t, t, t)
doubleDigit n = (ten, one, s, m) where
    ten = div n 10
    one = mod n 10
    s   = ten + one
    m   = ten * one

-- 2.11. Дано двузначное число. Получить число, образованное при перестановке цифр заданного числа.
doubleDigitReverse :: Integral a => a -> a
doubleDigitReverse n = div n 10 + (mod n 10 * 10)

-- 2.12. Дано трехзначное число. Найти:
-- а) число единиц в нем;
-- б) число десятков в нем;
-- в) сумму его цифр;
-- г) произведение его цифр.
threeDigit :: Integral t => t -> (t, t, t, t)
threeDigit n = (one, ten, s, m) where
    one = mod (mod n 100) 10
    ten = div (mod n 100) 10
    s   = div n 100 + one + ten
    m   = div n 100 * one * ten

-- 2.13. Дано трехзначное число. Найти число, полученное при прочтении его цифр справа налево.
threeDigitReverse :: Integral a => a -> a
threeDigitReverse n = div n 100 + div (mod n 100) 10 * 10 + mod (mod n 100) 10 * 100

-- 2.14. Дано трехзначное число. В нем зачеркнули первую слева цифру и приписалиее в конце. Найти полученное число.
threeDigitReverse2 :: Integral a => a -> a
threeDigitReverse2 n = div n 100 + div (mod n 100) 10 * 100 + mod (mod n 100) 10 * 10

-- 2.15. Дано трехзначное число. В нем зачеркнули последнюю справа цифру и приписали ее в начале. Найти полученное число.
threeDigitReverse3 :: Integral a => a -> a
threeDigitReverse3 n = div n 100 * 10 + div (mod n 100) 10 + mod (mod n 100) 10 * 100

-- 2.16. Дано трехзначное число. Найти число, полученное при перестановке первой и второй цифр заданного числа.
threeDigitReverse4 :: Integral a => a -> a
threeDigitReverse4 n = div n 100 * 10 + div (mod n 100) 10 * 100 + mod (mod n 100) 10

-- 2.17. Дано трехзначное число. Найти число, полученное при перестановке второй и третьей цифр заданного числа.
threeDigitReverse5 :: Integral a => a -> a
threeDigitReverse5 n = div n 100 * 100 + div (mod n 100) 10 + mod (mod n 100) 10 * 10

-- 2.18. Дано трехзначное число, в котором все цифры различны. Получить шесть чисел, образованных при перестановке цифр заданного числа.
threeDigitPerm :: Int -> [String]
threeDigitPerm = permutations . show

-- 2.19. Дано четырехзначное число. Найти:
-- а) сумму его цифр;
-- б) произведение его цифр.
fourDigit :: Int -> (Int, Int)
fourDigit n = (summum,multi) where
    summum = foldr ((+) . digitToInt) 0 (show n)
    multi  = foldr ((*) . digitToInt) 1 (show n)

-- 2.20. Дано четырехзначное число. Найти:
-- а) число, полученное при прочтении его цифр справа налево;
-- б) число, образуемое при перестановке первой и второй, третьей и четвертой
-- цифр заданного числа. Например, из числа 5434 получить 4543, из числа
-- 7048 — 784;
-- в) число, образуемое при перестановке второй и третьей цифр заданного чис-
-- ла. Например, из числа 5084 получить 5804;
-- г) число, образуемое при перестановке двух первых и двух последних цифр за-
-- данного числа. Например, из числа 4566 получить 6645, из числа 7304 — 473.
-- Последнюю задачу решить двумя способами:
-- 1) с выделением отдельных цифр заданного числа;
-- 2) без выделения отдельных цифр заданного числа.
fourDigitReverse :: Int -> Int
fourDigitReverse = read . reverse . show

fourDigitCoupleReverse :: Int -> Int
fourDigitCoupleReverse x = read $ uncurry (++) $ (\ y -> (f $ take 2 y, f $ drop 2 y)) $ show x where
    f = reverse

fourDigitTwoThreeReverse :: Int -> Int
fourDigitTwoThreeReverse x = read $ change $ show x where
    change (h:y:z:l) = h:z:y:l
    change [h,y]     = [h,y]
    change [h]       = [h]
    change []        = []

fourDigitCouple :: Int -> Int
fourDigitCouple x = read $ concat $ (\ y -> [drop 2 y, take 2 y]) $ show x

-- 2.21. Дано натуральное число n (n > 9). Найти:
-- а) число единиц в нем;
-- б) число десятков в нем.

decOne :: Integral t => t -> (t, t)
decOne n = (n, div n 10)

-- 2.22. Дано натуральное число n (n > 99). Найти:
-- а) число десятков в нем;
-- б) число сотен в нем

decHun :: Integral t => t -> (t, t)
decHun n = (div n 10, div n 100)

-- 2.24. Из трехзначного числа x вычли его последнюю цифру. Когда результат разделили на 10, а к частному слева приписали последнюю цифру числа x, то получилось число 237. Найти число x. (x - 2) / 10 + 200 = 237, x = 10 * (237 - 200) + 2

threeDigitX :: Int
threeDigitX = 10 * (237 - 200) + 2

-- 2.25. Из трехзначного числа x вычли его последнюю цифру. Когда результат разделили на 10, а к частному слева приписали последнюю цифру числа x, то получилось число n. Найти число n. По заданному n найти число x (значение n вводится с клавиатуры, 10 ≤ n ≤ 999 и при этом число десятков в n не равно нулю). (x - 2) / 10 + 200 = n, x = 10 * (n - 200) + 2
threeDigitX' :: Integral t => t -> t
threeDigitX' n = 10 * (n - 200) + 2

-- 2.26. В трехзначном числе x зачеркнули первую цифру. Когда оставшееся число умножили на 10, а произведение сложили с первой цифрой числа x, то получилось число 564. Найти число x.
-- 4yz -> (xyz - 400) * 10 + 4 = 564, (564-4)/10 + 400 = xyz

threeDigit26 :: Double
threeDigit26 = (564 - 4) / 10 + 400

-- 2.27. В трехзначном числе x зачеркнули первую цифру. Когда полученное число умножили на 10, а произведение сложили с первой цифрой числа x, то получилось число n. По заданному n найти число x (значение n вводится с клавиатуры, 1 ≤ n ≤ 999).

threeDigit27 :: Fractional a => a -> a
threeDigit27 n = (n - 4) / 10 + 400

-- 2.28. В трехзначном числе x зачеркнули его вторую цифру. Когда к образованному при этом двузначному числу слева приписали вторую цифру числа x, то получилось число 546. Найти число x.
-- x5z -> yxz = 546

threeDigit28 :: Int
threeDigit28 = read [x !! 1, head x, x !! 2] where
    x = show (546:: Int)

-- 2.29. В трехзначном числе x зачеркнули его вторую цифру. Когда к образованному при этом двузначному числу слева приписали вторую цифру числа x, то получилось число n. По заданному n найти число n (значение n вводится с клавиатуры, 10 ≤ n ≤ 999 и при этом число десятков в n не равно нулю).

threeDigit29 :: Int -> Int
threeDigit29 n = read [x !! 1, head x, x !! 2] where
    x = show n

-- 2.30. В трехзначном числе x зачеркнули его вторую цифру. Когда к образованному при этом двузначному числу справа приписали вторую цифру числа x, то получилось число 456. Найти число x.
-- xyz = 465 -> xzy = 456

threeDigit30 :: Int
threeDigit30 = read [head x, x !! 2, x !! 1] where
    x = show (456 :: Int)

-- 2.31. В трехзначном числе x зачеркнули его вторую цифру. Когда к образованному при этом двузначному числу справа приписали вторую цифру числа x, то получилось число n. По заданному n найти число x (значение n вводится с клавиатуры, 100 ≤ n ≤ 999).

threeDigit31 :: Int -> Int
threeDigit31 n = read [head x, x !! 2, x !! 1] where
    x = show (n :: Int)

-- 2.32. В трехзначном числе x зачеркнули его последнюю цифру. Когда в оставшемся двузначном числе переставили цифры, а затем приписали к ним слева последнюю цифру числа x, то получилось число 654. Найти число x. xyz -> zyx = 654

threeDigit32 :: Int
threeDigit32 = read $ reverse x where
    x = show (654 :: Int)

-- 2.33. В трехзначном числе x зачеркнули его последнюю цифру. Когда в оставшемся двузначном числе переставили цифры, а затем приписали к ним слева последнюю цифру числа x, то получилось число n. По заданному n найти число x (значение n вводится с клавиатуры, 1 ≤ n ≤ 999 и при этом число единиц в n не равно нулю).

threeDigit33 :: Int -> Int
threeDigit33 n = read $ reverse x where
    x = show n

-- 2.34. Даны цифры двух целых чисел: двузначного a2 a1 и однозначного b, где a1 — число единиц, a2 — число десятков. Получить цифры числа, равного сумме заданных чисел (известно, что это число двузначное). Слагаемое — двузначное число и число-результат не определять; условный оператор не использовать.

sum3Digit34 :: Integral t => t -> t -> t -> (t, t)
sum3Digit34 a1 a2 b = (div x 10, mod x 10) where
    x = a1 + a2*10 + b

-- 2.35. Даны цифры двух двузначных чисел, записываемых в виде a2 a1 и b2 b1 , где a1 и b1 — число единиц, a2 и b2 — число десятков. Получить цифры числа, равного сумме заданных чисел (известно, что это число двузначное). Слагаемое — двузначное число и число-результат не определять; условный оператор не использовать.
sum3Digit35 :: Integral t => t -> t -> t -> t -> (t, t)
sum3Digit35 a1 a2 b1 b2 = (div x 10, mod x 10) where
    x = a1 + a2*10 + b1 + b2*10

-- 2.36. Даны цифры двух десятичных целых чисел: трехзначного a3 a2 a1 и двузначного b2 b1 , где a1 и  b1 — число единиц, a2 и  2 — число десятков, a3 — число сотен. Получить цифры числа, равного сумме заданных чисел (известно, что это число трехзначное). Числа-слагаемые и число-результат не определять; условный оператор не использовать.
sum3Digit36 :: Integral t => t -> t -> t -> t -> t -> (t, t, t)
sum3Digit36 a1 a2 a3 b1 b2 = (div x 100, div (mod x 100) 10, mod x 10) where
    x = a1 + a2*10 + a3*100 + b1 + b2*10

-- 2.37. Даны целое число k (1 <= k <= 180) и последовательность цифр 10111213...9899, в которой выписаны подряд все двузначные числа. Определить:
-- а) номер пары цифр, в которую входит k-я цифра;
-- б) двузначное число, образованное парой цифр, в которую входит k-я цифра;
-- в) k-ю цифру, если известно, что:
-- k — четное число;
-- k — нечетное число.


digitFromK37 :: (Integral t1, Integral t) => t -> (Int, t1, t1)
digitFromK37 x = (n,m,e) where
    n | even x    = floor ((fromIntegral x / 2) :: Double)
      | otherwise = floor (((fromIntegral x + 1) / 2) :: Double)
    m = list !! (n - 1)
    list = [10,11..99]
    e | even x    = mod m 10
      | otherwise = div m 10

-- 2.38. Даны целое число k (1 k 150) и последовательность цифр 101102103...149150, в которой выписаны подряд все трехзначные числа от 101 до 150. Определить k-ю цифру, если известно, что:
-- k — число, кратное трем;
-- k — одно из чисел 1, 4, 7, ...;
-- k — одно из чисел 2, 5, 8, ...
digitFromK38 :: (Enum t1, Num t1, Integral t) => t -> t1
digitFromK38 k = x where
    n | mod k 3 == 0      = truncate ((fromIntegral k / 3) :: Double)
      | k `elem` [1,4..150] = truncate ((fromIntegral (k + 2) / 3) :: Double)
      | k `elem` [2,5..150] = truncate ((fromIntegral (k + 1) / 3) :: Double)
      | otherwise         = 0
    list                  = [101,102..150]
    x                     = list !! (n - 1)

-- 2.39. Даны целые числа h, m, s (0 < h ≤ 23, 0 ≤ m ≤ 59, 0 ≤ s ≤ 59), указывающие момент времени: "h часов, m минут, s секунд". Определить угол (в градусах) между положением часовой стрелки в начале суток и в указанный момент времени.

timeToAngle :: (Fractional a1, Integral a) => a -> a1 -> a1 -> a1
timeToAngle h m s = fromIntegral (mod h 12) * 30 + m * 0.5 + s * 0.0083

-- 2.40. С начала суток часовая стрелка повернулась на y градусов (0 ≤ y < 360, y — вещественное число). Определить число полных часов и число полных минут, прошедших с начала суток.

angleToHourMin :: (Integral t1, RealFrac t) => t -> (t1, t1)
angleToHourMin  g = (h,m) where
    n = truncate $ g * 10
    h = div n 300
    m = div (mod n 300) 5

-- 2.41. Часовая стрелка образует угол y с лучом, проходящим через центр и через точку, соответствующую 12 часам на циферблате, 0 < y ≤ 2Пи . Определить значение угла для минутной стрелки, а также количество полных часов и полных минут.
-- radianToGradus -> angleHour -> (H,m) -> minuteToAngle -> (minuteAngle, hour, minute)

gradusToRadian :: Floating a => a -> a
gradusToRadian grad = pi / 180 * grad

radianToGradus :: Floating a => a -> a
radianToGradus rad = 180 / pi * rad


minuteRadToGradus :: (Fractional a, Floating t, RealFrac t) => t -> a
minuteRadToGradus rad = 0.5 * fromInteger (snd $ angleToHourMin $ radianToGradus rad)


minGradAndTime :: (Floating t3, Floating t2, Integral t1, Integral t, RealFrac t2) => t2 -> (t3, t1, t)
minGradAndTime rad = (minGrad, hour, minute) where
    minGrad = gradusToRadian $ minuteRadToGradus rad
    hour    = fst $ angleToHourMin $ radianToGradus rad
    minute  = snd $ angleToHourMin $ radianToGradus rad

-- 2.42. Даны целые числа h, m (0 < h ≤ 12, 0 ≤ m ≤ 59), указывающие момент времени: "h часов, m минут". Определить наименьшее время (число полных минут), которое должно пройти до того момента, когда часовая и минутная стрелки на циферблате:
-- а) совпадут;
-- б) расположатся перпендикулярно друг другу.
-- HourMinToGradus -> (hGrad,mGrad) -> minuteToGradus ->



hourMinZeroAngele :: Int -> Integer -> Integer
hourMinZeroAngele h m | m <= a    = a - m
                      | otherwise = b + 60 - m where
                          a = listZeroGenerator !! mod h 12
                          b = listZeroGenerator !! mod (h + 1) 12

minZeroAngle :: (Integral b, RealFrac a) => a -> b
minZeroAngle h = truncate $ 60 * h / 11

listZeroGenerator :: [Integer]
listZeroGenerator = [minZeroAngle (h :: Double) | h <- [0..12]]

list90Angle :: (Integral a1, RealFrac a, Enum a) => a -> (a1, a1)
list90Angle h | h `elem` [0..2] = (t $ plus + 1, t $ plus + 32.72)
              | h `elem` [3..8] = (t $ minus + 1, t $ plus + 1)
              | otherwise       = (t $ minus - 31.72, t $ minus + 1) where
                    plus  = (60 * h + 180) / 11
                    minus = (60 * h - 180) / 11
                    t     = truncate

list90Generator :: [(Integer, Integer)]
list90Generator = [list90Angle (h :: Double) | h <- [0..12]]

calcTime :: Int -> Integer -> Integer
calcTime h m | m <= a          = a - m
             | m > a && m <= b = b - m
             | otherwise       = c + 60 - m where
                 a = fst $ list90Generator !! h
                 b = snd $ list90Generator !! h
                 c = fst $ list90Generator !! mod (h+1) 12

-- 2.43. Даны два целых числа a и b. Если a делится на b или b делится на a, то вывести 1, иначе — любое другое число. Условные операторы и операторы цикла не использовать.

doubleDivision :: (Num t, Integral a) => a -> a -> t
doubleDivision a b | mod a b == 0 || mod b a == 0 = 1
                   | otherwise                    = 0
