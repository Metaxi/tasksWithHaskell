module T4 where

import Data.Char (digitToInt)
-- import Data.List (sort)

-- 4.1. Рассчитать значение у при заданном значении х:
guard41 :: (Ord a, Floating a) => a -> a
guard41 x | x > 0     = sin x ^ (2 :: Int)
          | otherwise = 1 - 2 * sin (x^ (2::Int))

-- 4.2. Рассчитать значение у при заданном значении х:
guard42 :: (Ord a, Floating a) => a -> a
guard42 x | x > 0     = sin (x ^ (2 :: Int))
          | otherwise = 1 - 2 * sin x ^ (2::Int)

-- 4.3. Определить, в какую из областей — I или II (рис. 4.1) — попадает точка с заданными координатами. Для простоты принять, что точка не попадает на границу областей.
guard43 :: (Num t1, Num a, Ord a) => (a, a) -> t1
guard43 (x,_) | x < 4     = 1
              | otherwise = 2

-- 4.4.Определить, в какую из областей — I или II (рис. 4.2) — попадает точка с заданными координатами. Для простоты принять, что точка не попадает на границу областей.
guard44 :: (Num t1, Num a, Ord a) => (a, a) -> t1
guard44 (_,y) | y > 3     = 1
              | otherwise = 2

-- 4.5. Для функций, заданных графически (рис. 4.3), определить значение у при заданном значении х.
guard45a :: (Num t, Ord t) => t -> t
guard45a x | x > 2     = 2
           | otherwise = x

guard45b :: (Num t, Ord t) => t -> t
guard45b x | x > 3     = -3
           | otherwise = -x

-- 4.6. Даны два различных вещественных числа. Определить:
-- а) какое из них больше;
-- б) какое из них меньше.
guard46 :: Ord a => a -> a -> a
guard46 = max

-- 4.8. Известны два расстояния: одно в километрах, другое — в футах ( 1 фут 0,305 м). Какое из расстояний меньше?

guard47 :: (Fractional a, Ord a) => a -> a -> a
guard47 k f | max k (f * 0.305) == k = k
            | otherwise              = f

-- 4.9.Известны две скорости: одна в километрах в час, другая — в метрах в секунду. Какая из скоростей больше?
guard49 :: (Fractional t, Ord t) => t -> t -> t
guard49 k m | max (k*1000) m == m = m
            | otherwise           = k

-- 4.10. Даны радиус круга и сторона квадрата. У какой фигуры площадь больше?
guard410 :: (Ord t, Floating t) => t -> t -> String
guard410 r s | s^(2::Int) > r^(2::Int)*pi = "s"
             |otherwise     = "r"

-- 4.11. Даны объемы и массы двух тел из разных материалов. Материал какого из тел имеет большую плотность?
guard411 :: (Num t, Fractional a, Ord a) => (a, a) -> (a, a) -> t
guard411 (v1,m1) (v2,m2) | m1 / v1 > m2 / v2 = 1
                         | otherwise         = 2

-- 4.12. Известны сопротивления двух несоединенных друг с другом участков электрической цепи и напряжение на каждом из них. По какому участку протекает меньший ток?
guard412 :: (Fractional a, Ord a) => (a, a) -> (a, a) -> String
guard412 (o1,v1) (o2,v2) | v1 / o1 > v2 / o2  = "1"
                         | v1 / o1 == v2 / o2 = "equel"
                         | otherwise          = "2"

-- 4.13. Даны вещественные числа a, b, c (a 0). Выяснить, имеет ли уравнение ax2 + bx + c = 0 вещественные корни.
guard413 :: (Num t, Ord t) => t -> t -> t -> String
guard413 a b c | d > 0     = "2 roots"
               | d == 0    = "1 root"
               | otherwise = "no roots" where
                   d = b ^ (2::Int) - 4 * a * c

-- 4.14. Для условий предыдущей задачи в случае наличия вещественных корней найти их, в противном случае — вывести на экран соответствующее сообщение. Вариант равенства корней отдельно не рассматривать.
guard414 :: (Floating t, Show t, Ord t) => t -> t -> t -> String
guard414 a b c | d > 0     = "roots are " ++ show x1 ++ " and " ++ show x2
               | d == 0    = "root is " ++ show x3
               | otherwise = "no roots" where
                   d  = b ^ (2::Int) - 4 * a * c
                   x1 = (sqrt d - b) / 2 * a
                   x2 = (- b - sqrt d) / 2 * a
                   x3 = - b / 2 * a

-- 4.15. Известны год и номер месяца рождения человека, а также год и номер месяца сегодняшнего дня (январь — 1 и т. д.). Определить возраст человека (число полных лет). В случае совпадения указанных номеров месяцев считать, что прошел полный год.
guard415 :: (Num a1, Ord a) => a1 -> a -> a1 -> a -> a1
guard415 y m yNow mNow | mNow >= m  = yNow - y
                       | otherwise = (yNow - y) - 1

-- 4.16. Известны площади круга и квадрата. Определить:
-- а) уместится ли круг в квадрате?
-- б) уместится ли квадрат в круге?

guard416a :: (Floating t, Ord t) => t -> t -> String
guard416a circle square | r <= y / 2 = "Yes. Circle is inside square"
                        | otherwise  = "No. Circle is outside square" where
                              y = sqrt square
                              r = sqrt (circle / pi)

guard416b :: (Floating t, Ord t) => t -> t -> String
guard416b circle square | r >= sqrt 2  * y / 2 = "Yes. Square is inside circle"
                        | otherwise            = "No. Square is outside circle" where
                              y = sqrt square
                              r = sqrt (circle / pi)

-- 4.17. Известны площади круга и равностороннего треугольника. Определить:
-- а) уместится ли круг в треугольнике?
-- б) уместится ли треугольник в круге?
guard417a :: (Floating t, Ord t) => t -> t -> String
guard417a circle triangle | r <= sqrt 3 * a / 6 = "Yes. Circle is inside triange"
                          | otherwise           = "No. Circle is outside triangle" where
                              a = sqrt (4*triangle / sqrt 3)
                              r = sqrt (circle / pi)

guard417b :: (Floating t, Ord t) => t -> t -> String
guard417b circle triangle | r >= sqrt 3 * a / 3  = "Yes. Triangle is inside circle"
                          | otherwise            = "No. Triangle is outside circle" where
                              a = sqrt (4*triangle / sqrt 3)
                              r = sqrt (circle / pi)

-- 4.18.*Даны два прямоугольника, стороны которых параллельны или перпендикулярны осям координат. Известны координаты левого нижнего и правого верхнего углов каждого из них. Найти координаты левого нижнего и правого верхнего углов минимального прямоугольника, содержащего указанные прямоугольники.
minRectangle :: (Ord t1, Ord t) => (t1, t) -> (t1, t) -> (t1, t) -> (t1, t) -> [(t1, t)]
minRectangle (x1,y1) (x2,y2) (x3,y3) (x4,y4) = [(x5,y5),(x6,y6)] where
    x5 = min x1 x2
    y5 = min y1 y2
    x6 = max x3 x4
    y6 = max y3 y4

-- 4.19.*Даны два прямоугольника, стороны которых параллельны или перпендикулярны осям координат. Известны координаты левого нижнего угла каждого из них и длины их сторон. Найти координаты левого нижнего и правого верхнего углов минимального прямоугольника, содержащего указанные прямоугольники.
minRectangle2 :: (Num t1, Num t, Ord t, Ord t1) => (t1, t, t1, t) -> (t1, t, t1, t) -> [(t1, t)]
minRectangle2 (x1,y1,s1,s2) (x2,y2,s3,s4) = minRectangle (x1,y1) (x2,y2) (x3,y3) (x4,y4) where
    x3 = x1 + s1
    y3 = y1 + s2
    x4 = x2 + s3
    y4 = y2 + s4

-- 4.20. Если целое число m делится нацело на целое число n, то вывести на экран частное от деления, в противном случае вывести сообщение "m на n нацело не делится".
intMod :: Integer -> Integer -> String
intMod m n | mod m n == 0 = show (division :: Double)
           | otherwise    = "m on n doesn`t divide" where
               division = fromInteger m / fromInteger n

-- 4.21. Определить, является ли число a делителем числа b?
isDivision :: Integral a => a -> a -> Bool
isDivision b a | mod b a == 0 = True
               | otherwise    = False

-- 4.22. Дано натуральное число. Определить:
-- а) является ли оно четным;
-- б) оканчивается ли оно цифрой 7.
evenSeven :: Int -> (Bool, Bool)
evenSeven x = (isEven, isSeven) where
    isEven  = even x
    isSeven = mod x 10 == 7

-- 4.23. Дано двузначное число. Определить:
-- а) какая из его цифр больше: первая или вторая;
-- б) одинаковы ли его цифры.
maxEqual :: Int -> (Int, Bool)
maxEqual xx = (maxDigit, equalDigit) where
    maxDigit   = max (div xx 10) (mod xx 10)
    equalDigit = div xx 10 == mod xx 10

-- 4.24. Дано двузначное число. Определить, равен ли квадрат этого числа учетверенной сумме кубов его цифр. Например, для числа 48 ответ положительный, для числа 52 — отрицательный.
equalDigits :: Int -> Bool
equalDigits yy = yy ^ (2::Int) == 4 * mod yy 10 ^ (3::Int) + 4 * div yy 10 ^ (3::Int)

-- 4.25. Дано двузначное число. Определить:
-- а) является ли сумма его цифр двузначным числом;
-- б) больше ли числа а сумма его цифр.
sumMore :: Integral t => t -> t -> (Bool, Bool)
sumMore xx a = (isEvenDigits, moreThenA) where
    isEvenDigits = even sumXX
    moreThenA    = sumXX > a
    sumXX        = div xx 10 + mod xx 10

-- 4.26. Дано двузначное число. Определить:
-- а) кратна ли трем сумма его цифр;
-- б) кратна ли сумма его цифр числу а.
multiple :: Integral t => t -> t -> (Bool,Bool)
multiple xx a = (multipleThree, multipleSumThree) where
    multipleThree    = mod sumXX 3 == 0
    multipleSumThree = mod sumXX a == 0
    sumXX            = div xx 10 + mod xx 10

-- 4.27. Дано трехзначное число. Выяснить, является ли оно палиндромом ("перевертышем"), т. е. таким числом, десятичная запись которого читается одинаково слева направо и справа налево.
paliandrome :: Int -> Bool
paliandrome x = show x == reverse (show x)

-- 4.28. Дано трехзначное число. Определить, какая из его цифр больше:
-- а) первая или последняя;
-- б) первая или вторая;
-- в) вторая или последняя.
whichDigitMore :: Integral t => t -> (t, t, t)
whichDigitMore x = (a,b,c) where
    a = max first third
    b = max first second
    c = max second third
    first  = div x 100
    second = mod (div x 10) 10
    third  = mod x 10

-- 4.29. Дано трехзначное число. Определить, равен ли квадрат этого числа сумме кубов его цифр.
squareCube :: Int -> Bool
squareCube y = y ^ (2::Int) == mod y 10 ^ (3::Int) + div y 100 ^ (3::Int) + mod (div y 10) 10 ^ (3::Int)

-- 4.30. Дано трехзначное число. Определить:
-- а) является ли сумма его цифр двузначным числом;
-- б) является ли произведение его цифр трехзначным числом;
-- в) больше ли числа а произведение его цифр;
-- г) кратна ли пяти сумма его цифр;
-- д) кратна ли сумма его цифр числу а.
isDifThree :: Integral t => t -> t -> (Bool, Bool, Bool, Bool, Bool)
isDifThree x a = (isSumTwoDig, isProdThreeDig, isProdMoreA, multipleSumFive, multipleSumA) where
    sumX  = div x 100 + mod x 10 + mod (div x 10) 10
    prodX = div x 100 * mod x 10 * mod (div x 10) 10
    isSumTwoDig     = sumX < 100 && sumX > 9
    isProdThreeDig  = prodX < 1000 && prodX > 99
    isProdMoreA     = prodX > a
    multipleSumFive = mod sumX 5 == 0
    multipleSumA    = mod sumX a == 0

-- 4.31. Дано трехзначное число.
-- а) Верно ли, что все его цифры одинаковые?
-- б) Определить, есть ли среди его цифр одинаковые.
equalAllDigits :: Show t => t -> (Bool, Bool)
equalAllDigits x = (isAllEqual, isAnyTwoEqual) where
    isAllEqual    = all (== head digit) digit
    isAnyTwoEqual = elem (head digit) digit || elem (last digit) digit
    digit = show x

-- 4.32. Дано четырехзначное число. Определить:
-- а) равна ли сумма двух первых его цифр сумме двух его последних цифр;
-- б) кратна ли трем сумма его цифр;
-- в) кратно ли четырем произведение его цифр;
-- г) кратно ли произведение его цифр числу а.
isDiffFour :: Int -> Int -> (Bool, Bool, Bool, Bool)
isDiffFour x a = (firstTwoEqLastTwo, sumMultThree, prodMultFour, prodMultA) where
    firstTwoEqLastTwo = div x 100 == mod x 100
    sumMultThree      = mod (foldr (\y s -> digitToInt y + s) 0 (show x)) 3 == 0
    prodMultFour      = mod (foldr (\y s -> digitToInt y * s) 1 (show x)) 4 == 0
    prodMultA         = mod (foldr (\y s -> digitToInt y * s) 1 (show x)) a == 0

-- 4.33. Дано натуральное число.
-- а) Верно ли, что оно заканчивается четной цифрой?
-- б) Верно ли, что оно заканчивается нечетной цифрой?
evenOdd :: Integral t => t -> (Bool, Bool)
evenOdd x = (lastEven, lastOdd) where
    lastEven = even $ mod x 10
    lastOdd  = odd $ mod x 10

-- 4.34. Определить, является ли число а делителем числа b? А наоборот? (Получить два ответа.)
multipleAndRev :: Integral t => t -> t -> (Bool, Bool)
multipleAndRev b a = (division, divisionRev) where
    division    = mod b a == 0
    divisionRev = mod a b == 0

-- 4.35.*Имеется стол прямоугольной формы с размерами a b (a и b — целые числа, a > b). В каком случае на столе можно разместить большее количество картонных прямоугольников с размерами c d (c и d — целые числа, c > d): при размещении их длинной стороной вдоль длинной стороны стола или вдоль короткой. Прямоугольники не должны лежать один на другом и не должны свисать со стола.
table :: Integral t1 => t1 -> t1 -> t1 -> t1 -> String
table a b c d = if short > long then "short" else "long" where
    short = div b d
    long  = div a c

-- 4.36. Работа светофора для пешеходов запрограммирована следующим образом: в начале каждого часа в течение трех минут горит зеленый сигнал, затем в течение двух минут — красный, в течение трех минут — опять зеленый и т. д. Дано вещественное число t, означающее время в минутах, прошедшее с начала очередного часа. Определить, сигнал какого цвета горит для пешеходов в этот момент.

greenOrRed :: RealFrac t => t -> String
greenOrRed t = if roundT < 3 then "green" else "red" where
    roundT = mod (truncate t::Int) 5
