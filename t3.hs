module T3 where

-- 3.1. Вычислить значение логического выражения при следующих значениях логических величин А, В и С: А = Истина, В = Ложь, С = Ложь:
--  а) А или В;
--  б) А и В;
--  в) В или С.

bool31 :: (Bool, Bool, Bool)
bool31 = (a || b, a && b, b || c) where
    a = True
    b = False
    c = False

-- 3.2. Вычислить значение логического выражения при следующих значениях логических величин X, Y и Z: X = Ложь, Y = Истина, Z = Ложь:
 -- а) X или Z;
 -- б) X и Y;
 -- в) X и Z.

bool32 :: (Bool, Bool, Bool)
bool32 = (x || y, x && y, x && z) where
    x = False
    y = True
    z = False

-- 3.3. Вычислить значение логического выражения при следующих значениях логических величин А, В и С: А = Истина, В = Ложь, С = Ложь:
 -- а) не А и В;
 -- б) А или не В;
 -- в) А и В или С.


bool33 :: (Bool, Bool, Bool)
bool33 = (not a1 && b1, a1 && not b1, a1 && b1 || c1) where
     a1 = True
     b1 = False
     c1 = False

-- 3.4.Вычислить значение логического выражения при следующих значениях логических величин X, Y и Z: X = Истина, Y = Истина, Z = Ложь:
 -- а) не X и Y;
 -- б) X или не Y;
 -- в) X или Y и Z.

bool34 :: (Bool, Bool, Bool)
bool34 = (not x1 && y1, x1 || not y1, x1 || y1 && z1) where
     x1 = True
     y1 = True
     z1 = False

-- 3.5.Вычислить значение логического выражения при следующих значениях логических величин А, В и С: А = Истина, В = Ложь, С = Ложь:
-- а) А или В и не С;
--  г) А и не В или С;
-- б) не А и не В;
--  д) А и (не В или С);
-- в) не (А и С) или В;
--  е) А или (не (В и С)).


bool35 :: (Bool, Bool, Bool, Bool, Bool, Bool)
bool35 = (a2 || b2 && not c2, a2 && not b2 || c2, not a2 && not b2, a2 && (not b2 || c2), not (a2 && c2) || b2, a2 || not (b2 && c2)) where
     a2 = True
     b2 = False
     c2 = False

-- 3.6.Вычислить значение логического выражения при следующих значениях логических величин X, Y и Z: X = Ложь, Y = Ложь, Z = Истина:
-- а) X или Y и не Z;
--  г) X и не Y или Z;
-- б) не X и не Y;
--  д) X и (не Y или Z);
-- в) не (X и Z) или Y;
--  е) X или (не (Y или Z)).

bool36 :: (Bool, Bool, Bool, Bool, Bool, Bool)
bool36 = (x2 || y2 && not z2, x2 && not y2 || z2, not x2 && not y2, x2 && (not y2 || z2), not (x2 && z2) || x2, x2 || not (y2 && z2)) where
     x2 = False
     y2 = False
     z2 = True

-- 3.7.Вычислить значение логического выражения при следующих значениях логических величин А, В и С: А = Истина, В = Ложь, С = Ложь:
-- а) А или не (А и В) или С;
-- б) не А или А и (В или С);
-- в) (А или В и не С) и С.

bool37 :: (Bool, Bool, Bool)
bool37 = (a3 || not (a3 && b3) || c3, not a3 || a3 && (b3 || c3), (a3 || b3 && not c3) && c3) where
    a3 = True
    b3 = False
    c3 = False

-- 3.8.Вычислить значение логического выражения при следующих значениях логических величин X, Y и Z: X = Ложь, Y = Истина, Z = Ложь:
-- а) X и не (Z или Y) или не Z;
-- б) не X или X и (Y или Z);
-- в) (X или Y и не Z) и Z.

bool38 :: (Bool, Bool, Bool)
bool38 = (x3 && not (z3 || y3) || not z3, not x3 || x3 && (y3 || z3), (x3 || y3 && not z3) && z3) where
    x3 = False
    y3 = True
    z3 = False

-- 3.9.Вычислить значение логического выражения при следующих значениях логических величин X, Y и Z: X = Истина, Y = Ложь, Z = Ложь:
-- а) не X или не Y или не Z;
-- б) (не X или не Y) и (X или Y);
-- в) X и Y или X и Z или не Z.
bool39 :: (Bool, Bool, Bool)
bool39 = (not x3 || not y3 || not z3, (not x3 || not y3) && (x3 || y3), x3 && y3 || x3 && z3 || not z3) where
    x3 = True
    y3 = False
    z3 = False

-- 3.10. Вычислить значение логического выражения при следующих значениях логических величин А, В и С: А = Ложь, В = Ложь, С = Истина:
-- а) (не А или не В) и не С;
-- б) (не А или не В) и (А или В);
-- в) А и В или А и С или не С.
bool310 :: (Bool, Bool, Bool)
bool310 = ((not a || not b) && not c, (not a || not b) && (a || b), a && b || a && c || not c) where
    a = False
    b = False
    c = True

-- 3.11. Вычислить значение логического выражения:

expression311a :: Bool
expression311a = x ^ (2::Int) + y ^ (2::Int) <= (4::Int) where
    x = 1
    y = -1

expression311b :: Bool
expression311b = x > (0::Int) || y ^ (2::Int) /= (4::Int) where
    x = 1
    y = 2

expression311c :: Bool
expression311c = x >= (0::Int) && y ^ (2 :: Int) /= (4::Int) where
    x = 1
    y = 2

expression311d :: Bool
expression311d = x * y /= (0::Int) && y > x where
    x = 2
    y = 1

expression311e :: Bool
expression311e = x * y >= (0::Int) && y > x where
    x = 2
    y = 1

expression311j :: Bool
expression311j = x * y >= (0::Int) || y > x where
    x = 1
    y = 2

-- 3.12. Вычислить значение логического выражения:
expression312a :: Bool
expression312a = x ^ (2::Int) - y ^ (2::Int) <= (0::Int) where
    x = 1
    y = -1

expression312b :: Bool
expression312b = x >= 2 || y ^ (2::Int) /= (4 :: Int) where
    x = 2 :: Int
    y = - 2

expression312c :: Bool
expression312c = x >= (0::Int) && y ^ (2::Int) > (4::Int) where
    x = 2
    y = 2

expression312d :: Bool
expression312d = x * y /= (4::Int) && y > x where
    x = 1
    y = 2

expression312e :: Bool
expression312e = x * y /= (0::Int) && y < x where
    x = 2
    y = 1

expression312f :: Bool
expression312f = x * y >= (1::Int) && y > x where
    x = 1
    y = 2

expression312j :: Bool
expression312j = x * y >= (0::Int) && y > x where
        x = 2
        y = 1

-- 3.13. Вычислить значение логического выражения при всех возможных значениях логических величин А и В:
-- а) не (А и В);
-- б) не А или В;
-- в) А или не В.

boolList313a :: [Bool]
boolList313a = [not (a && b) | a <- [False, True], b <- [False,True]]

boolList313b :: [Bool]
boolList313b = [not a || b | a <- [False, True], b <- [False,True]]

boolList313c :: [Bool]
boolList313c = [a || not b | a <- [False, True], b <- [False,True]]

-- 3.14. Вычислить значение логического выражения при всех возможных значениях логических величин X и Y:
-- а) не (X или Y);
-- б) не X и Y;
-- в) X и не Y.
boolList314a :: [Bool]
boolList314a = [not (x && y) | x <- [False, True], y <- [False,True]]

boolList314b :: [Bool]
boolList314b = [not x && y | x <- [False, True], y <- [False,True]]

boolList314c :: [Bool]
boolList314c = [x && not y | x <- [False, True], y <- [False,True]]

-- 3.15. Вычислить значение логического выражения при всех возможных значениях логических величин А и В:
-- а) не А или не В;
-- б) А и (А или не В);
-- в) (не А или В) и В.
boolList315a :: [Bool]
boolList315a = [not a || not b | a <- [False, True], b <- [False,True]]

boolList315b :: [Bool]
boolList315b = [a && (a || not b) | a <- [False, True], b <- [False,True]]

boolList315c :: [Bool]
boolList315c = [(not a || b) && b | a <- [False, True], b <- [False,True]]

-- 3.16. Вычислить значение логического выражения при всех возможных значениях логических величин X и Y:
-- а) не X и не Y;
-- б) X или (не X и Y);
-- в) (не X и Y) или Y.
boolList316a :: [Bool]
boolList316a = [not x && not y | x <- [False, True], y <- [False,True]]

boolList316b :: [Bool]
boolList316b = [x || (not x && y) | x <- [False, True], y <- [False,True]]

boolList316c :: [Bool]
boolList316c = [(not x && y) || y | x <- [False, True], y <- [False,True]]

-- 3.17. Вычислить значение логического выражения при всех возможных значениях логических величин А и В:
-- а) не А и не В или А;
-- б) В или не А и не В;
-- в) В и не (А и не В).
boolList317a :: [Bool]
boolList317a = [not a && not b || a | a <- [False, True], b <- [False,True]]

boolList317b :: [Bool]
boolList317b = [b || not a && not b | a <- [False, True], b <- [False,True]]

boolList317c :: [Bool]
boolList317c = [b && not (a && not b) | a <- [False, True], b <- [False,True]]

-- 3.18. Вычислить значение логического выражения при всех возможных значениях логических величин X и Y:
-- а) не (X и не Y) или X;
-- б) Y и не X или не Y;
-- в) не Y и не X или Y.

boolList318a :: [Bool]
boolList318a = [not (x && not y) || x | x <- [False, True], y <- [False,True]]

boolList318b :: [Bool]
boolList318b = [y && not x || not y | x <- [False, True], y <- [False,True]]

boolList318c :: [Bool]
boolList318c = [not y && not x || y | x <- [False, True], y <- [False,True]]

-- -- 3.19. Вычислить значение логического выражения при всех возможных значениях логических величин А и В:
-- а) не (не А и не В) и А;
-- б) не (не А или не В) или А;
-- в) не (не А или не В) и В.

boolList319a :: [Bool]
boolList319a = [not (not a && not b) && a | a <- [False, True], b <- [False,True]]

boolList319b :: [Bool]
boolList319b = [not (not a || not b) || a | a <- [False, True], b <- [False,True]]

boolList319c :: [Bool]
boolList319c = [not (not a || not  b) && b | a <- [False, True], b <- [False,True]]

-- -- 3.20. Вычислить значение логического выражения при всех возможных значениях логических величин X и Y:
-- а) не (не X или Y) или не X;
-- б) не (не X и не Y) и X;
-- в) не (X или не Y) или не Y.
boolList320a :: [Bool]
boolList320a = [not (not x || y) || not x | x <- [False, True], y <- [False,True]]

boolList320b :: [Bool]
boolList320b = [not (not x && not y) && x | x <- [False, True], y <- [False,True]]

boolList320c :: [Bool]
boolList320c = [not (x || not y) || not y | x <- [False, True], y <- [False,True]]

-- 3.21. Вычислить значение логического выражения при всех возможных значениях логических величин А, В и С:
-- а) не (А или не В и С);
-- б) А и не (В и или не С);
-- в) не (не А или В и С).
boolList321a :: [Bool]
boolList321a = [not (a || not b && c) | a <- [False, True], b <- [False,True], c <- [False,True]]

boolList321b :: [Bool]
boolList321b = [a && not (b && a || not c) | a <- [False, True], b <- [False,True], c <- [False,True]]

boolList321c :: [Bool]
boolList321c = [not (not a || b && c) | a <- [False, True], b <- [False,True], c <- [False,True]]

-- 3.22. Вычислить значение логического выражения при всех возможных значениях логических величин X, Y и Z:
-- а) не (X или не Y и Z);
-- б) Y или (X и не Y или Z);
-- в) не (не X и Y или Z).
boolList322a :: [Bool]
boolList322a = [not (x || not y && z) | x <- [False, True], y <- [False,True], z <- [False,True]]

boolList322b :: [Bool]
boolList322b = [y || (x && not y || z) | x <- [False, True], y <- [False,True], z <- [False,True]]

boolList322c :: [Bool]
boolList322c = [not (not x && y || z) | x <- [False, True], y <- [False,True], z <- [False,True]]

-- 3.23. Вычислить значение логического  выражения при всех возможных значениях логических величин А, В и С:
-- а) не (А или не В и С) или С;
-- б) не (А и не В или С) и В;
-- в) не (не А или В и С) или А.
boolList323a :: [Bool]
boolList323a = [not (a || not b && c) || c | a <- [False, True], b <- [False,True], c <- [False,True]]

boolList323b :: [Bool]
boolList323b = [not (a && not b || c) && b | a <- [False, True], b <- [False,True], c <- [False,True]]

boolList323c :: [Bool]
boolList323c = [not (not a || b && c) || a | a <- [False, True], b <- [False,True], c <- [False,True]]

-- 3.24. Вычислить значение логического выражения при всех возможных значениях логических величин X, Y и Z:
-- а) не (Y или не X и Z) или Z;
-- б) X и не (не Y или Z) или Y;
-- в) не (X или Y и Z) или не X.
boolList324a :: [Bool]
boolList324a = [not (y || not x && z) || z | x <- [False, True], y <- [False,True], z <- [False,True]]

boolList324b :: [Bool]
boolList324b = [x && not (not y || z) || y | x <- [False, True], y <- [False,True], z <- [False,True]]

boolList324c :: [Bool]
boolList324c = [not (x || y && z) || not x | x <- [False, True], y <- [False,True], z <- [False,True]]

-- 3.25. Вычислить значение логического выражения при всех возможных значениях логических величин А, В и С:
-- а) не (А и В) и (не А или не С);
-- б) не (А и не В) или (А или не С);
-- в) А и не В или не (А или не С).
boolList325a :: [Bool]
boolList325a = [not (a && b) && (not a || not c) | a <- [False, True], b <- [False,True], c <- [False,True]]

boolList325b :: [Bool]
boolList325b = [not (a && not b) || (a || not c) | a <- [False, True], b <- [False,True], c <- [False,True]]

boolList325c :: [Bool]
boolList325c = [a && not b || not (a || not c) | a <- [False, True], b <- [False,True], c <- [False,True]]

-- 3.26. Вычислить значение логического выражения при всех возможных значениях логических величин X, Y и Z:
-- а) не (X или Y) и (не X или не Z);
-- б) не (не X и Y) или (X и не Z);
-- в) X или не Y и не (X или не Z).
boolList326a :: [Bool]
boolList326a = [not (x || y) && (not x || not z) | x <- [False, True], y <- [False,True], z <- [False,True]]

boolList326b :: [Bool]
boolList326b = [not (not x && y) || (x && not z) | x <- [False, True], y <- [False,True], z <- [False,True]]

boolList326c :: [Bool]
boolList326c = [x || not y && not (x || not z) | x <- [False, True], y <- [False,True], z <- [False,True]]

-- 3.27. Записать логические выражения, которые имеют значение "Истина" только при выполнении указанных условий:

isTrue327a :: Int -> Int -> Bool
isTrue327a x y = x > 2 && y > 3

isTrue327b :: (Num a1, Num a, Ord a1, Ord a) => a -> a1 -> Bool
isTrue327b x y = x > 1 || y < (-2)

isTrue327c :: (Num a1, Num a, Ord a1, Ord a) => a -> a1 -> Bool
isTrue327c x y = x >= 0 && y < 5

isTrue327d :: (Num a, Ord a) => a -> Bool
isTrue327d x = x > 3 || x < (-1)

isTrue327e :: (Num a, Ord a) => a -> Bool
isTrue327e x = x > 3 && x < 10

isTrue327f :: (Num a, Ord a) => a -> Bool
isTrue327f x = x <= 2

isTrue327j :: (Num a, Ord a) => a -> Bool
isTrue327j x = not (x > 0 && x < 5)

isTrue327i :: (Num a1, Num a, Ord a1, Ord a) => a -> a1 -> Bool
isTrue327i x y = x < 5 && 0 < y && y <= 4

-- 3.28. Записать условие, которое является истинным, когда:
-- а) каждое из чисел А и В больше 100;
-- б) только одно из чисел А и В четное;
-- в) хотя бы одно из чисел А и В положительно;
-- г) каждое из чисел А, В, С кратно трем;
-- д) только одно из чисел А, В и С меньше 50;
-- е) хотя бы одно из чисел А, В, С отрицательно.

isTrue328a :: (Num a1, Num a, Ord a1, Ord a) => a -> a1 -> Bool
isTrue328a a b = a > 100 && b > 100

isTrue328b :: (Integral a1, Integral a) => a -> a1 -> Bool
isTrue328b a b = even a /= even b

isTrue328c :: (Num a1, Num a, Ord a1, Ord a) => a -> a1 -> Bool
isTrue328c a b = (a > 0) || (b > 0)

isTrue328d :: (Integral a2, Integral a1, Integral a) => a -> a1 -> a2 -> Bool
isTrue328d a b c = mod a 3 == 0 && mod b 3 == 0 && mod c 3 == 0

isTrue328e :: (Num a2, Num a1, Num a, Ord a2, Ord a1, Ord a) => a -> a1 -> a2 -> Bool
isTrue328e a b c = (a < 50) /= ((b < 50) /= (c < 50))

isTrue328f :: (Num a2, Num a1, Num a, Ord a2, Ord a1, Ord a) => a -> a1 -> a2 -> Bool
isTrue328f a b c = (a < 0) || (b < 0) || (c < 0)

-- 3.29. Записать условие, которое является истинным, когда:
-- а) каждое из чисел X и Y нечетное;
-- б) только одно из чисел X и Y меньше 20;
-- в) хотя бы одно из чисел X и Y равно нулю;
-- г) каждое из чисел X, Y, Z отрицательное;
-- д) только одно из чисел X, Y и Z кратно пяти;
-- е) хотя бы одно из чисел X, Y, Z больше 100.
isTrue329a :: (Integral a1, Integral a) => a -> a1 -> Bool
isTrue329a x y = odd x && odd y

isTrue329b :: (Num a1, Num a, Ord a1, Ord a) => a -> a1 -> Bool
isTrue329b x y = (x < 20) /= (y < 20)

isTrue329c :: (Num a1, Num a, Eq a1, Eq a) => a -> a1 -> Bool
isTrue329c x y = x == 0 || y == 0

isTrue329d :: (Num a2, Num a1, Num a, Ord a2, Ord a1, Ord a) => a -> a1 -> a2 -> Bool
isTrue329d x y z = x < 0 && y < 0 && z < 0

isTrue329e :: (Integral a2, Integral a1, Integral a) => a -> a1 -> a2 -> Bool
isTrue329e x y z = (mod x 5 == 0) /= ((mod y 5 == 0) /= (mod z 5 == 0))

isTrue329f :: (Num a2, Num a1, Num a, Ord a2, Ord a1, Ord a) => a -> a1 -> a2 -> Bool
isTrue329f x y z = x > 100 || y > 100 || z > 100

-- 3.30. Записать условие, которое является истинным, когда:
-- а) целое А кратно двум или трем;
-- б) целое А не кратно трем и оканчивается нулем.
isTrue330a :: Integral a => a -> Bool
isTrue330a a = mod a 2 == 0 || mod a 3 == 0

isTrue330b :: Integral a => a -> Bool
isTrue330b a = mod a 3 /= 0 && x == fromInteger (round (x :: Double)) where
    x = fromIntegral a / 10

-- 3.31. Записать условие, которое является истинным, когда:
-- а) целое N кратно пяти или семи;
-- б) целое N кратно четырем и не оканчивается нулем.

isTrue331a :: Integral a => a -> Bool
isTrue331a a = mod a 5 == 0 || mod a 7 == 0

isTrue331b :: Integral a => a -> Bool
isTrue331b a = mod a 4 /= 0 && x /= fromInteger (round (x :: Double)) where
    x = fromIntegral a / 10

-- 3.32. Записать условие, которое является истинным, когда точка с координатами (х, у) попадает в заштрихованные участки плоскости, включая их границы (рис. 3.1).

isTrue332a :: (Num a1, Num a, Ord a1, Ord a) => (a, a1) -> Bool
isTrue332a (x,y) = x < -2 && y > 1

isTrue332b :: (Fractional a, Ord a) => (t, a) -> Bool
isTrue332b (_,y) = y > -2 && y < 1.5

isTrue332c :: (Num a1, Num a, Ord a1, Ord a) => (a, a1) -> Bool
isTrue332c (x,y) = x > 1 && x < 2 && y < 4

isTrue332d :: (Num a1, Num a, Ord a1, Ord a) => (a, a1) -> Bool
isTrue332d (x,y) = y > 2 && y < 4 && x > 1

isTrue332e :: (Num a1, Num a, Ord a1, Ord a) => (a, a1) -> Bool
isTrue332e (x,y) = (x > 1 && y < -1) || (y > 0 && x > 2)

isTrue332f :: (Fractional a1, Num a, Ord a1, Ord a) => (a, a1) -> Bool
isTrue332f (x,y) = (x > 2 && y < -1.5) || (y > 1 && x > 2)

isTrue332j :: (Fractional a1, Num a, Ord a1, Ord a) => (a, a1) -> Bool
isTrue332j (x,y) = x > 1 && x < 3 && y > -2 && y < -1

isTrue332i :: (Fractional a1, Num a, Ord a1, Ord a) => (a, a1) -> Bool
isTrue332i (x,y) = x >= 2 || x < 2 && y < 1.5 && y > 0.5

-- 3.33. Записать условие, которое является истинным, когда точка с координатами (х, у) попадает в заштрихованные участки плоскости, включая их границы (рис. 3.2).
isTrue333a :: (Num a1, Num a, Ord a1, Ord a) => (a, a1) -> Bool
isTrue333a (x,y) = x < -1 && y < -2

isTrue333b :: (Num a, Ord a) => (t, a) -> Bool
isTrue333b (_,y) = y < -3 && y > 1

isTrue333c :: (Num a, Ord a) => (t, a) -> Bool
isTrue333c (_,y) = y > -4 && y < -3 && y > 1

isTrue333d :: (Fractional a1, Fractional a, Ord a1, Ord a) => (a, a1) -> Bool
isTrue333d (x,y) = x > -1 && x < -1.5 && y > -0.5 && y < 1.5

isTrue333e :: (Num a1, Num a, Ord a1, Ord a) => (a, a1) -> Bool
isTrue333e (x,y) = x > 1 && x < 4 && y > 2 && y < 4

isTrue333f :: (Num a1, Num a, Ord a1, Ord a) => (a, a1) -> Bool
isTrue333f (x,y) = (x < -1 && y > 1) || (y > 1 && x > 2)

isTrue333j :: (Num a1, Num a, Ord a1, Ord a) => (a, a1) -> Bool
isTrue333j (x,y) = x > 1 && x < 3 && y > -3 && y < 1

isTrue333i :: (Fractional a1, Fractional a, Ord a1, Ord a) => (a, a1) -> Bool
isTrue333i (x,y) = (x > 1 && x < 2.5 && y > -2 && y < 1.5) || y >= 1.5

-- 3.34. Поле шахматной доски определяется парой натуральных чисел, каждое из которых не превосходит 8: первое число — номер горизонтали (при счете слева направо), второе — номер вертикали (при счете снизу вверх). Даны натуральные числа a, b, c, d, каждое из которых не превосходит 8.
-- а) На поле (a, b) расположена ладья. Записать условие, при котором она угро-
-- жает полю (c, d).

chess334a :: (Num a1, Num a, Eq a1, Eq a) => (a, a1) -> (a, a1) -> Bool
chess334a (a,b) (c,d) = (c - a == 0 && d - b /= 0) || (c - a /= 0 && d - b == 0)

-- б) На поле (a, b) расположен слон. Записать условие, при котором он угрожает
-- полю (c, d).

chess334b :: (Num a, Eq a) => (a, a) -> (a, a) -> Bool
chess334b (a,b) (c,d) = abs (c - a) == abs (d - b)

-- в) На поле (a, b) расположен король. Записать условие, при котором он может
-- одним ходом попасть на поле (c, d).

chess334c :: (Num a1, Num a, Eq a1, Eq a) => (a, a1) -> (a, a1) -> Bool
chess334c (a,b) (c,d) = (abs (a - c) == 1 && abs (d - b) == 0) || (abs (a - c) == 0 && abs (d - b) == 1) || (abs (a - c) == 1 && abs (d - b) == 1)

-- г) На поле (a, b) расположен ферзь. Записать условие, при котором он угрожа-
-- ет полю (c, d).

chess334d :: (Num a, Eq a) => (a, a) -> (a, a) -> Bool
chess334d (a,b) (c,d) = (abs (a - c) == abs (d - b)) || (c - a == 0 && d - b /= 0) || (c - a /= 0 && d - b == 0)

-- д) На поле (a, b) расположена белая пешка. Записать условие, при котором
-- она может одним ходом попасть на поле (c, d):
-- при обычном ходе;
-- когда она "бьет" фигуру или пешку соперника.
-- Примечание: Белые пешки перемещаются на доске снизу вверх.

chess334e :: (Num a1, Num a, Eq a1, Eq a) => (a, a1) -> (a, a1) -> Bool
chess334e (a,b) (c,d) | b == 2 = (a - c == 0 && (d - b == 1 || d - b == 2)) || (abs (c - a) == 1 && d - b == 1)
                      | otherwise = (a - c == 0 && d - b == 1) || (abs (c - a) == 1 && d - b == 1)

-- е) На поле (a, b) расположена черная пешка. Записать условие, при котором
-- она может одним ходом попасть на поле (c, d):
-- при обычном ходе;
-- когда она "бьет" фигуру или пешку соперника.
-- Примечание: Черные пешки перемещаются на доске сверху вниз.

chess334f :: (Num a1, Num a, Eq a1, Eq a) => (a, a1) -> (a, a1) -> Bool
chess334f (a,b) (c,d) | b == 7 = (a - c == 0 && (d - b == -1 || d - b == -2)) || (abs (c - a) == 1 && d - b == -1)
                      | otherwise = (a - c == 0 && d - b == -1) || (abs (c - a) == 1 && d - b == -1)

-- ж) На поле (a, b) расположен конь. Записать условие, при котором он угрожа-
-- ет полю (c, d).
chess334j :: (Num a1, Num a, Eq a1, Eq a) => (a, a1) -> (a, a1) -> Bool
chess334j (a,b) (c,d) = (abs (c - a) == 1 && abs (d - b) == 2) || (abs (c - a) == 2 && abs (d - b) == 1)

-- 3.35. Поле шахматной доски определяется парой натуральных чисел, каждое из которых не превосходит 8: первое число — номер горизонтали (при счете слева направо), второе — номер вертикали (при счете снизу вверх). Даны натуральные числа a, b, c, d, e, f, каждое из которых не превосходит 8. Записать условие, при котором белая фигура, расположенная на поле (a, b), может одним ходом пойти на поле (e, f), не попав при этом под удар черной фигуры,находящейся на поле (c, d). Рассмотреть следующие варианты сочетаний белой и черной фигур:
-- ладья и ладья
chessNoAttack335a :: (Eq a, Eq a1, Num a, Num a1) => (a, a1) -> (a, a1) -> (a, a1) -> Bool
chessNoAttack335a (a,b) (c,d) (e,f) = chess334a (a,b) (e,f) && not (chess334a (c,d) (e,f))
-- ладья и ферзь
chessNoAttack335b :: (Eq a, Num a) => (a, a) -> (a, a) -> (a, a) -> Bool
chessNoAttack335b (a,b) (c,d) (e,f) = chess334a (a,b) (e,f) && not (chess334d (c,d) (e,f))
-- ладья и конь
chessNoAttack335c :: (Eq a, Num a) => (a, a) -> (a, a) -> (a, a) -> Bool
chessNoAttack335c (a,b) (c,d) (e,f) = chess334a (a,b) (e,f) && not (chess334j (c,d) (e,f))
-- лвдья и слон
chessNoAttack335d :: (Eq a, Num a) => (a, a) -> (a, a) -> (a, a) -> Bool
chessNoAttack335d (a,b) (c,d) (e,f) = chess334a (a,b) (e,f) && not (chess334b (c,d) (e,f))
-- ферзь и ферзь
chessNoAttack335e :: (Eq a, Num a) => (a, a) -> (a, a) -> (a, a) -> Bool
chessNoAttack335e (a,b) (c,d) (e,f) = chess334d (a,b) (e,f) && not (chess334d (c,d) (e,f))
-- ферзь и ладья
chessNoAttack335f :: (Eq a, Num a) => (a, a) -> (a, a) -> (a, a) -> Bool
chessNoAttack335f (a,b) (c,d) (e,f) = chess334d (a,b) (e,f) && not (chess334a (c,d) (e,f))
-- ферзь и конь
chessNoAttack335g :: (Eq a, Num a) => (a, a) -> (a, a) -> (a, a) -> Bool
chessNoAttack335g (a,b) (c,d) (e,f) = chess334d (a,b) (e,f) && not (chess334j (c,d) (e,f))
-- ферзь и слон
chessNoAttack335h :: (Eq a, Num a) => (a, a) -> (a, a) -> (a, a) -> Bool
chessNoAttack335h (a,b) (c,d) (e,f) = chess334d (a,b) (e,f) && not (chess334b (c,d) (e,f))
-- конь и конь
chessNoAttack335i :: (Eq a, Num a) => (a, a) -> (a, a) -> (a, a) -> Bool
chessNoAttack335i (a,b) (c,d) (e,f) = chess334j (a,b) (e,f) && not (chess334j (c,d) (e,f))
-- конь и ладья
chessNoAttack335j :: (Eq a, Num a) => (a, a) -> (a, a) -> (a, a) -> Bool
chessNoAttack335j (a,b) (c,d) (e,f) = chess334j (a,b) (e,f) && not (chess334a (c,d) (e,f))
-- конь и ферзь
chessNoAttack335k :: (Eq a, Num a) => (a, a) -> (a, a) -> (a, a) -> Bool
chessNoAttack335k (a,b) (c,d) (e,f) = chess334j (a,b) (e,f) && not (chess334d (c,d) (e,f))
-- конь и слон
chessNoAttack335l :: (Eq a, Num a) => (a, a) -> (a, a) -> (a, a) -> Bool
chessNoAttack335l (a,b) (c,d) (e,f) = chess334j (a,b) (e,f) && not (chess334b (c,d) (e,f))
-- слон и слон
chessNoAttack335m :: (Eq a, Num a) => (a, a) -> (a, a) -> (a, a) -> Bool
chessNoAttack335m (a,b) (c,d) (e,f) = chess334b (a,b) (e,f) && not (chess334b (c,d) (e,f))
-- слон и ферзь
chessNoAttack335n :: (Eq a, Num a) => (a, a) -> (a, a) -> (a, a) -> Bool
chessNoAttack335n (a,b) (c,d) (e,f) = chess334b (a,b) (e,f) && not (chess334d (c,d) (e,f))
-- слон и ладья
chessNoAttack335o :: (Eq a, Num a) => (a, a) -> (a, a) -> (a, a) -> Bool
chessNoAttack335o (a,b) (c,d) (e,f) = chess334b (a,b) (e,f) && not (chess334a (c,d) (e,f))
-- слон и конь
chessNoAttack335p :: (Eq a, Num a) => (a, a) -> (a, a) -> (a, a) -> Bool
chessNoAttack335p (a,b) (c,d) (e,f) = chess334b (a,b) (e,f) && not (chess334j (c,d) (e,f))
-- король и ладья
chessNoAttack335q :: (Eq a, Num a) => (a, a) -> (a, a) -> (a, a) -> Bool
chessNoAttack335q (a,b) (c,d) (e,f) = chess334c (a,b) (e,f) && not (chess334a (c,d) (e,f))
-- король и ферзь
chessNoAttack335r :: (Eq a, Num a) => (a, a) -> (a, a) -> (a, a) -> Bool
chessNoAttack335r (a,b) (c,d) (e,f) = chess334c (a,b) (e,f) && not (chess334d (c,d) (e,f))
-- король и конь
chessNoAttack335s :: (Eq a, Num a) => (a, a) -> (a, a) -> (a, a) -> Bool
chessNoAttack335s (a,b) (c,d) (e,f) = chess334c (a,b) (e,f) && not (chess334j (c,d) (e,f))
-- король и слон
chessNoAttack335t :: (Eq a, Num a) => (a, a) -> (a, a) -> (a, a) -> Bool
chessNoAttack335t (a,b) (c,d) (e,f) = chess334c (a,b) (e,f) && not (chess334b (c,d) (e,f))
