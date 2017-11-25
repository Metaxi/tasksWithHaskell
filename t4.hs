module T4 where

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

-- 4.18.*Даны два прямоугольника, стороны которых параллельны или перпендикулярны осям координат. Известны координаты левого нижнего и правого нижнего углов каждого из них. Найти координаты левого нижнего и правого верхнего углов минимального прямоугольника, содержащего указанные прямоугольники.
