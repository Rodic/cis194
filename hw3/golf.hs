module Golf where

skips :: [a] -> [[a]]
skips xs = map (everyNth xs) [1..length xs]

everyNth :: [a] -> Int -> [a]
everyNth xs = reverse . (everyNthReverse xs)

everyNthReverse :: [a] -> Int -> [a]
everyNthReverse xs n = foldl insert [] (zip xs [1..])
                      where
                        insert acc (x, i)
                          | i `mod` n == 0 = x : acc
                          | True           = acc

-----------------------

tailSafe :: [a] -> [a]
tailSafe [] = []
tailSafe (x : xs) = xs

localMaxima :: [Integer] -> [Integer]
localMaxima xs = [ x | (x, y, z) <- zip3 xs' ys' zs', x > y && x > z ]
                where
                  xs' = tailSafe xs
                  ys' = tailSafe (tailSafe xs)
                  zs' = xs

----------------------

count :: [Int] -> [Int]
count xs = map (\n -> length (filter (\i -> i == n) xs)) [1..9]

stringify :: [Int] -> Int -> [String]
stringify _ 0 = ["=========", "123456789"]
stringify counts maxCount = stars : (stringify newCounts (maxCount - 1))
                            where
                              countToStar n
                                | n == maxCount = ('*', n - 1)
                                | True          = (' ', n)
                              (stars, newCounts) = unzip (map countToStar counts)

histogram :: [Int] -> String
histogram xs = unlines (stringify counts maxCount)
               where
                 counts   = count xs
                 maxCount = maximum counts
