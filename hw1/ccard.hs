
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0    = []
  | otherwise = n `rem` 10 : toDigitsRev (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs =
  [ x*y | (x,y) <- zip xs ys]
  where
    ys = case (length xs) `mod` 2 of
      0 -> cycle [2, 1]
      _ -> cycle [1, 2]

sumDigits :: [Integer] -> Integer
sumDigits xs = foldr ((+) . sum . toDigitsRev)  0  xs

validate :: Integer -> Bool
validate n = ((sumDigits . doubleEveryOther . toDigits) n `rem` 10) == 0
