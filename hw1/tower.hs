type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 p1 p2 p3 = []
hanoi disc p1 p2 p3 =
  hanoi (disc-1) p1 p3 p2 ++ [(p1, p2)] ++ hanoi (disc-1) p3 p2 p1
