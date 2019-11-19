data Move = Rock | Paper | Scissors
     deriving (Eq, Read, Show, Enum, Bounded)

data Outcome = Lose | Tie | Win deriving (Show, Eq, Ord)

outcome :: Move->Move->Outcome

outcome Rock Scissors        = Win
outcome Paper Rock           = Win
outcome Scissors Paper       = Win
outcome p1 p2 | p1==p2 = Tie
              | otherwise = Lose

