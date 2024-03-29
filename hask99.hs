

myLast :: [a] -> a
myLast []= error "No end for empty lists"
myLast [x] = x
myLast (x:xs) = myLast xs



myButLast :: [a] -> a
myButLast [] = error " No second end for empty lists"
myButLast [x] = error " No second last for singletons"
myButLast (x:[y]) = x
myButLast (x:xs) = myButLast xs



elementAt :: [a] -> Int -> a
elementAt [] _ = error " No kth for empty lists"
elementAt (x:_) 1 = x
elementAt (_:xs) k
      | k<1 = error "kth element doesnt exists"
      | otherwise = elementAt xs (k-1)

myLength :: [a] -> Int
myLength []=0
myLength [x]=1
myLength (_:xs)=1+myLength xs

reversed :: [a] -> [a]
reversed []=[]
reversed [x]=[x]
reversed (x:xs)= reversed xs++[x]

palindrome :: (Eq a) => [a] -> Bool
palindrome [x]= True
palindrome []= True
palindrome xs= xs==reversed(xs)

-- had to refer solution               
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem a   )   = [a]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List [])     = []


single :: (Eq a) => [a] -> [a]
single []=[]
single [x]=[x]
single (x:y:[]) = x:y:[]
single (x:y:ys)

    |  x==y = single (y:ys)

    |  otherwise = x:single (y:ys)  



