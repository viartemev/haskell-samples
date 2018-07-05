doubleUs x y = (doubleMe x) + (doubleMe y)
doubleMe x = x + x
doubleSmallNumber x = if x > 100
                        then x
                        else x*2
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x ]
length' xs = sum [1 | _ <- xs]

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]   

addThree :: Int -> Int -> Int -> Int  
addThree x y z = x + y + z  

lucky :: (Integral a) => a -> String  
lucky 7 = "LUCKY NUMBER SEVEN!"  
lucky x = "Sorry, you're out of luck, pal!"

sayMe :: (Integral a) => a -> String  
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"

factorial :: (Integral a) => a -> a  
factorial 0 = 1  
factorial n = n * factorial (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
addVectors a b = (fst a + fst b, snd a + snd b)

addVectors' :: (Num a) => (a, a) -> (a, a) -> (a, a)  
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)  

first :: (a, b, c) -> a
first (x, _, _) = x
  
second :: (a, b, c) -> b
second (_, y, _) = y
  
third :: (a, b, c) -> c
third (_, _, z) = z

head' :: [a] -> a  
head' [] = error "Can't call head on an empty list, dummy!"  
head' (x:_) = x  

tell :: (Show a) => [a] -> String  
tell [] = "The list is empty"  
tell [x] = "The list has one element: " ++ show x  
tell [x,y] = "The list has two elements: " ++ show x ++ " and " ++ show y  
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y  

length2' :: (Num b) => [a] -> b  
length2' [] = 0  
length2' (_:xs) = 1 + length2' xs  

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String  
capital "" = "Empty String, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]


quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted

bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
          skinny = 18.5  
          normal = 25.0  
          fat = 30.0  

max' :: (Ord a) => a -> a -> a
max' a b 
    | a > b = a
    | otherwise = b


myCompare :: (Ord a) => a -> a -> Ordering  
a `myCompare` b  
    | a > b     = GT  
    | a == b    = EQ  
    | otherwise = LT  

initials :: String -> String -> String  
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname    

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [ bmi | (w,h) <- xs, let bmi = w / h ^ 2]

head1' :: [a] -> a
head1' xs = case xs of [] -> error "No head for empty lists!"
                       (x:_) -> x

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."

describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
    where what [] = "empty."
          what [x] = "a singleton list."
          what xs = "a longer list."


maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
      | x > maxTail = x
      | otherwise = maxTail
      where maxTail = maximum' xs

arraySum :: (Num a) => [a] -> a
arraySum [] = error "sum of empty list"
arraySum [x] = x
arraySum (x:xs) = (x) + (arraySum xs)

replicate' :: (Num i, Ord i) => a -> i -> [a]
replicate' n x
          | x <=0 = []
          | otherwise = n:(replicate' n (x - 1))


take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
     | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x:(take' (n-1) xs)


reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys