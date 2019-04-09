-- QUESTION 1
--list_of_lists_len :: [a] -> [[a]]

skips :: [a] -> [[a]]

slice :: [a] -> Int -> [a]
slice lst n = [x | (i, x) <- (zip [0..] lst), ((i+1) `mod` n)==0 ]

skips list = [slice list i | i <- [1..length list]]
  --if list == []

  --  then []
  --else if step mod n == 0
  --  then [list!!0] ++ [(skips (tail list) (step+1) n)]
  --else
  --  [skips((tail list) (step+1) n)]


{-skips :: [a] -> [[a]]

skips list step n=
  if list == []
    then []
  else if step mod n == 0
    then list!!0:(skips (tail list) step+1 n)
  else
    skips(tail list step+1 n)-}



-- QUESTION 2


listnumber :: [Integer] -> Integer
listnumber [] = 0
listnumber (x:xs) = 1 + listnumber xs

isMaxima :: [Integer] -> [Integer]

isMaxima listname = 
  if listname==[] || listnumber(listname) <= 2
    then []
  else if listname!!0 < listname!!1 && listname !!1 > listname!!2
    then listname!!1:isMaxima(tail listname)
  else
    isMaxima (tail listname)



-- QUESTION 3

listsize :: [Integer] -> Integer
listsize [] = 0
listsize list = 1 + listsize (tail list)

countnumber_helper :: [Integer] -> Integer -> Integer
countnumber_helper list target = 
  if list == []
    then 0
  else if list!!0 == target
    then 1+ countnumber_helper (tail list) target
  else
    0 + countnumber_helper (tail list) target

countnumber :: [Integer]->Integer -> [Integer]
countnumber list n =
  if list /= [] && n <= 9
    then (countnumber_helper list n):(countnumber list (n+1))
  else
    []

create_display :: Int->[Int] -> String
create_display n = map (("* "!!).signum.abs.( subtract n))
--create_display list n = [if x==n then '*' else ' ' | x<-list]

display :: [Int] -> Int -> String
display list n =
  if n < 0
    then " "
  else
    (create_display n list) ++ "\n" ++ (display  list (n-1))
 
main :: IO()

histogram :: [Integer] -> String

histogram list = 

--main = display [1,4,4,3,6] 6

--main = print display [1,2,3,1,2]
main = print (display [1,4,4,3,6] )
