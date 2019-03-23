data Shape = Circle Float Float Float | Rectangle Float Float Float Float

surface :: Shape -> Float

surface (Circle _ _ r)= pi*r^2
surface (Rectangle x1 y1 x2 y2)= (abs $ x2-x1) * (abs $ y2-y1)

data Point1 = Point Float Float deriving (Show)
--nothing special about having the data type name and the value constructor to the same, jus best practice

data Shape1 = Circle1 Point1 Float | Rectangle1 Point1 Point1 deriving (Show)
surface1 (Circle1 _ r) = pi*r^2

data Person = Person String String Int Float String String deriving (Show)

-- Assignment questions here ---------------#######################

data Op = Add | Mul deriving (Show)
data Expr = Val Int | App Op Expr Expr deriving (Show)


eval :: Expr -> Int
--eval (Val x)= x
preEval expr = expr
eval (Val x) = x
eval (App Add (Val exp1) (Val ex2))= (preEval exp1) + (preEval  ex2)
				  
eval (App Mul (Val exp1) (Val ex2)) = (preEval exp1) * (preEval  ex2)
eval (App Add exp1 exp2) = (eval exp1) + (eval exp2) 
eval (App Mul exp1 exp2) = (eval exp1) * (eval exp2)



values :: Expr -> [Int]
values (Val x) = [x]
values (App Add (Val exp1) (Val exp2)) =exp1:exp2:[]
values (App Mul (Val exp1) (Val exp2)) = exp1:exp2:[] 
values (App Add exp1 exp2) =(values exp1) ++ (values exp2)
values (App Mul exp1 exp2) = (values exp1) ++ (values exp2)

values _ = error "wrong input my friend"

-- question2 here #################################################


delete' :: Int -> [Int] -> [Int]
delete' _ [] = []
delete' y (x:xs) | y==x = xs
		 | otherwise = x:delete' y xs



-- question3 here #################################----------------------------

perms :: [Int] -> [[Int]]
perms []= []
perms [x] = [[x]]


--question 4 here
splitFirst :: Int -> [Int]-> [([Int],[Int])]
splitFirst n xs | n==1 = [((take 1 xs),(drop 1 xs))]
		| otherwise = ((take n xs),(drop n xs)):splitFirst (n-1) xs
		where len = length xs

split :: [Int] -> [([Int],[Int])]
split xs =reverse (splitFirst ((length xs)-1) xs)


--question 5 here ###############################
--exprs :: [Int] -> [Expr]

--list_to_expr [x] = Vs

exprs1 [x] = (Val x)
exprs1 [y,x] = App Add (Val y) (Val x)
exprs1 (x:xs) = App Add (Val x) (exprs1 xs)

exprs xs = [App Add (exprs1 (fst lst)) (exprs1 (snd lst))|lst<- (split xs)]

--question 6 here #######################################################
