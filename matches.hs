{-# OPTIONS_GHC -Wall #-}
module GAME where

import Data.Set (fromList, toList)
removeDuplicates :: (Eq a, Ord a) => [a] -> [a]
removeDuplicates = toList . fromList

type Pair = ([Int], Char)

type List = [Pair]

type Res = [[(Int,Int)]]


perehids :: [(Int, Int, Int)]
perehids = [(0,7,3),(3, 1, 3),(3, 7, 2),(4, 1, 2),(6, 5, 1),(7, 1, 1),(8, 0, 1),(8, 2, 2),(8, 3, 2),(8, 4, 3),(8, 5, 2),(8, 6, 1),(8, 9, 1),(9, 3, 1),(9, 4, 2),(9, 5, 1),(9, 7, 3)]

perehidS :: (Char, Char, Int)
perehidS = ('+', '-', 1)

change :: Int -> Int -> Int -> Int -> Bool
change numb n res x = if ((elem (numb, res, x) perehids) && (n+1 > x)) then True else False


changes :: [Int] -> Int -> Res
changes [] _ = []
--changes [x] n = [[(y,z)|z<-[1,2,3],y<-[0..9],change x n y z]]
changes (xi:xs) n = [(y,z)|z<-[1,2,3],y<-[0..9],change xi n y z]:changes xs n

changesS :: Char -> Int -> [(Char,Int)]
changesS xch _ = [(y,1)|y<-['+','-','='],(xch, y, 1) == perehidS]

chList :: (List,[Int]) -> Int -> ([(Res,[(Char,Int)])],Res)
chList ([],i) n = ([], []:changes i n)
chList ((xi,xch):x,i) n = ((changes xi n, ((xch,0):changesS xch n)) : fst (chList (x,i) n), changes i n)


changesB :: [Int] -> Int -> Res
changesB [] _ = []
changesB (xi:xs) n = [(y,z)|z<-[1,2,3],y<-[0..9],change y n xi z]:changesB xs n

changesSB :: Char -> Int -> [(Char,Int)]
changesSB xch _ = [(y,1)|y<-['+','-','='],(y, xch, 1) == perehidS]

chListBack :: (List,[Int]) -> Int -> ([(Res,[(Char,Int)])],Res)
chListBack ([],i) n = ([], changesB i n)
chListBack ((xi,xch):x,i) n = ((changesB xi n, ((xch,0):changesSB xch n)) : fst (chListBack (x,i) n), changesB i n)

list_in_eq :: (List,[Int]) -> ([(Int,Char)],Int)
list_in_eq (_,0:_:_) = ([(0,'f')],0)
list_in_eq ((0:_:_,_):_,_) = ([(0,'f')],0)
list_in_eq ([],i) =  ([], toInt i)
list_in_eq ((xi,xch):xs,i) =  ((toInt xi, xch): fst (list_in_eq (xs,i)), toInt i)

toInt :: [Int] -> Int
toInt [x] = x
toInt (x:xs) = x*10^(length xs) + toInt xs

iss_list :: Res -> Bool
iss_list [[]] = False
iss_list ([]:[_]:_) = True


toto :: ([(Res,[(Char,Int)])],Res) -> [([(Res,(Char,Int))],Res)]
toto (xss,i) = let s = [[(a2,b)|a2<-(comb $ map (ful) (fst xs)),b<-(snd xs)]|xs<-xss] in [(c,a1)|a1<-(comb $ map (ful) i),c<-comb s]

ful :: [(Int,Int)] -> Res
ful [] = [[]]
ful [a] = [[],[a]]
ful a = []:[[a1]|a1<-a]

comb :: [[a]] -> [[a]]
comb [] = []
comb [a] = [[a1]|a1<-a]
comb (a1:a2) = [[a11]++a22|a11<-a1,a22<-(comb a2)]

clear_price :: ([(Res,[(Char,Int)])],Res) -> Int -> [([(Res,(Char,Int))],Res)]
clear_price l p = [a|a<-toto l, p == sump a]

--([([[],[(7,2)]],('+',0)),([[(3,2)],[(5,1)]],('=',0))],[[]])
sump :: ([(Res,(Char,Int))],Res) -> Int
sump ([],[]) = 0
sump ([],[[]]) = 0
sump ([],(x:xs)) = if (x == []) then 0 + sump ([],xs) else (snd (head x)) + sump ([],xs)
sump (x:xs,i) = sump ([],fst x) + (snd (snd x)) + sump (xs,i)

move_list :: (List,[Int])->([(Res,(Char,Int))],Res)->(List,[Int])
move_list ([],[]) _ = ([], [])
move_list ([],hi:it) ([],chi) = if (head chi == []) then ([], hi : (snd (move_list ([],it) ([],(tail chi)))) )
 else ([],fst (head (head chi)) : (snd (move_list ([],it) ([],(tail chi)))) )
move_list ((xi,_):xs,i) ((xchi,xchch):xsch,chi) = ( ( (snd (move_list ([],xi) ([], xchi) )),(fst xchch)): (fst (move_list (xs,i) (xsch,chi) )) ,(snd (move_list ([],i) ([], chi) )) )

arifm :: ([(Int,Char)],Int) -> Bool
arifm ([(n1,'=')],n) = if (n1 == n) then True else False
arifm ((i,ch):(i2,ch2):xs,n) = if (ch == '+') then arifm ((i+i2,ch2):xs,n) else if (ch == '-') then arifm ((i-i2,ch2):xs,n) else False
arifm _ = False

doThings :: (List,[Int]) -> Int -> ([(Res,[(Char,Int)])],Res) -> [([(Int,Char)],Int)]
doThings l p ch = [list_in_eq (move_list l x) |x<- (clear_price ch p)]

solution :: (List,[Int]) -> Int -> [([(Int,Char)],Int)]
solution l p = let ch = (chList l p) in [x |x<- doThings l p ch, arifm x]

generator :: (List,[Int]) -> Int -> [([(Int,Char)],Int)]
generator r p = doThings r p (chListBack r p)

game :: (List,[Int]) -> Int -> ([([(Int,Char)],Int)],[([(Int,Char)],Int)])
game l p = let s = solution l p in ([r|r<-s], (removeDuplicates ([g|r<-s, g<-generator (back_l r) p])))

back_l :: ([(Int,Char)],Int) -> (List,[Int])
back_l ([],i) = ([],(fromInt i))
back_l ((xi,xch):xs,i) = ((fromInt xi, xch):(fst (back_l (xs,i))),(fromInt i))


fromInt :: Int -> [Int]
fromInt x = if(x `div` 10 > 0) then fromInt (x `div` 10) ++ [x `mod` 10] else [x]