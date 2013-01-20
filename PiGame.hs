import Cards
import System.Random
import Data.List

data Player = PlayerA | PlayerB deriving Show
data PlayerState = PS { 
	player			:: Player, 
	hand  			:: Pile, 
	piles 			:: [Pile] 
} deriving Show
data Game = Start { 
	pickingPlayer	:: Player, 
	playerA      	:: PlayerState,
	playerB      	:: PlayerState,
	unpicked     	:: [Pile]	
} deriving Show

top :: Pile -> Card
top = head

draw :: Pile -> Int -> (Pile, Pile)
draw p n = (take n p, drop n p)