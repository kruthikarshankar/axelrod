-------------------------------------------------------------------------
-- | Program 1,        CS 5314,        Fall 2014
--
--   Author: Kruthika Rathinavel
--   Date:   09-14-2014
--
--   Two-Player Prisoner's Dilemma in Scheme
--
--   This file is separated into three sections:
--
--     * Part I  : Choice histories
--
--     * Part II : Sample strategies discussed in assignment
--
--     * Part III: Functions for playing rounds
--
module Prisoner where

-- ======================================================================
-- * Part I: Choice Histories
-- ======================================================================
-- $PartI
--   As rounds are being played, each player's choices are accumulated
--   in a \"history.\"  In later rounds, a strategy can refer to its own
--   or its opponent's history to decide on its next choice.  Basically,
--   this choice history is stored as a plain list of 'Choice' values.  The
--   most recent choice is first in the list, with the oldest at the rear.
--
--   This data structure is so simple that the operations that
--   manipulate history lists are simply renamings of basic list
--   functions.  Giving them new names improves the readability of
--   functions that manipulate histories, however.
--

-------------------------------------------------------------------------
-- | Represents a single choice by a player during one round.
--
data Choice = Cooperate | Defect
	deriving (Eq, Show)


-------------------------------------------------------------------------
-- | A synonym for a list of 'Choice' values, intended to represent
--   a sequence of choices made by a given player.  Choices are stored
--   in the list in reverse-chronological order (newest first, oldest last).
--
type History = [Choice]


-------------------------------------------------------------------------
-- | A synonym for a player strategy in a two-player game--that is, a
--   function that takes in two histories and returns a new choice.  The
--   first history should be "this player's", while the second is
--   "the opponent's".
--
type Strategy2P = History -> History -> Choice


-------------------------------------------------------------------------
mostRecentChoice :: History -> Choice
-- ^ Returns the most recent choice from a history (the element at the
--   front of the list).
--
--   >>> mostRecentChoice [Defect,Cooperate]
--   Defect
--
--   >>> mostRecentChoice [Cooperate,Cooperate,Defect]
--   Cooperate
--
mostRecentChoice = head


-------------------------------------------------------------------------
restOfChoices :: History -> History
-- ^ Returns a shorter history containing all but the most recent choice
--   from the given history (i.e., the same list without its first element).
--
--   >>> restOfChoices [Defect,Cooperate]
--   [Cooperate]
--
--   >>> restOfChoices [Cooperate,Cooperate,Defect]
--   [Cooperate,Defect]
--
restOfChoices = tail


-- ======================================================================
-- * Part II: Sample Strategies
-- ======================================================================
-- $PartII
--   A sampler of strategies, as described in the assignment.
--
--   Any strategy for the two-player (or N-player) game is a function
--   that takes two (or N) history lists as parameters, with \"your\"
--   history as the first argument and the histories of opponents as the
--   remaining arguments.  A strategy function should return
--   'Cooperate' or 'Defect'.
--

-------------------------------------------------------------------------
meanie2P :: History -> History -> Choice
-- ^ This strategy always defects.
--
--   Examples:
--
--   >>> meanie2P [] []
--   Defect
--
--   >>> meanie2P [Cooperate] [Cooperate]
--   Defect
---

meanie2P history1 history2 = Defect


-------------------------------------------------------------------------
sucker2P :: History -> History -> Choice
-- ^ This strategy always cooperates.
--
--   Examples:
--
--   >>> sucker2P [] []
--   Cooperate
--
--   >>> sucker2P [Defect] [Defect]
--   Cooperate
--
sucker2P history1 history2 = Cooperate


-------------------------------------------------------------------------
democratic2P :: History -> History -> Choice
-- ^ This strategy counts the opponent's choices and then makes the same
--   choice that was most commonly made by the opponent.  If the opponent
--   has made exactly the same number of defections and cooperations,
--   then the strategy chooses to cooperate.
--
--   Examples:
--
--   >>> democratic2P [] [Cooperate, Cooperate, Defect]
--   Cooperate
--
--   >>> democratic2P [] [Defect, Defect, Cooperate]
--   Defect
--
--   >>> democratic2P [] [Cooperate, Cooperate, Defect, Defect]
--   Cooperate
--
democratic2P _ history =
    if (length history + 1) `div` 2 <= countOccurrences Cooperate history
        then Cooperate
        else Defect


-------------------------------------------------------------------------
countOccurrences :: (Eq a) => a -> [a] -> Int
-- ^ Counts the number of times a given value appears in a list.  Used
--   by 'democratic2P'.
--
--   >>> countOccurrences 2 [1, 2, 3, 2, 1]
--   2
--
--   >>> countOccurrences 3 [1, 2, 3, 2, 1]
--   1
--
--   >>> countOccurrences 4 [1, 2, 3, 2, 1]
--   0
--
countOccurrences _ [] = 0
countOccurrences value (x:rest)
    | x == value  =  1 + countOccurrences value rest
    | otherwise   =  countOccurrences value rest


-------------------------------------------------------------------------
eyeForEye2P :: History -> History -> Choice
-- ^ This strategy always does the same thing the opponent did in the last
--   round.  In the very first round, it cooperates.
--
--   >>> eyeForEye2P [] [Cooperate, Defect]
--   Cooperate
--
--   >>> eyeForEye2P [] [Defect, Cooperate]
--   Defect
--
--   >>> eyeForEye2P [] []
--   Cooperate
--
eyeForEye2P _ []      = Cooperate
eyeForEye2P _ history = mostRecentChoice history

		
-- ======================================================================
-- * Part III: Functions for Playing a Set of Rounds
-- ======================================================================

-------------------------------------------------------------------------
playNRounds2P:: Int -> Strategy2P -> Strategy2P -> (Int, History, Int, History)
-- ^ This is the main function for playing a set of rounds.  It repeatedly
--   plays rounds between the given strategies.  It returns a tuple
--   containing the first strategy's score, then its history, then
--   the second strategy's score, and finally the second strategy's history.
--
--   Examples:
--
--   >>> playNRounds2P 4 (\ history1 history2 -> if length history1 == 0 then Cooperate else if (mostRecentChoice history1) == Defect then Cooperate else Defect) sucker2P
--   (16,[Defect,Cooperate,Defect,Cooperate],6,[Cooperate,Cooperate,Cooperate,Cooperate])
--
--   >>> playNRounds2P 4 meanie2P democratic2P
--   (8,[Defect,Defect,Defect,Defect],3,[Defect,Defect,Defect,Cooperate])
--
--   >>> playNRounds2P 4 democratic2P (\ history1 history2 -> if length history1 == 0 then Cooperate else if (mostRecentChoice history1) == Defect then Cooperate else Defect)
--   (6,[Cooperate,Cooperate,Cooperate,Cooperate],16,[Defect,Cooperate,Defect,Cooperate])
--
playNRounds2P 0 _ _ = (0, [], 0, [])
playNRounds2P n strategy1 strategy2 =
    let
        (prevScore1, prevHistory1, prevScore2, prevHistory2) =
            playNRounds2P (n - 1) strategy1 strategy2
        choice1 = strategy1 prevHistory1 prevHistory2
        choice2 = strategy2 prevHistory2 prevHistory1
    in
        (prevScore1 + score2P choice1 choice2,
         choice1 : prevHistory1,
         prevScore2 + score2P choice2 choice1,
         choice2 : prevHistory2)


-------------------------------------------------------------------------
score2P :: Choice -> Choice -> Int
-- ^ Compute the score (in a two-player game) for one player based on
--   his/her choice (the first argument) and the opponent's choice (the
--   second argument).
--
--   >>> score2P Cooperate Cooperate
--   3
--
--   >>> score2P Defect Cooperate
--   5
--
--   >>> score2P Defect Defect
--   1
--
--   >>> score2P Cooperate Defect
--   0
--
score2P Cooperate Cooperate = 3
score2P Cooperate Defect    = 0
score2P Defect    Cooperate = 5
score2P Defect    Defect    = 1

-- =====================================================================
-- * Part A: Changes for Three Players
-- =====================================================================
	
-------------------------------------------------------------------------
-- | A synonym for a player strategy in a three-player game--that is, a
--   function that takes in three histories and returns a new choice.  The
--   first history should be "this player's", while the second is
--   "the first opponent's" and the third, "the second opponent's".
--
type Strategy3P = History -> History -> History -> Choice


-------------------------------------------------------------------------
playNRounds3P :: Int -> Strategy3P -> Strategy3P -> Strategy3P -> (Int, History, Int, History, Int, History)
-- ^ This is the main function for playing a set of rounds for three players.  It repeatedly
--   plays rounds between the given strategies.  It returns a tuple
--   containing the first strategy's score, then its history,
--   the second strategy's score, then its history, and then 
--   the third strategy's score, and the third strategy's history.
--
--   Examples:
--
--   >>> playNRounds3P 4 toughEyeForEye toughEyeForEye softEyeForEye
--   (28,[Cooperate,Cooperate,Cooperate,Cooperate],28,[Cooperate,Cooperate,Cooperate,Cooperate],28,[Cooperate,Cooperate,Cooperate,Cooperate])
--
--   >>> playNRounds3P 0 toughEyeForEye (\ history1 history2 history3 -> if length history1 == 0 then Cooperate else if (mostRecentChoice history1) == Defect then Cooperate else Defect) softEyeForEye
--   (0,[],0,[],0,[])
--
--   >>> playNRounds3P 5 toughEyeForEye (\ history1 history2 history3 -> if length history1 == 0 then Cooperate else if (mostRecentChoice history1) == Defect then Cooperate else Defect) toughEyeForEye
--   (21,[Defect,Defect,Defect,Cooperate,Cooperate],17,[Cooperate,Defect,Cooperate,Defect,Cooperate],21,[Defect,Defect,Defect,Cooperate,Cooperate])
--
--   >>> playNRounds3P 4 (\ history1 history2 history3 -> if length history1 == 0 then Cooperate else if (mostRecentChoice history1) == Defect then Cooperate else Defect) softEyeForTwoEyes softEyeForTwoEyes
--   (32,[Defect,Cooperate,Defect,Cooperate],20,[Cooperate,Cooperate,Cooperate,Cooperate],20,[Cooperate,Cooperate,Cooperate,Cooperate])
--
--   >>> playNRounds3P 4 softEyeForEye (combineStrategies eyeForEye2P eyeForEye2P (\ r1 r2 -> if r1 == Defect || r2 == Defect then Defect else Cooperate)) (makeEyeForNEyes 3 )
--   (28,[Cooperate,Cooperate,Cooperate,Cooperate],28,[Cooperate,Cooperate,Cooperate,Cooperate],28,[Cooperate,Cooperate,Cooperate,Cooperate])
--
--   >>> playNRounds3P 4 softEyeForEye softEyeForTwoEyes (makeRotatingStrategy toughEyeForEye softEyeForEye 3)
--   (28,[Cooperate,Cooperate,Cooperate,Cooperate],28,[Cooperate,Cooperate,Cooperate,Cooperate],28,[Cooperate,Cooperate,Cooperate,Cooperate])
--
--   >>> playNRounds3P 7 softEyeForEye softEyeForTwoEyes (makeTripleStrategy softEyeForTwoEyes softEyeForEye softEyeForEye 2)
--   (49,[Cooperate,Cooperate,Cooperate,Cooperate,Cooperate,Cooperate,Cooperate],49,[Cooperate,Cooperate,Cooperate,Cooperate,Cooperate,Cooperate,Cooperate],49,[Cooperate,Cooperate,Cooperate,Cooperate,Cooperate,Cooperate,Cooperate])
--
--   >>> playNRounds3P 5 softEyeForEye  (\ history1 history2 history3 -> if length history1 == 0 then Cooperate else if (mostRecentChoice history1) == Defect then Cooperate else Defect) (combineStrategies (\ history1 history2 -> if length history1 == 0 then Cooperate else if (mostRecentChoice history1) == Defect then Cooperate else Defect) sucker2P (\ r1 r2 -> if r1 == Defect then r1 else r2))
--   (25,[Defect,Cooperate,Defect,Cooperate,Cooperate],23,[Cooperate,Defect,Cooperate,Defect,Cooperate],23,[Cooperate,Defect,Cooperate,Defect,Cooperate])
--
playNRounds3P 0 _ _ _ = (0, [], 0, [], 0, [])
playNRounds3P n strategy1 strategy2 strategy3 =
    let
		(prevScore1, prevHistory1, prevScore2, prevHistory2, prevScore3, prevHistory3) =
			playNRounds3P (n - 1) strategy1 strategy2 strategy3
		choice1 = strategy1 prevHistory1 prevHistory2 prevHistory3
		choice2 = strategy2 prevHistory2 prevHistory3 prevHistory1
		choice3 = strategy3 prevHistory3 prevHistory1 prevHistory2
    in
		(prevScore1 + score3P choice1 choice2 choice3,
         choice1 : prevHistory1,
         prevScore2 + score3P choice2 choice3 choice1,
         choice2 : prevHistory2,
		 prevScore3 + score3P choice3 choice1 choice2,
         choice3 : prevHistory3)
		 

-------------------------------------------------------------------------
score3P :: Choice -> Choice -> Choice -> Int
-- ^ Compute the score (in a three-player game) for one player based on
--   his/her choice (the first argument) and the opponents' choices (the
--   second argument - first opponent's choice, and the third argument - 
--	 second opponent's choice).
--
--   >>> score3P Cooperate Defect Defect
--   0
--
--   >>> score3P Defect Defect Defect
--   1
--
--   >>> score3P Cooperate Cooperate Defect 
--   3
--
--   >>> score3P Cooperate Defect Cooperate 
--   3
--
--   >>> score3P Defect Cooperate Defect
--   5
--
--   >>> score3P Defect Defect Cooperate
--   5
--
--   >>> score3P Cooperate Cooperate Cooperate
--   7
--
--   >>> score3P Defect Cooperate Cooperate
--   9
--
score3P Cooperate Defect    Defect    = 0
score3P Defect    Defect    Defect    = 1
score3P Cooperate Cooperate Defect    = 3
score3P Cooperate Defect Cooperate    = 3
score3P Defect    Cooperate Defect    = 5
score3P Defect    Defect Cooperate    = 5
score3P Cooperate Cooperate Cooperate = 7
score3P Defect    Cooperate Cooperate = 9

-------------------------------------------------------------------------
meanie3P :: History -> History -> History -> Choice
-- ^ This strategy always defects.
--
--   Examples:
--
--   >>> meanie3P [] [] []
--   Defect
--
--   >>> meanie3P [Cooperate] [Cooperate] [Defect]
--   Defect
--
meanie3P history1 history2 history3 = Defect


-------------------------------------------------------------------------
sucker3P :: History -> History -> History -> Choice
-- ^ This strategy always cooperates.
--
--   Examples:
--
--   >>> sucker3P [] [] []
--   Cooperate
--
--   >>> sucker3P [Defect] [Defect] [Cooperate]
--   Cooperate
--
sucker3P history1 history2 history3 = Cooperate


-- ======================================================================
-- * Part B: tough- and softEyeForEye
-- ======================================================================

-------------------------------------------------------------------------
toughEyeForEye :: History -> History -> History -> Choice
-- ^ This strategy defects if either of the opponents defected on the 
--   previous round.
--
--   >>> toughEyeForEye [] [Cooperate,Cooperate] [Defect,Cooperate]
--   Defect
--
--   >>> toughEyeForEye [Defect,Defect] [Defect,Cooperate] [Defect,Defect]
--   Defect
--
--   >>> toughEyeForEye [] [Cooperate,Cooperate] [Cooperate,Cooperate]
--   Cooperate
--
--   >>> toughEyeForEye [Defect,Defect] [Cooperate,Cooperate] [Cooperate,Cooperate]
--   Cooperate
--
--   >>> toughEyeForEye [] [] []
--   Cooperate
--
toughEyeForEye _ [] [] = Cooperate
toughEyeForEye history1 history2 history3 = 
	if (mostRecentChoice history2 == Defect || mostRecentChoice history3 == Defect)
	then Defect
	else Cooperate
	
	
-------------------------------------------------------------------------
softEyeForEye :: History -> History -> History -> Choice
-- ^ This strategy defects only if both opponents defected on the 
--   previous round.
--
--   >>> softEyeForEye [] [Cooperate,Cooperate] [Defect,Cooperate]
--   Cooperate
--
--   >>> softEyeForEye [] [Defect,Cooperate] [Defect,Cooperate]
--   Defect
--
--   >>> softEyeForEye [Defect,Defect] [Cooperate,Cooperate] [Cooperate,Cooperate]
--   Cooperate
--
--   >>> softEyeForEye [] [] []
--   Cooperate
--
softEyeForEye _ [] [] = Cooperate
softEyeForEye history1 history2 history3 = 
	if (mostRecentChoice history2 == Defect && mostRecentChoice history3 == Defect)
	then Defect
	else Cooperate
	

	
-- =====================================================================
-- * Part C: softEyeForTwoEyes
-- =====================================================================
	
-------------------------------------------------------------------------
softEyeForTwoEyes :: History -> History -> History -> Choice
-- ^ This strategy always cooperates unless both opponents defected
--   on both of the previous two rounds.
--
--   >>> softEyeForTwoEyes [] [Cooperate,Cooperate] [Defect,Cooperate]
--   Cooperate
--
--   >>> softEyeForTwoEyes [] [Defect,Defect,Cooperate] [Defect,Defect,Defect]
--   Defect
--
--   >>> softEyeForTwoEyes [Defect,Defect,Defect] [Defect,Cooperate,Cooperate] [Cooperate,Defect,Defect]
--   Cooperate
--
--   >>> softEyeForTwoEyes [] [] []
--   Cooperate
--
--   >>> softEyeForTwoEyes [Defect] [Defect] [Defect]
--   Cooperate
--
softEyeForTwoEyes _ [] [] = Cooperate
softEyeForTwoEyes p (q:xq) (r:xr) = 
	if (softEyeForEye p (q:xq) (r:xr) == Defect &&  softEyeForEye p xq xr == Defect)
	then Defect	
	else Cooperate
	

-- =====================================================================
-- * Part D: makeEyeForNEyes
-- =====================================================================

-------------------------------------------------------------------------
makeEyeForNEyes :: Int -> Strategy3P
-- ^ This function, makeEyeForNEyes takes a number as input 
--   and returns the appropriate softEyeForEye-like strategy. 
--   For example, makeEyeForNEyes 2 should return a strategy  
--   equivalent to softEyeForTwoEyes.
-- 
--   >>> makeEyeForNEyes 2 [Cooperate,Defect] [Cooperate,Cooperate] [Defect,Cooperate]
--   Cooperate
--
--   >>> makeEyeForNEyes 3 [Cooperate,Defect] [Cooperate,Cooperate] [Defect,Defect]
--   Cooperate
--
--   >>> makeEyeForNEyes 3 [Cooperate,Cooperate,Cooperate] [Defect,Defect,Defect] [Defect,Defect,Defect]
--   Defect
--
--   >>> makeEyeForNEyes 3 [] [] []
--   Cooperate
--
--   >>> makeEyeForNEyes 0 [] [] []
--   Cooperate
--
--   >>> makeEyeForNEyes 1 [] [] []
--   Cooperate
--
--   >>> makeEyeForNEyes 1 [Defect] [Defect] [Defect]
--   Defect
--
makeEyeForNEyes 0 _ _ _ = Cooperate
makeEyeForNEyes 1 x y z = softEyeForEye x y z
makeEyeForNEyes n p q r	
	| softEyeForEye p q r == Defect &&  makeEyeForNEyes kn xp xq xr == Defect = Defect
	| otherwise = Cooperate
		where 	
			kn = n-1
			xp = if length p > 1 then tail p else []
			xq = if length q > 1 then tail q else []
			xr = if length r > 1 then tail r else []
			

-- =====================================================================
-- * Part E: makeRotatingStrategy
-- =====================================================================

-------------------------------------------------------------------------
makeRotatingStrategy :: Strategy3P -> Strategy3P -> Int -> Strategy3P
-- ^ The function makeRotatingStrategy takes as input two strategies 
--   for a three-player game and an 
--   integer, say switch-point.  This function returns a strategy 
--   which plays strategy1 for the first switch-point rounds in the 
--   iterated game, then switches to strategy2 for the next 
--   switch-point rounds, and so on.
--
--   >>> makeRotatingStrategy (\ history1 history2 history3 -> if length history2 == 0 then Cooperate else if (mostRecentChoice history1) == Defect then Cooperate else Defect) softEyeForEye 2 [Cooperate,Defect] [Cooperate,Cooperate] [Defect,Cooperate]
--   Cooperate
--
--   >>> makeRotatingStrategy (makeEyeForNEyes 3) (\ history1 history2 history3 -> if length history2 == 0 then Cooperate else if (mostRecentChoice history1) == Defect then Cooperate else Defect) 4 [Defect,Defect,Defect] [Defect,Defect,Defect] [Defect,Defect,Defect]
--   Defect
--
--   >>> makeRotatingStrategy (\ history1 history2 history3 -> if length history2 == 0 then Cooperate else if (mostRecentChoice history1) == Defect then Cooperate else Defect) meanie3P 2 [Defect,Defect,Cooperate,Defect] [Defect,Defect,Defect,Defect] [Defect,Defect, Defect,Defect]
--   Cooperate
--
--   >>> makeRotatingStrategy meanie3P (makeTripleStrategy softEyeForTwoEyes meanie3P softEyeForTwoEyes 2) 2 [Cooperate,Defect] [Cooperate,Cooperate] [Defect,Cooperate]
--   Defect
--
--   >>>  makeRotatingStrategy (\ history1 history2 history3 -> if length history2 == 0 then Cooperate else if (mostRecentChoice history1) == Defect then Cooperate else Defect) (\ history1 history2 history3 -> if length history1 == 0 then Cooperate else if (mostRecentChoice history1) == Defect then Cooperate else Defect) 2 [Cooperate,Defect,Defect] [Cooperate,Defect,Defect] [Defect,Cooperate,Cooperate]
--   Defect
--
--   >>>  makeRotatingStrategy meanie3P sucker3P 2 [Defect,Cooperate,Defect,Defect,Defect,Defect,Cooperate,Defect] [Defect,Cooperate,Defect,Defect,Defect,Defect,Cooperate,Defect] [Cooperate,Defect,Cooperate,Cooperate,Defect,Defect,Cooperate,Cooperate]
--   Defect
--
--   >>> makeRotatingStrategy sucker3P softEyeForEye 3 [] [] []
--   Cooperate
--
--makeRotatingStrategy strategy1 strategy2 0 _ history2 history3 = strategy1 [] history2 history3 
makeRotatingStrategy strategy1 strategy2 x history1 history2 history3 =
	if (even (lengthOfHistory `div` x))
	then strategy1 history1 history2 history3 
	else strategy2 history1 history2 history3
	where
		lengthOfHistory = length history1
		

-------------------------------------------------------------------------
makeTripleStrategy :: Strategy3P -> Strategy3P -> Strategy3P -> Int ->  Strategy3P
-- ^ The function makeTripleStrategy uses makeRotatingStrategy and takes 
--   three strategies and a switch point as inputs for a three player 
--   game, and returns a strategy which plays strategy1 for the first 
--   switch-point rounds, strategy2 for the second switch-point rounds, 
--   and strategy3 for the third switch-point rounds and so on.
--
--   >>> makeTripleStrategy meanie3P (\ history1 history2 history3 -> if length history1 == 0 then Cooperate else if (mostRecentChoice history1) == Defect then Cooperate else Defect) softEyeForTwoEyes 2 [Cooperate,Defect,Defect,Cooperate,Defect] [Cooperate,Cooperate,Defect,Cooperate,Defect] [Defect,Cooperate,Defect,Cooperate,Defect]
--   Cooperate
--
--   >>> makeTripleStrategy sucker3P (\ history1 history2 history3 -> if length history1 == 0 then Cooperate else if (mostRecentChoice history1) == Defect then Cooperate else Defect) softEyeForEye 3 [Defect,Defect,Cooperate] [Cooperate,Defect,Defect] [Defect,Defect,Defect]
--   Cooperate
--
--   >>> makeTripleStrategy (\ history1 history2 history3 -> if length history1 == 0 then Cooperate else if (mostRecentChoice history1) == Defect then Cooperate else Defect) sucker3P softEyeForEye 2 [Defect,Defect,Cooperate] [Defect,Defect,Cooperate] [Defect,Defect,Cooperate]
--   Cooperate
--
--   >>> makeTripleStrategy (makeRotatingStrategy softEyeForTwoEyes softEyeForEye 2) meanie3P (\ history1 history2 history3 -> if length history1 == 0 then Cooperate else if (mostRecentChoice history1) == Defect then Cooperate else Defect) 2 [Defect,Defect] [Defect,Defect] [Defect,Defect]
--   Defect
--
--   >>> makeTripleStrategy toughEyeForEye meanie3P sucker3P 2 [Defect,Defect] [Defect,Defect] [Defect,Defect]
--   Defect
--
--   >>> makeTripleStrategy toughEyeForEye softEyeForEye softEyeForEye 3 [] [] []
--   Cooperate
--
--makeTripleStrategy strategy1 strategy2 strategy3 0 history1 history2 history3 = strategy1 history1 history2 history3 
makeTripleStrategy strategy1 strategy2 strategy3 x history1 history2 history3
	| y == 0 = makeRotatingStrategy strategy1 strategy1 x history1 history2 history3
	| y == 1 = makeRotatingStrategy strategy2 strategy2 x history1 history2 history3
	| y == 2 = makeRotatingStrategy strategy3 strategy3 x history1 history2 history3
	where 
		lengthOfHistory = length history1
		z = lengthOfHistory `div` x	
		y = z `mod` 3  
		
		
-- =====================================================================
-- * Part F: Combining Two-Player Strategies
-- =====================================================================
		
-------------------------------------------------------------------------
combineStrategies :: Strategy2P -> Strategy2P -> (Choice -> Choice -> Choice) -> Strategy3P
-- ^ The function combineStrategies takes as input two two-player strategies and a 
--   "combining" function. It returns a three-player strategy that plays one of 
--   the two-player strategies against one of the opponents, and the other 
--   two-player strategy against the other opponent, then calls the "combining" 
--   procedure on the two two-player results.
--
--   >>> combineStrategies eyeForEye2P eyeForEye2P (\ r1 r2 -> if r1 == Defect || r2 == Defect then Defect else Cooperate) [Cooperate,Defect] [Cooperate,Cooperate] [Defect,Cooperate]
--   Defect
--
--   >>> combineStrategies (\ history1 history2 -> if length history1 == 0 then Cooperate else if (mostRecentChoice history1) == Defect then Cooperate else Defect) democratic2P (\ r1 r2 -> if r1 == Defect then r1 else r2) [Cooperate,Cooperate] [Cooperate,Cooperate] [Cooperate,Cooperate]
--   Defect
--
--   >>> combineStrategies eyeForEye2P sucker2P (\ r1 r2 -> if r1 == Defect then r1 else r2) [] [] []
--   Cooperate
--
--   >>> combineStrategies meanie2P (\ history1 history2 -> if length history1 == 0 then Cooperate else if (mostRecentChoice history1) == Defect then Cooperate else Defect) (\ r1 r2 -> if r1 == Defect then r1 else r2) [Cooperate,Cooperate] [Cooperate,Cooperate] [Cooperate,Cooperate]
--   Defect
--
combineStrategies strategy2p1 strategy2p2 newfunction history1 history2 history3 =  newfunction choice1 choice2
	where
		choice1 = strategy2p1 history1 history2
		choice2 = strategy2p2 history1 history3
