**The Prisoner's Dilemma: A Fable**

In the mid-1920's, the Nebraska State Police achieved what may still be their finest moment. After a 400-mile car chase over dirt roads and through corn fields, they finally caught up with the notorious bank robbers Bunny and Clod. The two criminals were brought back to the police station in Omaha for further interrogation.

Bunny and Clod were questioned in separate rooms, and each was offered the same deal by the police. The deal went as follows (since both are the same, we need only describe the version presented to Bunny):

"Bunny, here's the offer that we are making to both you and Clod. If you both hold out on us, and don't confess to bank robbery, then we admit that we don't have enough proof to convict you. However, we will be able to jail you both for one year, for reckless driving and endangerment of corn. If you turn state's witness and help us convict Clod (assuming he doesn't confess), then you will go free, and Clod will get twenty years in prison. On the other hand, if you don't confess and Clod does, then he will go free and you will get twenty years."
"What happens if both Clod and I confess?" asked Bunny.

"Then you both get ten years," responded the police.

Bunny, who had been a math major at Cal Tech before turning to crime, reasoned this way:

Suppose Clod intends to confess. Then if I don't confess, I'll get twenty years, but if I do confess, I'll only get ten years. On the other hand, suppose Clod intends to hold out on the cops. Then if I don't confess, I'll go to jail for a year, but if I do confess, I'll go free. So no matter what Clod intends to do, I am better off confessing than holding out. So I'd better confess.
Naturally, Clod employed the very same reasoning. Both criminals confessed, and both went to jail for ten years. The police, of course, were triumphant, since the criminals would have been free in a year had both remained silent.

**The Prisoner's Dilemma**
The Bunny and Clod story is an example of a situation known in mathematical game theory as the "prisoner's dilemma." A prisoner's dilemma always involves two "game players," and each has a choice between "cooperating" and "defecting." If the two players cooperate, they each do moderately well; if they both defect, they each do moderately poorly. If one player cooperates and the other defects, then the defector does extremely well and the cooperator does extremely poorly. (In the case of the Bunny and Clod story, "cooperating" means cooperating with one's partner--i.e. holding out on the police--and "defecting" means confessing to bank robbery.) Before formalizing the prisoner's dilemma situation, we need to introduce some basic game theory notation.

**The Prisoner's Dilemma Game Matrix**



In this case, Players A and B both have a dominant choice - namely, defection. No matter what Player B does, Player A improves his own score by defecting, and vice versa.

However, there is something odd about this game. It seems as though the two players would benefit by choosing to cooperate. Instead of winning only one point each, they could win three points each. So the "rational" choice of mutual defection has a puzzling self-destructive flavor.

The second matrix is an example of a prisoner's dilemma game situation. Just to formalize the situation, let CC be the number of points won by each player when they both cooperate; let DD be the number of points won when both defect; let CD be the number of points won by the cooperating party when the other defects; and let DC be the number of points won by the defecting party when the other cooperates. Then the prisoner's dilemma situation is characterized by the following conditions:

DC > CC > DD > CD
CC > 	DC + CD
2
In the second game matrix, we have:

DC = 5,  CC = 3,  DD = 1,  CD = 0
so both conditions are met. In the Bunny and Clod story, by the way, you can verify that:

DC = 0,  CC = -1,  DD = -10,  CD = -20
Again, these values satisfy the prisoner's dilemma conditions.

**Axelrod's Tournament**
In the late 1970's, political scientist Robert Axelrod held a computer tournament designed to investigate the prisoner's dilemma situation (The rules and results of both his tournaments--he held two--are described in his book: The Evolution of Cooperation). Contestants in the tournament submitted computer programs that would compete in an iterated prisoner's dilemma game of approximately two hundred rounds, using the second matrix above. Each contestant's program played five iterated games against each of the other programs submitted, and after all games had been played the scores were tallied.

The contestants in Axelrod's tournament included professors of political science, mathematics, computer science, and economics. The winning program--the program with the highest average score--was submitted by Anatol Rapoport, a professor of psychology at the University of Toronto. In this problem set, we will pursue Axelrod's investigations and make up our own programs to play the iterated prisoner's dilemma game.

Before we look at the two-player program, it is worth speculating on what possible strategies might be employed in the iterated prisoner's dilemma game. Here are some examples:

*Meanie*
A program using the Meanie strategy simply defects on every round of every game.

*Sucker*
A program using the Sucker strategy cooperates on every round of every game.

*Democratic*
This program cooperates on the first round. On all subsequent rounds, Democratic examines the history of the other player's actions, counting the total number of defections and cooperations by the other player. If the other player's defections outnumber her cooperations, Democratic will defect; otherwise this strategy will cooperate.

*Eye-for-Eye*
This program cooperates on the first round, and then on every subsequent round it mimics the other player's previous move. Thus, if the other player cooperates (defects) on the nth round, then Eye-for-Eye will cooperate (defect) on the (n-1)st round.

All of these strategies are extremely simple. Indeed, the first two do not even pay any attention to the other player; their responses are uninfluenced by the previous rounds of the game. Nevertheless, simplicity is not necessarily a disadvantage. Rapoport's first-prize program employed the Eye-for-Eye strategy, and achieved the highest average score in a field of far more complicated programs.

**Three player Prisoner's Dilemma in Haskell**

Three ground rules for formulating payoffs:

Defection should be the dominant choice for each player. In other words, it should always be better for a player to defect, regardless of what the opponents do. This rule gives three constraints:
DCC > CCC
DDD > CDD
DCD > CCD
A player should always be better off if more of his opponents choose to cooperate. This rule gives:
DCC > DCD > DDD
CCC > CCD > CDD
If one player's choice is fixed, the other two players should be left in a two-player prisoner's dilemma. This rule gives the following constraints:
CCD > DDD
CCC > DCD
CCD > 	CDD + DCD
2
We can satisfy all of these constraints with the following payoffs:

CDD = 0,  DDD = 1,  CCD = 3,  DCD = 5  CCC = 7,  DCC = 9


*B: tough- and softEyeForEye*

Write two new strategies for three-player games: toughEyeForEye and softEyeForEye. toughEyeForEye should defect if either of the opponents defected on the previous round.  softEyeForEye should defect only if both opponents defected on the previous round. Play some games using these two new strategies to make sure everything is working (you needn't include these games in your solution, of course).

*C: softEyeForTwoEyes*

Write a new strategy called softEyeForTwoEyes. The strategy should always cooperate unless both opponents defected on both of the previous two rounds. (Looked at another way: softEyeForTwoEyes should cooperate if either opponent cooperated on either of the previous two rounds.) Play softEyeForTwoEyes against other strategies.

*D: makeEyeForNEyes*

Write a function called makeEyeForNEyes that takes a number as input and returns the appropriate softEyeForEye-like strategy. For example, (makeEyeForNEyes 2) should return a strategy equivalent to softEyeForTwoEyes.

*E: makeRotatingStrategy*

Write a function called makeRotatingStrategy that takes as input two strategies for a three-player game (say, strategy1 and strategy2) and an integer (say switch-point).  makeRotatingStrategy should return a strategy which plays strategy1 for the first switch-point rounds in the iterated game, then switches to strategy2 for the next switch-point rounds, and so on.

Use makeRotatingStrategy to define a function called makeTripleStrategy that takes as input three strategies and a switch point.

*F: Combining Two-Player Strategies*

Write a function called combineStrategies that takes as input two two-player strategies and a "combining" function. combineStrategies should return a three-player strategy that plays one of the two-player strategies against one of the opponents, and the other two-player strategy against the other opponent, then calls the "combining" procedure on the two two-player results. Here's an example: this call to combineStrategies returns a strategy equivalent to toughEyeForEye in Part B:

combineStrategies
    eyeForEye2P eyeForEye2P
    (\ r1 r2 -> if r1 == Defect || r2 == Defect then Defect else Cooperate)
    
The resulting strategy plays eyeForEye2P against each opponent, and then calls the combining function on the two results. If either of the two two-player strategies has returned Defect, then the three-player strategy will also return Defect.

Here's another example. This call to combineStrategies returns a three-player strategy that plays eyeForEye2P against one opponent, democratic2P against another, and chooses the "meaner" of the two results:

combineStrategies
    eyeForEye2P democratic2P
    (\ r1 r2 -> if r1 == Defect then r1 else r2)
    

