```
// ****************************************************************
// *   Software Failure.  Press left mouse button to continue.    *
// *             Guru Meditation #00000000.00000000               *
// ****************************************************************
```

## What is this?

This is Guru Meditation's entry to the [ICFP 2012 Contest](http://icfpcontest2012.wordpress.com/).

## Who are you?

* André Silva (ShiftForward)
* Hugo Sereno Ferreira (ShiftForward, FEUP)
* Joao Azevedo (ShiftForward)

## How did you do this?

We are big fans of the object-functional language Scala, and that's what we used for coding. We also used SBT, Emacs, Ensime and IntelliJ.

## What was your score?

We actually never submitted :-) We deemed our scores not good-enough™ for a modest classification. However, judging from IRC and Twitter reactions, we might have been wrong. At aproximately 12h before the end of the contest, our scores were (ignoring maps with trampolines, which our solver wasn't properly processing):

```
contest1.map: 210
contest2.map: 278
contest3.map: 275
contest4.map: 575
contest5.map: 1291
contest6.map: 677
contest7.map: 867
contest8.map: 1269
contest9.map: 1917
contest10.map: 1931
flood1.map: 355
flood2.map: 278
flood3.map: 747
flood4.map: 957
flood5.map: 571
beard1.map: 437
beard2.map: 4507
beard4.map: 1460
```

## Ok... so... why did you release the code?

Two reasons. First, the number of Scala entries are very low. Opening the code *might* attract more people to use the language.

The second reason is because we think the Pattern Matching algorithm we used in the simulator is cool. We know there are lots of ways to enhance it, including the design of an external DSL to capture the patterns more "graphically", and it's probably slower when compared to an in-place, mutable, switch-case based C implementation... but it's still cool :-)

## Could you talk a little on your solving strategy?

### Patterns

A pattern looks like this:

```scala
val MvUpRazor = Pattern(OpcodePred('MoveUp),
                  Seq((0, -1) -> 'Razor, (0, 0) -> 'Robot),
                  Seq((0, -1) -> 'Robot, (0, 0) -> 'Empty),
                  { s => s.copy(nRazors = s.nRazors + 1, robotPos = s.robotPos + Coordinate(0, -1)) } )
```

There's a predicate to test if the pattern is applicable (based on the opcode and board), in this case this pattern is only applicable when the 'MoveUp opcode is triggered.
The pattern is composed of two matrices which are centered on the target Tile. The source matrix is used to match a pattern scenario on the board, and if it passes, the board is updated
according to the transformation matrix. Additionally we can pass a function to capture additional side-effects on the Board (e.g. like changing the number of razors).

This allowed us to quickly implement all the game rules as they were announced. It also allowed the solver to independently test all of the possible move patterns "blindly", which allowed the bot to automatically adapt to new movements as they were implemented on the simulator (of course the heuristic still had to be changed to take into account new rules).

### Solver

We started by a greedy best-first approach, just trying to fetch the lambdas that were closer to the bot. This quickly revealed itself as a bad strategy, specially in tricky maps where either the lambdas or the lift would become unreachable (such as contest8.map). We then evolved into a generic A* that would try the available opcodes in each state. The state was identified by the hash code of the map tiles, in order to reduce space. Our heuristic was based on the map score, penalizing the number of moves and prioritizing the number of lambdas gathered. We would also benefit states where we were closer to a lambda or to the lift (in case all lambdas were gathered) and penalize states where either a lambda or the lift was unreachable. In order to check for reachability, we would perform a flood fill starting on the bot position. For every interesting element (lambda or lift) outside the fill area, we would attempt to compute a path from the bot's current position to the element. We would also keep a cache of paths (which, in the end, I didn't think it was correctly implemented). We spent a generous amount of time tweaking the heuristic, and didn't consider flooding, beards, trampolines or higher order rocks in it.

### WTF is it with Tile and Opcode?!

We started by representing tiles and opcodes as case classes which allowed for all the pattern matching goodness. Unfortunately it was slow(ish) and had an higher memory consumption, so the move to Symbol was a cheap performance optimization. Looking back on it, I forgot to try case objects instead...


