```
// ****************************************************************
// *   Software Failure.  Press left mouse button to continue.    *
// *             Guru Meditation #00000000.00000000               *
// ****************************************************************
```

## What is this?

This is Guru Meditations's entry to the ICFP 2012 Contest.

## Who are you?

* André Silva (ShiftForward)
* Hugo Sereno Ferreira (ShiftForward, FEUP)
* João Azevedo (ShiftForward)

## How did you do this?

We are big fans of the object-functional language Scala, and that's what we used for coding. We also used SBT, Emacs, Ensime and IntelliJ.

## What was your score?

We actually never submitted :-) We deemed our scores not good-enough™ for a modest classification. However, judging from IRC and Twitter reactions, we might have been wrong. At aproximately 12h before the end of the contest, our scores were:

```
Map1:
Map2:
...
```

## Ok... so... why did you released the code?

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

This allowed us to quickly implement all the game rules as they were announced. It also allowed the solver to independently test all of the possible move patterns "blindly", which allowed the bot to "automatically" adapt to new movements as they were implemented on the simulator.