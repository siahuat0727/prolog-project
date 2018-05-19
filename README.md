# Tic-Tac-Toe

## How to run
```
swipl --load tictactoe.pl
```

s : showBoard

h(x, y) : mark position (x, y) as 'x'

c : computer turn

## Example
```
?- s.
    # # #
    # # #
    # # #
true .

?- h(1,2).
    # x #
    # # #
    # # #
true .

?- c.
    # x #
    # o #
    # # #
true .

?- h(1,1).
    x x #
    # o #
    # # #
true .

?- c.
    x x o
    # o #
    # # #
true .

?-
...
```
