### Overview of Purpose of Project
We are going to make an interface for Texas Hold ‘em Poker along with an AI so that the user can play against computer players and potentially other human players via a server. It will be command-line based for the UI, and the user will be able to adjust the difficulty of the AI.

### Complete Mock Use of App
User:

$ ./poker.exe
Welcome to OCaml Hold 'em!

Enter your name:
> Tobi
Hello, Tobi. How many bots would you like to play against? (1-5):
> 1
Setting up table: Tobi vs. Bot_1.
Blinds are $5 (Small) / $10 (Big).
All players start with $1000.

--------------------
--- HAND 1 ---
--------------------
Tobi is the Dealer (Small Blind: $5).
Bot_1 is the Big Blind: $10.
Pot: $15

Dealing hole cards...

[Your View: Tobi]
Your Hand: [A♠, K♥]
Your Chips: $995
Current Bet to Call: $10 (from Bot_1)
Pot: $15

Your action? (Options: [f]old, [c]all, [r]aise [amount])
> r 30
Tobi raises to $30.
Bot_1 is thinking...
Bot_1 calls $20.

--------------------
--- THE FLOP ---
--------------------
Pot: $60
Community Cards: [A♣, 10♦, 5♠]

[Your View: Tobi]
Your Hand: [A♠, K♥]
Board: [A♣, 10♦, 5♠]
Your Best Hand: Pair of Aces (A, A, K, 10, 5)
Your Chips: $970

Your action? (You are first to act.)
(Options: [f]old, [c]heck, [b]et [amount])
> b 40
Tobi bets $40.
Bot_1 is thinking...
Bot_1 calls $40.

--------------------
--- THE TURN ---
--------------------
Pot: $140
Community Cards: [A♣, 10♦, 5♠] [K♦]

[Your View: Tobi]
Your Hand: [A♠, K♥]
Board: [A♣, 10♦, 5♠] [K♦]
Your Best Hand: Two Pair (Aces and Kings)
Your Chips: $930

Your action?
(Options: [f]old, [c]heck, [b]et [amount])
> c
Tobi checks.
Bot_1 is thinking...
Bot_1 checks.

--------------------
--- THE RIVER ---
--------------------
Pot: $140
Community Cards: [A♣, 10♦, 5♠] [K♦] [2♥]

[Your View: Tobi]
Your Hand: [A♠, K♥]
Board: [A♣, 10♦, 5♠] [K♦] [2♥]
Your Best Hand: Two Pair (Aces and Kings)
Your Chips: $930

Your action?
(Options: [f]old, [c]heck, [b]et [amount])
> b 100
Tobi bets $100.
Bot_1 is thinking...
Bot_1 calls $100.

--------------------
--- SHOWDOWN ---
--------------------
Pot: $340
Tobi shows: [A♠, K♥] (Hand: Two Pair, Aces and Kings)
Bot_1 shows: [A♥, Q♠] (Hand: Pair of Aces)

Tobi wins $340.

[Chip Counts]
Tobi: $1170
Bot_1: $830

Play another hand? ([y]es, [n]o)
> y

--------------------
--- HAND 2 ---
--------------------
Bot_1 is the Dealer (Small Blind: $5).
Tobi is the Big Blind: $10.
Pot: $15

Bot_1 is thinking...
Bot_1 calls $5.
Pot: $20

Dealing hole cards...

[Your View: Tobi]
Your Hand: [7♦, 2♠]
Your Chips: $1160
Current Bet to Call: $10 (You are BB)
Pot: $20

Your action? (Bot_1 limped in. You can check.)
(Options: [f]old, [c]heck, [r]aise [amount])
> c
Tobi checks.

--------------------
--- THE FLOP ---
--------------------
Pot: $20
Community Cards: [Q♣, J♦, 9♠]

[Your View: Tobi]
Your Hand: [7♦, 2♠]
Board: [Q♣, J♦, 9♠]
Your Best Hand: Queen High
Your Chips: $1160

Your action? (You are first to act.)
(Options: [f]old, [c]heck, [b]et [amount])
> c
Tobi checks.
Bot_1 is thinking...
Bot_1 bets $15.

Your action?
(Options: [f]old, [c]all, [r]aise [amount])
> f
Tobi folds.
Bot_1 wins $35.

[Chip Counts]
Tobi: $1160
Bot_1: $845

Play another hand? ([y]es, [n]o)
> n
Thanks for playing OCaml Hold 'em!
Your final chip count is $1160.
$


### List of Libraries We are Planning on Using
We will be using the following libraries: Owl, Core, Ounit2, LWT

### Implementation Plan
We plan to implement the following modules/features in this order:
  - Card module (by November 17)
  - Deck module (by November 17)
  - Player module (by November 19)
  - Core gameplay features (by November 24)
    - Game module
    - Interface module
    - Bot module (abstract type)
  - Bot variants (by December 1)
  - Main (by December 1)
  - Testing (by December 3)

### Other Information