# Plan: Fix Folded Players Not Being Reset for New Hand

## Problem Analysis

**Current Issue:**
When a player folds during a hand, their `folded` field is set to `true`. When a new hand starts, this `folded = true` status persists, causing:
- Folded players to be excluded from `get_active_players`
- The game to skip them entirely
- The game loop to think there are no active players or skip to asking "play again"

**Root Cause:**
The `folded` status is not reset to `false` when starting a new hand. This happens in two places:
1. `handle_winner` in `poker.ml` - when starting a new hand after a winner
2. `next_street` Showdown case in `game.ml` - when transitioning to a new hand

## Solution Plan

### Step 1: Create Helper Function to Reset Folded Status
**Location**: `src/lib/player.ml` or `src/lib/table.ml`

**Option A - In Player module:**
```ocaml
val reset_folded : t -> t
(** [reset_folded player] returns player with folded = false. *)
```

**Option B - In Table module:**
```ocaml
val reset_all_folded : t -> t
(** [reset_all_folded table] returns table with all players' folded status reset to false. *)
```

**Recommendation**: Option B (Table module) - resets all players at once, cleaner for new hand setup.

### Step 2: Implement the Function
**Location**: `src/lib/table.ml`

```ocaml
let reset_all_folded (t : t) : t =
  let new_players = List.map t.players ~f:(fun p -> { p with folded = false }) in
  { t with players = new_players }
```

**Also add to `table.mli`:**
```ocaml
val reset_all_folded : t -> t
(** [reset_all_folded table] resets all players' folded status to false for a new hand. *)
```

### Step 3: Reset Folded Status in `handle_winner`
**Location**: `src/bin/poker.ml`, `handle_winner` function (around line 133)

**Current code:**
```ocaml
let current_players = Table.current_players game.table in
let next_players = List.map current_players ~f:(fun p ->
  if p.player_id = winner.player_id then winner_updated else p
) in
let new_table = Table.init next_players in
```

**Change to:**
```ocaml
let current_players = Table.current_players game.table in
let next_players = List.map current_players ~f:(fun p ->
  if p.player_id = winner.player_id then winner_updated else p
) in
let new_table = Table.init next_players in
let new_table_reset = Table.reset_all_folded new_table in  (* Reset folded status *)
let new_game = Game.init_game new_table_reset in  (* Use reset table *)
```

### Step 4: Reset Folded Status in Showdown Case
**Location**: `src/lib/game.ml`, `next_street` function, Showdown case (around line 193)

**Current code:**
```ocaml
| Card.Showdown ->
  let rotated_table = Table.rotate game.table in
  let fresh_deck = Deck.sorted_deck |> Deck.shuffle in
  let players = Table.current_players rotated_table in
  ...
```

**Change to:**
```ocaml
| Card.Showdown ->
  let rotated_table = Table.rotate game.table in
  let rotated_table_reset = Table.reset_all_folded rotated_table in  (* Reset folded status *)
  let fresh_deck = Deck.sorted_deck |> Deck.shuffle in
  let players = Table.current_players rotated_table_reset in  (* Use reset table *)
  ...
```

### Step 5: Alternative Approach (Simpler)
Instead of creating a new function, we could reset folded status inline:

**In `handle_winner`:**
```ocaml
let next_players = List.map current_players ~f:(fun p ->
  let p_reset = { p with folded = false } in  (* Reset folded *)
  if p_reset.player_id = winner.player_id then winner_updated else p_reset
) in
```

**In `next_street` Showdown case:**
```ocaml
let players = Table.current_players rotated_table in
let players_reset = List.map players ~f:(fun p -> { p with folded = false }) in
let (deck_after_deal, players_with_cards) =
  List.fold_map players_reset ~init:fresh_deck ~f:(...)
```

**Recommendation**: Use the helper function (Steps 1-4) - cleaner, more maintainable, and reusable.

## Summary

**Files to Modify:**
1. `src/lib/table.ml` - Add `reset_all_folded` function
2. `src/lib/table.mli` - Add function signature
3. `src/bin/poker.ml` - Reset folded status in `handle_winner`
4. `src/lib/game.ml` - Reset folded status in Showdown case of `next_street`

**Key Changes:**
- Reset `folded = false` for all players when starting a new hand
- This ensures all players can participate in the new hand
- The game loop will properly include all players in betting

**Testing:**
- Test that a player who folds can participate in the next hand
- Test that all players are active at the start of a new hand
- Test that the game loop properly prompts for actions from all players

