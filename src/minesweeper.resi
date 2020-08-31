/**
 * This module implements the game of Minesweeper - inspired by a classic Windows game.
 *
 * The idea is to represent the current game state by a value of type [state].
 * This value is immutable - in order to obtain a new state, one needs to use the
 * [update] function that accepts a game action and old state, and returns a new state.
 * The [selector] functions can be used to obtain more information about the game state,
 * like whether the game is already won, or how many mines are adjacent to a given field.
 */

/**
 * CORE TYPES
 */

/**
 * Represents field coordinates on a game board.
 */
type field = {
  x: int,
  y: int,
}

type fieldVisibility =
  | Hidden
  | Revealed
  | Marked

type fieldContents =
  | Mine
  | Safe

type fieldState = {
  contents: fieldContents,
  visibility: fieldVisibility,
}

type gameStatus =
  | Playing
  | Won
  | Lost

/**
 * GAME STATE
 *
 * The current game state is an abstract type in order to avoid leaking
 * of the implementation details.
 */
type state

/**
 * GAME ACTIONS
 */
type action =
  | Init(state)
  | Reveal(field)
  | ToggleMarker(field)

/**
 * Returns a game state with given width, height and a defined number
 * of mines scattered randomly.
 */
let initializeState: (~width: int, ~height: int, ~mines: int) => state

/**
 * Returns a game state with all fields in defined states (not random).
 * This is useful in tests, when needing to recreate a specific game state.
 */
let makeStateWithFieldsState: (
  ~width: int,
  ~height: int,
  ~fieldsWithState: list<(field, fieldState)>,
) => state

/**
 * SELECTORS
 */
let gameWidthSelector: state => int

let gameHeightSelector: state => int

let gameStatusSelector: state => gameStatus

let fieldsSelector: state => list<(field, fieldState)>

let fieldStateSelector: (state, field) => fieldState

let adjacentMinesCountSelector: (state, field) => int

let remainingMinesCountSelector: state => int

/**
 * UPDATE FUNCTION
 */
let update: (action, state) => state
