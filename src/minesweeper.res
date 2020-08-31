open Utils

module List = Belt.List
module Map = Belt.Map
module Set = Belt.Set

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

module FieldsComparator = Belt.Id.MakeComparable({
  type t = field
  let cmp = ({x: x0, y: y0}: t, {x: x1, y: y1}: t) =>
    switch Pervasives.compare(x0, x1) {
    | 0 => Pervasives.compare(y0, y1)
    | c => c
    }
})

module FieldsMap = {
  type t<'value> = Map.t<field, 'value, FieldsComparator.identity>

  let empty: t<'value> = Map.make(~id=module(FieldsComparator))
}

module FieldsSet = {
  type t = Set.t<FieldsComparator.t, FieldsComparator.identity>

  let empty: t = Set.make(~id=module(FieldsComparator))

  let fromList = (input: list<FieldsComparator.t>): t =>
    input->List.toArray->Set.mergeMany(empty, _)
}

type state = {
  width: int,
  height: int,
  fields: FieldsMap.t<fieldState>,
}

type action =
  | Init(state)
  | Reveal(field)
  | ToggleMarker(field)

let neighbourDiff =
  cartesian(list{-1, 0, 1}, list{-1, 0, 1})->List.keep(((x, y)) => x != 0 || y != 0)

if List.length(neighbourDiff) != 8 {
  failwith("nighbourDiff should contain exactly 8 items")
}

let gameWidthSelector = (state: state) => state.width

let gameHeightSelector = (state: state) => state.height

let fieldNeighboursSelector = (state: state, field: field): list<field> => {
  let {width, height} = state
  let {x, y} = field
  neighbourDiff
  ->List.map(((dx, dy)) => {x: x + dx, y: y + dy})
  ->List.keep(({x, y}) => x >= 0 && (x < width && (y >= 0 && y < height)))
}

let fieldsSelector = (state: state): list<(field, fieldState)> => Map.toList(state.fields)

let fieldStateSelector = (state: state, field: field): fieldState => Map.getExn(state.fields, field)

let adjacentMinesCountSelector = (state: state, field: field): int =>
  fieldNeighboursSelector(state, field)
  ->List.map(neighbour => fieldStateSelector(state, neighbour))
  ->List.keep(({contents}) => contents == Mine)
  ->List.length

let minesCountSelector = (state: state): int =>
  state.fields->Map.keep((_, {contents}) => contents == Mine)->Map.size

let markedCountSelector = (state: state): int =>
  state.fields->Map.keep((_, {visibility}) => visibility == Marked)->Map.size

let revealedCountSelector = (state: state): int =>
  state.fields->Map.keep((_, {visibility}) => visibility == Revealed)->Map.size

let remainingMinesCountSelector = (state: state): int => {
  let mines = minesCountSelector(state)
  let marked = markedCountSelector(state)
  mines - marked
}

let gameStatusSelector = (state: state): gameStatus => {
  let fields = state.fields
  let exploded = Map.some(fields, (_, fieldState) =>
    switch fieldState {
    | {contents: Mine, visibility: Revealed} => true
    | _ => false
    }
  )
  let safeRemaining = Map.some(fields, (_, fieldState) =>
    switch fieldState {
    | {contents: Safe, visibility: Hidden} => true
    | _ => false
    }
  )
  switch (exploded, safeRemaining) {
  | (false, false) => Won
  | (true, _) => Lost
  | (_, true) => Playing
  }
}

let makeStateWithFieldsState = (
  ~width,
  ~height,
  ~fieldsWithState: list<(field, fieldState)>,
): state => {
  let add2 = (map, (key, value)) => Map.set(map, key, value)
  let fieldsWithStateMap = List.reduce(fieldsWithState, FieldsMap.empty, add2)
  assert (Map.size(fieldsWithStateMap) === width * height)
  {width: width, height: height, fields: fieldsWithStateMap}
}

let makeStateWithFieldsContents = (
  ~width,
  ~height,
  ~fieldsWithContents: list<(field, fieldContents)>,
): state => {
  let fieldsWithState = List.map(fieldsWithContents, ((field, contents)) => (
    field,
    {contents: contents, visibility: Hidden},
  ))
  makeStateWithFieldsState(~width, ~height, ~fieldsWithState)
}

let initializeState = (~width: int, ~height: int, ~mines: int): state => {
  let xs = range(0, width)
  let ys = range(0, height)
  let fields = cartesian(xs, ys)->List.map(((x, y)) => {x: x, y: y})
  if List.length(fields) <= mines {
    failwith("Too many mines for the board")
  }
  let minedFields = fields->shuffle |> take(mines) |> FieldsSet.fromList
  let makeFieldWithContents = field => {
    let contents = Set.has(minedFields, field) ? Mine : Safe
    (field, contents)
  }
  let fieldsWithContents = List.map(fields, makeFieldWithContents)
  makeStateWithFieldsContents(~width, ~height, ~fieldsWithContents)
}

let rec reinitializeStateWithSafeField = (
  ~state: state,
  ~width: int,
  ~height: int,
  ~mines: int,
  ~safeField: field,
): state => {
  let {contents: revealingContents} = fieldStateSelector(state, safeField)
  switch revealingContents {
  | Safe => state
  | Mine =>
    let newState = initializeState(~width, ~height, ~mines)
    reinitializeStateWithSafeField(~state=newState, ~width, ~height, ~mines, ~safeField)
  }
}

let rec accumulateFieldsToReveal = (state, field, acc) => {
  let {contents} = fieldStateSelector(state, field)
  let mines = adjacentMinesCountSelector(state, field)
  let accWithFieldRevealed = Set.add(acc, field)
  switch (mines, contents) {
  | (0, Safe) =>
    let visitNeighbour = (acc, neighbour) =>
      Set.has(acc, neighbour) ? acc : accumulateFieldsToReveal(state, neighbour, acc)
    fieldNeighboursSelector(state, field)->List.reduce(accWithFieldRevealed, visitNeighbour)
  | (_, Safe | Mine) => accWithFieldRevealed
  }
}

let fieldsToReveal = (state, field) => accumulateFieldsToReveal(state, field, FieldsSet.empty)

let revealFields = (state, toReveal) => Map.mapWithKey(state.fields, (field, fieldState) => {
    let shouldReveal = Set.has(toReveal, field)
    switch fieldState {
    | fieldState when shouldReveal => {...fieldState, visibility: Revealed}
    | fieldState => fieldState
    }
  })

let isPlaying = state => gameStatusSelector(state) == Playing

let update = (action, state) =>
  switch action {
  | Init(state) => state
  | Reveal(field) when isPlaying(state) =>
    /**
     * First reveal must not be a mine. Create new fields if necessary.
     */
    let firstReveal = revealedCountSelector(state) == 0
    let state = switch firstReveal {
    | false => state
    | true =>
      let mines = minesCountSelector(state)
      reinitializeStateWithSafeField(
        ~state,
        ~width=state.width,
        ~height=state.height,
        ~mines,
        ~safeField=field,
      )
    }

    /**
     * Proceed with the reveal
     */
    let fieldState = fieldStateSelector(state, field)
    switch fieldState {
    | {visibility: Revealed} =>
      /*
       * Revealing already revealed field reveals all its neighbours
       * if enough fields are marked around it
       */
      let neighbours = fieldNeighboursSelector(state, field)
      let (markedNeighbours, nonMarkedNeighbours) = List.partition(neighbours, neighbour =>
        switch fieldStateSelector(state, neighbour) {
        | {visibility: Marked} => true
        | _ => false
        }
      )
      let mines = adjacentMinesCountSelector(state, field)
      if List.length(markedNeighbours) == mines {
        let toReveal =
          nonMarkedNeighbours
          ->List.map(fieldsToReveal(state))
          ->List.reduce(FieldsSet.empty, Set.union)
        let fields = revealFields(state, toReveal)
        {...state, fields: fields}
      } else {
        state
      }
    | {visibility: Hidden | Marked} =>
      let toReveal = fieldsToReveal(state, field)
      let fields = revealFields(state, toReveal)
      {...state, fields: fields}
    }
  | ToggleMarker(field) when isPlaying(state) =>
    let fieldState = fieldStateSelector(state, field)
    let newFieldState = switch fieldState {
    | {visibility: Hidden} => Some({...fieldState, visibility: Marked})
    | {visibility: Marked} => Some({...fieldState, visibility: Hidden})
    | {visibility: Revealed} => None
    }
    switch newFieldState {
    | Some(fieldState) =>
      let fields = Map.set(state.fields, field, fieldState)
      {...state, fields: fields}
    | None => state
    }
  | Reveal(_field) | ToggleMarker(_field) => state
  }
