open Utils;

/* Types */
type field = (int, int);

type fieldVisibility =
  | Hidden
  | Revealed
  | Marked;

type fieldContents =
  | Mine
  | Safe;

type fieldData = (fieldContents, fieldVisibility);

type status =
  | Playing
  | Won
  | Lost;

module OrderedFields = {
  type t = field;
  let compare = ((x0, y0): t, (x1, y1): t) =>
    switch (Pervasives.compare(x0, x1)) {
    | 0 => Pervasives.compare(y0, y1)
    | c => c
    };
};

module FieldsMap = Map.Make(OrderedFields);

module FieldsSet = Set.Make(OrderedFields);

type state = {
  width: int,
  height: int,
  fields: FieldsMap.t(fieldData)
};

type action =
  | Init(state)
  | Reveal(field)
  | ToggleMarker(field);

/* Selectors - computing values based on game state */
let neighbourDiff =
  cartesian([(-1), 0, 1], [(-1), 0, 1])
  |> List.filter(((x, y)) => x != 0 || y != 0);

if (List.length(neighbourDiff) != 8) {
  failwith("nighbourDiff should contain exactly 8 items");
};

let fieldNeighboursSelector = (state, field) => {
  let {width, height} = state;
  let (x, y) = field;
  neighbourDiff
  |> List.map(((dx, dy)) => (x + dx, y + dy))
  |> List.filter(((x, y)) => x >= 0 && x < width && y >= 0 && y < height);
};

let adjacentMinesSelector = (state, field) =>
  fieldNeighboursSelector(state, field)
  |> List.map(field => FieldsMap.find(field, state.fields))
  |> List.filter(((contents, _)) => contents == Mine)
  |> List.length;

let gameStatusSelector = state => {
  let fields = state.fields;
  let exploded =
    FieldsMap.exists(
      (_, data) =>
        switch data {
        | (Mine, Revealed) => true
        | _ => false
        },
      fields
    );
  let safeRemaining =
    FieldsMap.exists(
      (_, data) =>
        switch data {
        | (Safe, Hidden) => true
        | _ => false
        },
      fields
    );
  if (exploded) {
    Lost;
  } else if (safeRemaining) {
    Playing;
  } else {
    Won;
  };
};

let remainingMinesSelector = state => {
  let mines =
    state.fields
    |> FieldsMap.filter((_, (contents, _)) => contents == Mine)
    |> FieldsMap.cardinal;
  let marked =
    state.fields
    |> FieldsMap.filter((_, (_, visibility)) => visibility == Marked)
    |> FieldsMap.cardinal;
  mines - marked;
};

let add2 = (map, (key, value)) => FieldsMap.add(key, value, map);

/* Game actions logic */
let initializeState = (~width=10, ~height=8, ~mines=5, ()) => {
  let xs = range(0, width);
  let ys = range(0, height);
  let fields = cartesian(xs, ys);
  if (List.length(fields) <= mines) {
    failwith("Too many mines for the board");
  };
  let minedFields = fields |> shuffle |> take(mines) |> FieldsSet.of_list;
  let makeFieldWithData = field => {
    let contents = FieldsSet.mem(field, minedFields) ? Mine : Safe;
    let data = (contents, Hidden);
    (field, data);
  };
  let fieldsWithData =
    fields
    |> List.map(makeFieldWithData)
    |> List.fold_left(add2, FieldsMap.empty);
  {width, height, fields: fieldsWithData};
};

let rec accumulateFieldsToReveal = (state, field, acc) => {
  let mines = adjacentMinesSelector(state, field);
  let contents = fst(FieldsMap.find(field, state.fields));
  let accWithField = FieldsSet.add(field, acc);
  switch (mines, contents) {
  | (0, Safe) =>
    let visitNeighbour = (acc, neighbour) =>
      FieldsSet.mem(neighbour, acc) ?
        acc : accumulateFieldsToReveal(state, neighbour, acc);
    fieldNeighboursSelector(state, field)
    |> List.fold_left(visitNeighbour, accWithField);
  | _ => accWithField
  };
};

let fieldsToReveal = (state, field) =>
  accumulateFieldsToReveal(state, field, FieldsSet.empty);

let revealFields = (state, toReveal) =>
  FieldsMap.mapi(
    (field, data) => {
      let shouldReveal = FieldsSet.mem(field, toReveal);
      switch data {
      | (contents, _) when shouldReveal => (contents, Revealed)
      | data => data
      };
    },
    state.fields
  );

let isPlaying = state => gameStatusSelector(state) == Playing;

let reducer = (action, state) =>
  switch action {
  | Init(state) => ReasonReact.Update(state)
  | Reveal(field) when isPlaying(state) =>
    let data = FieldsMap.find(field, state.fields);
    switch data {
    | (_, Revealed) =>
      let neighbours = fieldNeighboursSelector(state, field);
      let (markedNeighbours, nonMarkedNeighbours) =
        List.partition(
          neighbour =>
            switch (FieldsMap.find(neighbour, state.fields)) {
            | (_, Marked) => true
            | _ => false
            },
          neighbours
        );
      let mines = adjacentMinesSelector(state, field);
      switch (List.length(markedNeighbours)) {
      | x when x == mines =>
        let toReveal =
          List.map(fieldsToReveal(state), nonMarkedNeighbours)
          |> List.fold_left(FieldsSet.union, FieldsSet.empty);
        let fields = revealFields(state, toReveal);
        ReasonReact.Update({...state, fields});
      | _ => ReasonReact.NoUpdate
      };
    | _ =>
      let toReveal = fieldsToReveal(state, field);
      let fields = revealFields(state, toReveal);
      ReasonReact.Update({...state, fields});
    };
  | ToggleMarker(field) when isPlaying(state) =>
    let data = FieldsMap.find(field, state.fields);
    let newData =
      switch data {
      | (contents, Hidden) => Some((contents, Marked))
      | (contents, Marked) => Some((contents, Hidden))
      | _ => None
      };
    switch newData {
    | Some(data) =>
      let fields = FieldsMap.add(field, data, state.fields);
      ReasonReact.Update({...state, fields});
    | None => ReasonReact.NoUpdate
    };
  | Reveal(_field) => ReasonReact.NoUpdate
  | ToggleMarker(_field) => ReasonReact.NoUpdate
  };

/* Game UI */
module Field = {
  type retainedProps = {data: fieldData};
  let component = ReasonReact.statelessComponentWithRetainedProps("Field");
  let make = (~mines, ~data, ~field, ~onClick, ~onDoubleClick, _children) => {
    ...component,
    retainedProps: {
      data: data
    },
    shouldUpdate: ({oldSelf, newSelf}) =>
      oldSelf.retainedProps.data !== newSelf.retainedProps.data,
    render: _self => {
      let buttonContent =
        switch data {
        | (_, Hidden) => ""
        | (_, Marked) => {js|🚩|js}
        | (Safe, Revealed) => mines |> string_of_int
        | (Mine, Revealed) => {js|💥|js}
        };
      let baseClassName = "game__board-field";
      let revealedClassName =
        switch data {
        | (_, Revealed) => {j|$baseClassName--revealed|j}
        | _ => ""
        };
      let minesClassName =
        switch data {
        | (Safe, Revealed) => {j|$baseClassName--$mines|j}
        | _ => ""
        };
      let explosionClassName =
        switch data {
        | (Mine, Revealed) => {j|$baseClassName--exploded|j}
        | _ => ""
        };
      let className =
        Cn.make([
          baseClassName,
          revealedClassName,
          minesClassName,
          explosionClassName
        ]);
      let onClick = _evt => onClick(field);
      let onDoubleClick = _event => onDoubleClick(field);
      <Double_click onClick onDoubleClick>
        ...<div className>
             <button _type="button"> (str(buttonContent)) </button>
           </div>
      </Double_click>;
    }
  };
};

let component = ReasonReact.reducerComponent("Game");

let make = (~width: int, ~height: int, ~mines: int, _children) => {
  ...component,
  initialState: initializeState(~width, ~height, ~mines),
  reducer,
  render: ({state, send}) => {
    let xs = range(0, state.width);
    let ys = range(0, state.height);
    let gameStatus = gameStatusSelector(state);
    let rows =
      arr @@
      Array.of_list @@
      List.map(
        y =>
          <div className="game__board-row" key=(string_of_int(y))>
            (
              arr @@
              Array.of_list @@
              List.map(
                x => {
                  let field = (x, y);
                  let data = FieldsMap.find(field, state.fields);
                  let displayedData =
                    switch (gameStatus, data) {
                    | (Won, (Mine, _)) => (Mine, Marked)
                    | (_, data) => data
                    };
                  let onClick = field => send(ToggleMarker(field));
                  let onDoubleClick = field => send(Reveal(field));
                  let mines = adjacentMinesSelector(state, field);
                  <Field
                    field
                    data=displayedData
                    onClick
                    onDoubleClick
                    mines
                    key=(string_of_int(x))
                  />;
                },
                xs
              )
            )
          </div>,
        ys
      );
    let buttonContents =
      switch gameStatus {
      | Playing => {js|🙂|js}
      | Won => {js|😎|js}
      | Lost => {js|😵|js}
      };
    let startButtonClick = _evt =>
      send(
        Init(
          initializeState(~width=state.width, ~height=state.height, ~mines, ())
        )
      );
    let remainingMines = remainingMinesSelector(state);
    <section className="game__wrapper">
      <div className="game">
        <div className="game__header">
          <div className="game__remaining-mines">
            (str(remainingMines |> string_of_int))
          </div>
          <button
            _type="button"
            className="game__start-button"
            onClick=startButtonClick>
            (str(buttonContents))
          </button>
        </div>
        <div className="game__board"> rows </div>
      </div>
      <p className="instructions">
        (str("double-click to reveal a field / click to mark a field"))
      </p>
    </section>;
  }
};
