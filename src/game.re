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
  |> List.filter(e => ! (fst(e) == 0 && snd(e) == 0));

if (List.length(neighbourDiff) != 8) {
  failwith("nighbourDiff should contain exactly 8 items");
};

let fieldNeighboursSelector = (state, field) => {
  let {width, height} = state;
  let (x, y) = field;
  let neighbourCandidates =
    List.map(((dx, dy)) => (x + dx, y + dy), neighbourDiff);
  List.filter(
    ((x, y)) => x >= 0 && x < width && y >= 0 && y < height,
    neighbourCandidates
  );
};

let adjacentMinesSelector = (state, field) => {
  let {fields} = state;
  let neighbours = fieldNeighboursSelector(state, field);
  let neighbourData = List.map(f => FieldsMap.find(f, fields), neighbours);
  let minedNeighbours =
    List.filter(((contents, _)) => contents == Mine, neighbourData);
  List.length(minedNeighbours);
};

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
  let playing =
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
  } else if (playing) {
    Playing;
  } else {
    Won;
  };
};

/* Game actions logic */
let initializeState = (~width=10, ~height=8, ~mines=5, ()) => {
  let xs = range(0, width);
  let ys = range(0, height);
  let fields = cartesian(xs, ys);
  if (List.length(fields) <= mines) {
    failwith("Too many mines for the board");
  };
  let minedFields = fields |> shuffle |> take(mines) |> FieldsSet.of_list;
  let reduceFields = (acc, field) => {
    let contents = FieldsSet.mem(field, minedFields) ? Mine : Safe;
    let data = (contents, Hidden);
    FieldsMap.add(field, data, acc);
  };
  let fieldsWithData = List.fold_left(reduceFields, FieldsMap.empty, fields);
  {width, height, fields: fieldsWithData};
};

let rec accumulateFieldsToReveal = (state, field, acc) => {
  let mines = adjacentMinesSelector(state, field);
  let contents = fst(FieldsMap.find(field, state.fields));
  let accWithField = FieldsSet.add(field, acc);
  switch (mines, contents) {
  | (0, Safe) =>
    let neighbours = fieldNeighboursSelector(state, field);
    List.fold_left(
      (acc, neighbour) =>
        if (FieldsSet.mem(neighbour, acc)) {
          acc;
        } else {
          accumulateFieldsToReveal(state, neighbour, acc);
        },
      accWithField,
      neighbours
    );
  | _ => accWithField
  };
};

let onlyWhenPlaying = (state, callback) =>
  switch (gameStatusSelector(state)) {
  | Playing => callback()
  | Won
  | Lost => ReasonReact.NoUpdate
  };

let reducer = (action, state) =>
  switch action {
  | Init(state) => ReasonReact.Update(state)
  | Reveal(field) =>
    onlyWhenPlaying(state) @@
    (
      () => {
        let data = FieldsMap.find(field, state.fields);
        switch data {
        | (_, Revealed) => ReasonReact.NoUpdate
        | _ =>
          let toReveal =
            accumulateFieldsToReveal(state, field, FieldsSet.empty);
          let fields =
            FieldsMap.mapi(
              (field, data) => {
                let shouldReveal = FieldsSet.mem(field, toReveal);
                switch (shouldReveal, data) {
                | (true, (contents, _)) => (contents, Revealed)
                | (false, data) => data
                };
              },
              state.fields
            );
          ReasonReact.Update({...state, fields});
        };
      }
    )
  | ToggleMarker(field) =>
    onlyWhenPlaying(state) @@
    (
      () => {
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
      }
    )
  };

/* Game UI */
module Field = {
  let component = ReasonReact.statelessComponent("Field");
  let make = (~mines, ~data, ~field, ~onClick, ~onDoubleClick, _children) => {
    ...component,
    render: _self => {
      let buttonContent =
        switch data {
        | (_, Hidden) => ""
        | (_, Marked) => {js|🚩|js}
        | (Safe, Revealed) => mines |> string_of_int
        | (Mine, Revealed) => {js|💥|js}
        };
      let onClick = _evt => onClick(field);
      let onDoubleClick = _event => onDoubleClick(field);
      let (contents, visibility) = data;
      let className =
        Cn.make([
          "game__board-field",
          "game__board-field--revealed" |> Cn.ifBool(visibility == Revealed),
          "game__board-field--"
          ++ string_of_int(mines)
          |> Cn.ifBool(visibility == Revealed && contents == Safe)
        ]);
      <Double_click onClick onDoubleClick>
        ...<div className>
             <button _type="button"> (str(buttonContent)) </button>
           </div>
      </Double_click>;
    }
  };
};

let component = ReasonReact.reducerComponent("Game");

let make = _children => {
  ...component,
  initialState: initializeState,
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
                  let onClick = field => send(ToggleMarker(field));
                  let onDoubleClick = field => send(Reveal(field));
                  let mines = adjacentMinesSelector(state, field);
                  <Field
                    field
                    data
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
    let startButtonClick = _evt => send(Init(initializeState()));
    <div className="game">
      <div className="game__header">
        <button
          _type="button" className="start-button" onClick=startButtonClick>
          (str(buttonContents))
        </button>
      </div>
      <div className="game__board"> rows </div>
    </div>;
  }
};
