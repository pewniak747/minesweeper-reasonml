let str = ReasonReact.stringToElement;

let arr = ReasonReact.arrayToElement;

type field = (int, int);

type fieldVisibility =
  | Hidden
  | Revealed;

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
  | Reveal(field);

let rec range = (start: int, end_: int) =>
  if (start >= end_) {
    [];
  } else {
    [start, ...range(start + 1, end_)];
  };

let cartesian = (l1, l2) =>
  List.concat(List.map(e => List.map(e' => (e, e'), l2), l1));

let compareFst = ((fst1: int, _), (fst2: int, _)) =>
  Pervasives.compare(fst1, fst2);

let shuffle = d => {
  let nd = List.map(c => (Random.bits(), c), d);
  let sond = List.sort(compareFst, nd);
  List.map(snd, sond);
};

let rec take = (n, lst) =>
  switch n {
  | 0 => []
  | n => [List.hd(lst), ...take(n - 1, List.tl(lst))]
  };

let initializeState = (~width=10, ~height=8, ~mines=20, ()) => {
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

let neighbourDiff =
  cartesian([(-1), 0, 1], [(-1), 0, 1])
  |> List.filter(e => ! (fst(e) == 0 && snd(e) == 0));

if (List.length(neighbourDiff) != 8) {
  failwith("nighbourDiff should contain exactly 8 items");
};

let adjacentMinesSelector = (state, field) => {
  let {width, height, fields} = state;
  let (x, y) = field;
  let neighbourCandidates =
    List.map(((dx, dy)) => (x + dx, y + dy), neighbourDiff);
  let neighbours =
    List.filter(
      ((x, y)) => x >= 0 && x < width && y >= 0 && y < height,
      neighbourCandidates
    );
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

let component = ReasonReact.reducerComponent("Game");

let make = _children => {
  ...component,
  initialState: initializeState,
  reducer: (action, state) =>
    switch action {
    | Init(state) => ReasonReact.Update(state)
    | Reveal(field) =>
      switch (gameStatusSelector(state)) {
      | Playing =>
        let data = FieldsMap.find(field, state.fields);
        let newData =
          switch data {
          | (contents, _) => (contents, Revealed)
          };
        let fields = FieldsMap.add(field, newData, state.fields);
        ReasonReact.Update({...state, fields});
      | Won => ReasonReact.NoUpdate
      | Lost => ReasonReact.NoUpdate
      }
    },
  render: ({state, send}) => {
    let xs = range(0, state.width);
    let ys = range(0, state.height);
    let gameStatus = gameStatusSelector(state);
    let rows =
      Array.of_list @@
      List.map(
        y =>
          <div className="game__board-row" key=(string_of_int(y))>
            (
              arr(
                Array.of_list @@
                List.map(
                  x => {
                    let field = (x, y);
                    switch (FieldsMap.find(field, state.fields)) {
                    | exception Not_found => ReasonReact.nullElement
                    | (contents, visibility) =>
                      let buttonContent =
                        switch (contents, visibility) {
                        | (_, Hidden) => ""
                        | (Safe, Revealed) =>
                          adjacentMinesSelector(state, field) |> string_of_int
                        | (Mine, Revealed) => {js|💥|js}
                        };
                      let onClick = _event => send(Reveal(field));
                      let className =
                        Cn.make([
                          "game__board-field",
                          "game__board-field--revealed"
                          |> Cn.ifBool(visibility == Revealed),
                          "game__board-field--"
                          ++ string_of_int(
                               adjacentMinesSelector(state, field)
                             )
                          |> Cn.ifBool(
                               visibility == Revealed && contents == Safe
                             )
                        ]);
                      <div className key=(string_of_int(x))>
                        <button _type="button" onClick>
                          (str(buttonContent))
                        </button>
                      </div>;
                    };
                  },
                  xs
                )
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
      <div className="game__board"> (arr(rows)) </div>
    </div>;
  }
};
