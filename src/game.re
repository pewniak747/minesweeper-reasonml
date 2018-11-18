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

module List = Belt.List;
module Map = Belt.Map;
module Set = Belt.Set;

module FieldsComparator =
  Belt.Id.MakeComparable({
    type t = field;
    let cmp = ((x0, y0): t, (x1, y1): t) =>
      switch (Pervasives.compare(x0, x1)) {
      | 0 => Pervasives.compare(y0, y1)
      | c => c
      };
  });

module FieldsMap = {
  type t('value) = Map.t(field, 'value, FieldsComparator.identity);

  let make = (): t('value) => Map.make(~id=(module FieldsComparator));
};

module FieldsSet = {
  type t = Set.t(FieldsComparator.t, FieldsComparator.identity);

  let make = (): t => Set.make(~id=(module FieldsComparator));

  let fromList = (input: list(FieldsComparator.t)): t => {
    let empty = make();
    input |> List.toArray |> Set.mergeMany(empty);
  };
};

type state = {
  width: int,
  height: int,
  fields: FieldsMap.t(fieldData),
};

type action =
  | Init(state)
  | Reveal(field)
  | ToggleMarker(field);

/* Selectors - computing values based on game state */
let neighbourDiff =
  cartesian([(-1), 0, 1], [(-1), 0, 1])
  |> List.keep(_, ((x, y)) => x != 0 || y != 0);

if (List.length(neighbourDiff) != 8) {
  failwith("nighbourDiff should contain exactly 8 items");
};

let fieldNeighboursSelector = (state, field) => {
  let {width, height} = state;
  let (x, y) = field;
  neighbourDiff
  |> List.map(_, ((dx, dy)) => (x + dx, y + dy))
  |> List.keep(_, ((x, y)) => x >= 0 && x < width && y >= 0 && y < height);
};

let adjacentMinesSelector = (state, field) =>
  fieldNeighboursSelector(state, field)
  |> List.map(_, field => Map.getExn(state.fields, field))
  |> List.keep(_, ((contents, _)) => contents == Mine)
  |> List.length;

let minesCountSelector = state =>
  state.fields
  |> Map.keep(_, (_, (contents, _)) => contents == Mine)
  |> Map.size;

let markedCountSelector = state =>
  state.fields
  |> Map.keep(_, (_, (_, visibility)) => visibility == Marked)
  |> Map.size;

let revealedCountSelector = state =>
  state.fields
  |> Map.keep(_, (_, (_, visibility)) => visibility == Revealed)
  |> Map.size;

let gameStatusSelector = state => {
  let fields = state.fields;
  let exploded =
    Map.some(fields, (_, data) =>
      switch (data) {
      | (Mine, Revealed) => true
      | _ => false
      }
    );
  let safeRemaining =
    Map.some(fields, (_, data) =>
      switch (data) {
      | (Safe, Hidden) => true
      | _ => false
      }
    );
  switch (exploded, safeRemaining) {
  | (false, false) => Won
  | (true, _) => Lost
  | (_, true) => Playing
  };
};

let remainingMinesSelector = state => {
  let mines = minesCountSelector(state);
  let marked = markedCountSelector(state);
  mines - marked;
};

let add2 = (map, (key, value)) => Map.set(map, key, value);

/* Game actions logic */
let initializeState = (~width=10, ~height=8, ~mines=5, ()) => {
  let xs = range(0, width);
  let ys = range(0, height);
  let fields = cartesian(xs, ys);
  if (List.length(fields) <= mines) {
    failwith("Too many mines for the board");
  };
  let minedFields = fields |> shuffle |> take(mines) |> FieldsSet.fromList;
  let makeFieldWithData = field => {
    let contents = Set.has(minedFields, field) ? Mine : Safe;
    let data = (contents, Hidden);
    (field, data);
  };
  let fieldsWithData =
    fields
    |> List.map(_, makeFieldWithData)
    |> List.reduce(_, FieldsMap.make(), add2);
  {width, height, fields: fieldsWithData};
};

let rec reinitializeStateWithSafeField =
        (
          ~state: state,
          ~width: int,
          ~height: int,
          ~mines: int,
          ~safeField: field,
          (),
        ) => {
  let revealingMine =
    state.fields
    |> Map.getExn(_, safeField)
    |> (((contents, _visibility)) => contents == Mine);
  switch (revealingMine) {
  | false => state
  | true =>
    let newState = initializeState(~width, ~height, ~mines, ());
    reinitializeStateWithSafeField(
      ~state=newState,
      ~width,
      ~height,
      ~mines,
      ~safeField,
      (),
    );
  };
};

let rec accumulateFieldsToReveal = (state, field, acc) => {
  let mines = adjacentMinesSelector(state, field);
  let (contents, _) = Map.getExn(state.fields, field);
  let accWithFieldRevealed = Set.add(acc, field);
  switch (mines, contents) {
  | (0, Safe) =>
    let visitNeighbour = (acc, neighbour) =>
      Set.has(acc, neighbour) ?
        acc : accumulateFieldsToReveal(state, neighbour, acc);
    fieldNeighboursSelector(state, field)
    |> List.reduce(_, accWithFieldRevealed, visitNeighbour);
  | (_, Safe | Mine) => accWithFieldRevealed
  };
};

let fieldsToReveal = (state, field) =>
  accumulateFieldsToReveal(state, field, FieldsSet.make());

let revealFields = (state, toReveal) =>
  Map.mapWithKey(
    state.fields,
    (field, data) => {
      let shouldReveal = Set.has(toReveal, field);
      switch (data) {
      | (contents, _) when shouldReveal => (contents, Revealed)
      | data => data
      };
    },
  );

let isPlaying = state => gameStatusSelector(state) == Playing;

let reducer = (action, state) =>
  switch (action) {
  | Init(state) => ReasonReact.Update(state)
  | Reveal(field) when isPlaying(state) =>
    /**
     * First reveal must not be a mine. Create new fields if necessary.
     */
    let firstReveal = revealedCountSelector(state) == 0;
    let state =
      switch (firstReveal) {
      | false => state
      | true =>
        let mines = minesCountSelector(state);
        reinitializeStateWithSafeField(
          ~state,
          ~width=state.width,
          ~height=state.height,
          ~mines,
          ~safeField=field,
          (),
        );
      };

    /**
     * Proceed with the reveal
     */
    let data = Map.getExn(state.fields, field);
    switch (data) {
    | (_contents, Revealed) =>
      /*
       * Revealing already revealed field reveals all its neighbours
       * if enough fields are marked around it
       */
      let neighbours = fieldNeighboursSelector(state, field);
      let (markedNeighbours, nonMarkedNeighbours) =
        List.partition(neighbours, neighbour =>
          switch (Map.getExn(state.fields, neighbour)) {
          | (_, Marked) => true
          | _ => false
          }
        );
      let mines = adjacentMinesSelector(state, field);
      if (List.length(markedNeighbours) == mines) {
        let toReveal =
          nonMarkedNeighbours
          |> List.map(_, fieldsToReveal(state))
          |> List.reduce(_, FieldsSet.make(), Set.union);
        let fields = revealFields(state, toReveal);
        ReasonReact.Update({...state, fields});
      } else {
        ReasonReact.NoUpdate;
      };
    | (_contents, Hidden | Marked) =>
      let toReveal = fieldsToReveal(state, field);
      let fields = revealFields(state, toReveal);
      ReasonReact.Update({...state, fields});
    };
  | ToggleMarker(field) when isPlaying(state) =>
    let data = Map.getExn(state.fields, field);
    let newData =
      switch (data) {
      | (contents, Hidden) => Some((contents, Marked))
      | (contents, Marked) => Some((contents, Hidden))
      | (_contents, Revealed) => None
      };
    switch (newData) {
    | Some(data) =>
      let fields = Map.set(state.fields, field, data);
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
      data: data,
    },
    shouldUpdate: ({oldSelf, newSelf}) =>
      oldSelf.retainedProps.data !== newSelf.retainedProps.data,
    render: _self => {
      let buttonContent =
        switch (data) {
        | (_, Hidden) => ""
        | (_, Marked) => {js|🚩|js}
        | (Safe, Revealed) => mines |> string_of_int
        | (Mine, Revealed) => {js|💥|js}
        };
      let baseClassName = "game__board-field";
      let revealedClassName =
        switch (data) {
        | (_, Revealed) => {j|$baseClassName--revealed|j}
        | _ => ""
        };
      let minesClassName =
        switch (data) {
        | (Safe, Revealed) => {j|$baseClassName--$mines|j}
        | _ => ""
        };
      let explosionClassName =
        switch (data) {
        | (Mine, Revealed) => {j|$baseClassName--exploded|j}
        | _ => ""
        };
      let className =
        Cn.make([
          baseClassName,
          revealedClassName,
          minesClassName,
          explosionClassName,
        ]);
      let onClick = _evt => onClick(field);
      let onDoubleClick = _event => onDoubleClick(field);
      <Double_click onClick onDoubleClick>
        ...<div className>
             <button type_="button"> {str(buttonContent)} </button>
           </div>
      </Double_click>;
    },
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
      List.toArray @@
      List.map(ys, y =>
        <div className="game__board-row" key={string_of_int(y)}>
          {
            arr @@
            List.toArray @@
            List.map(
              xs,
              x => {
                let field = (x, y);
                let data = Map.getExn(state.fields, field);
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
                  key={string_of_int(x)}
                />;
              },
            )
          }
        </div>
      );
    let buttonContents =
      switch (gameStatus) {
      | Playing => {js|🙂|js}
      | Won => {js|😎|js}
      | Lost => {js|😵|js}
      };
    let startButtonClick = _evt =>
      send(
        Init(
          initializeState(
            ~width=state.width,
            ~height=state.height,
            ~mines,
            (),
          ),
        ),
      );
    let remainingMines = remainingMinesSelector(state);
    <section className="game__wrapper">
      <div className="game">
        <div className="game__header">
          <div className="game__remaining-mines">
            {str(remainingMines |> string_of_int)}
          </div>
          <button
            type_="button"
            className="game__start-button"
            onClick=startButtonClick>
            {str(buttonContents)}
          </button>
        </div>
        <div className="game__board"> rows </div>
      </div>
      <p className="instructions">
        {str("double-click to reveal a field / click to mark a field")}
      </p>
    </section>;
  },
};
