open Utils;

module Field = {
  type retainedProps = {fieldState: Game.fieldState};

  let component = ReasonReact.statelessComponentWithRetainedProps("Field");

  let make =
      (
        ~mines: int,
        ~field: Game.field,
        ~fieldState: Game.fieldState,
        ~onClick: Game.field => unit,
        ~onDoubleClick: Game.field => unit,
        _children,
      ) => {
    ...component,
    retainedProps: {
      fieldState: fieldState,
    },
    shouldUpdate: ({oldSelf, newSelf}) =>
      oldSelf.retainedProps.fieldState !== newSelf.retainedProps.fieldState,
    render: _self => {
      let buttonContent =
        switch (fieldState) {
        | (_, Hidden) => ""
        | (_, Marked) => {js|🚩|js}
        | (Safe, Revealed) => mines |> string_of_int
        | (Mine, Revealed) => {js|💥|js}
        };
      let baseClassName = "game__board-field";
      let revealedClassName =
        switch (fieldState) {
        | (_, Revealed) => {j|$baseClassName--revealed|j}
        | _ => ""
        };
      let minesClassName =
        switch (fieldState) {
        | (Safe, Revealed) => {j|$baseClassName--$mines|j}
        | _ => ""
        };
      let explosionClassName =
        switch (fieldState) {
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

module Game = {
  type action = Game.action;

  type state = Game.state;

  let component = ReasonReact.reducerComponent("Game");

  let make = (~width: int, ~height: int, ~mines: int, _children) => {
    ...component,
    initialState: () => Game.initializeState(~width, ~height, ~mines),
    reducer: (action, state) => {
      let newState = Game.update(action, state);
      state === newState ?
        ReasonReact.NoUpdate : ReasonReact.Update(newState);
    },
    render: self => {
      let state: state = self.state;
      let send: action => unit = self.send;
      let width = Game.gameWidthSelector(state);
      let height = Game.gameHeightSelector(state);
      let xs = range(0, width);
      let ys = range(0, height);
      let gameStatus = Game.gameStatusSelector(state);
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
                  let fieldState = Game.fieldStateSelector(state, field);
                  let displayedFieldState: Game.fieldState =
                    switch (gameStatus, fieldState) {
                    | (Won, (Mine, _)) => (Mine, Marked)
                    | (_, fieldState) => fieldState
                    };
                  let onClick = field => send(ToggleMarker(field));
                  let onDoubleClick = field => send(Reveal(field));
                  let mines = Game.adjacentMinesCountSelector(state, field);
                  <Field
                    field
                    mines
                    fieldState=displayedFieldState
                    onClick
                    onDoubleClick
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
        send(Init(Game.initializeState(~width, ~height, ~mines)));
      let remainingMines = Game.remainingMinesCountSelector(state);
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
};