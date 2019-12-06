open Utils;

module Field = {
  [@react.component]
  let make =
      (
        ~mines: int,
        ~field: Game.field,
        ~fieldState: Game.fieldState,
        ~onClick: Game.field => unit,
        ~onDoubleClick: Game.field => unit,
      ) => {
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
           <button type_="button"> {React.string(buttonContent)} </button>
         </div>
    </Double_click>;
  };

  let arePropsEqual = (oldProps, newProps) => {
    let oldState: Game.fieldState = oldProps##fieldState;
    let newState: Game.fieldState = newProps##fieldState;
    oldState === newState;
  };

  let make = React.memoCustomCompareProps(make, arePropsEqual);
};

module Game = {
  type action = Game.action;

  type state = Game.state;

  let reducer = (state, action) => Game.update(action, state);

  [@react.component]
  let make = (~width: int, ~height: int, ~mines: int) => {
    let (state, send) =
      React.useReducerWithMapState(reducer, (), () =>
        Game.initializeState(~width, ~height, ~mines)
      );
    let width = Game.gameWidthSelector(state);
    let height = Game.gameHeightSelector(state);
    let xs = range(0, width);
    let ys = range(0, height);
    let gameStatus = Game.gameStatusSelector(state);
    let rows =
      React.array @@
      List.toArray @@
      List.map(ys, y =>
        <div className="game__board-row" key={string_of_int(y)}>
          {React.array @@
           List.toArray @@
           List.map(
             xs,
             x => {
               let field: Game.field = {x, y};
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
           )}
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
            {React.string(string_of_int(remainingMines))}
          </div>
          <button
            type_="button"
            className="game__start-button"
            onClick=startButtonClick>
            {React.string(buttonContents)}
          </button>
        </div>
        <div className="game__board"> rows </div>
      </div>
      <p className="instructions">
        {React.string(
           "double-click to reveal a field / click to mark a field",
         )}
      </p>
    </section>;
  };
};
