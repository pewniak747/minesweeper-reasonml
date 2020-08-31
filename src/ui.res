open Utils

module Field = {
  type display =
    | Hidden
    | Marked
    | Safe({mines: int})
    | Exploded

  @react.component
  let make = (
    ~mines: int,
    ~field: Game.field,
    ~fieldState: Game.fieldState,
    ~onClick: Game.field => unit,
    ~onDoubleClick: Game.field => unit,
  ) => {
    let display = switch fieldState {
    | {visibility: Hidden} => Hidden
    | {visibility: Marked} => Marked
    | {visibility: Revealed, contents: Safe} => Safe({mines: mines})
    | {visibility: Revealed, contents: Mine} => Exploded
    }
    let buttonContent = switch display {
    | Hidden => React.null
    | Marked => React.string(`🚩`)
    | Safe({mines}) => React.int(mines)
    | Exploded => React.string(`💥`)
    }
    let baseClassName = "game__board-field"
    let revealedClassName = switch display {
    | Safe(_) | Exploded => j`$baseClassName--revealed`
    | Hidden | Marked => ""
    }
    let contentClassName = switch display {
    | Safe({mines}) => j`$baseClassName--$mines`
    | Exploded => j`$baseClassName--exploded`
    | Hidden | Marked => ""
    }
    let className = Cn.make(list{baseClassName, revealedClassName, contentClassName})
    let onClick = _evt => onClick(field)
    let onDoubleClick = _event => onDoubleClick(field)
    <Double_click onClick onDoubleClick>
      <div className> <button type_="button"> buttonContent </button> </div>
    </Double_click>
  }

  let arePropsEqual = (oldProps, newProps) => {
    let oldState: Game.fieldState = oldProps["fieldState"]
    let newState: Game.fieldState = newProps["fieldState"]
    oldState === newState
  }

  let make = React.memoCustomCompareProps(make, arePropsEqual)
}

module Game = {
  type action = Game.action

  type state = Game.state

  let reducer = (state, action) => Game.update(action, state)

  @react.component
  let make = (~width: int, ~height: int, ~mines: int) => {
    let (state, send) = React.useReducerWithMapState(reducer, (), () =>
      Game.initializeState(~width, ~height, ~mines)
    )
    let width = Game.gameWidthSelector(state)
    let height = Game.gameHeightSelector(state)
    let xs = range(0, width)
    let ys = range(0, height)
    let gameStatus = Game.gameStatusSelector(state)
    let rows =
      List.map(ys, y =>
        <div className="game__board-row" key={string_of_int(y)}> {List.map(xs, x => {
            let field: Game.field = {x: x, y: y}
            let fieldState = Game.fieldStateSelector(state, field)
            let displayedFieldState: Game.fieldState = switch (gameStatus, fieldState) {
            | (Won, {contents: Mine}) => {
                contents: Mine,
                visibility: Marked,
              }
            | (Won, {contents: Safe} as fieldState) | (Playing | Lost, fieldState) => fieldState
            }
            let onClick = field => send(ToggleMarker(field))
            let onDoubleClick = field => send(Reveal(field))
            let mines = Game.adjacentMinesCountSelector(state, field)
            <Field
              field
              mines
              fieldState=displayedFieldState
              onClick
              onDoubleClick
              key={string_of_int(x)}
            />
          })->List.toArray->React.array} </div>
      )
      ->List.toArray
      ->React.array
    let buttonContents = switch gameStatus {
    | Playing => `🙂`
    | Won => `😎`
    | Lost => `😵`
    }
    let startButtonClick = _evt => send(Init(Game.initializeState(~width, ~height, ~mines)))
    let remainingMines = Game.remainingMinesCountSelector(state)
    <section className="game__wrapper">
      <div className="game">
        <div className="game__header">
          <div className="game__remaining-mines"> {React.int(remainingMines)} </div>
          <button type_="button" className="game__start-button" onClick=startButtonClick>
            {React.string(buttonContents)}
          </button>
        </div>
        <div className="game__board"> rows </div>
      </div>
      <p className="instructions">
        {React.string("double-click to reveal a field / click to mark a field")}
      </p>
    </section>
  }
}
