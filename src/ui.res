open Utils

module Double_click = {
  let thresholdMs = 200.0

  @react.component
  let make = (~onClick as onSingleClick, ~onDoubleClick, ~children) => {
    let lastClickAtRef = React.useRef(0.0)
    let timeoutIdRef = React.useRef(None)

    let onClick = evt => {
      ReactEvent.Synthetic.preventDefault(evt)
      let now = Js.Date.now()
      let lastClickAt = lastClickAtRef.React.current
      let isDoubleClick = lastClickAt +. thresholdMs > now
      lastClickAtRef.React.current = now

      switch timeoutIdRef.React.current {
      | Some(id) => Js.Global.clearTimeout(id)
      | None => ()
      }

      if isDoubleClick {
        onDoubleClick(evt)
      } else {
        ReactEvent.Synthetic.persist(evt)
        let timeoutId =
          thresholdMs |> int_of_float |> Js.Global.setTimeout(() => onSingleClick(evt))
        timeoutIdRef.React.current = Some(timeoutId)
      }
    }

    <div onClick> children </div>
  }
}

module Field = {
  type display =
    | Hidden
    | Marked
    | Safe({mines: int})
    | Exploded

  @react.component
  let make = (
    ~mines: int,
    ~field: Minesweeper.field,
    ~fieldState: Minesweeper.fieldState,
    ~onClick: Minesweeper.field => unit,
    ~onDoubleClick: Minesweeper.field => unit,
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
    | Safe(_) | Exploded => `${baseClassName}--revealed`
    | Hidden | Marked => ""
    }
    let contentClassName = switch display {
    | Safe({mines}) => `${baseClassName}--${string_of_int(mines)}`
    | Exploded => `${baseClassName}--exploded`
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
    let oldState: Minesweeper.fieldState = oldProps["fieldState"]
    let newState: Minesweeper.fieldState = newProps["fieldState"]
    oldState === newState
  }

  let make = React.memoCustomCompareProps(make, arePropsEqual)
}

module Game = {
  type action = Minesweeper.action

  type state = Minesweeper.state

  let reducer = (state, action) => Minesweeper.update(action, state)

  @react.component
  let make = (~width: int, ~height: int, ~mines: int) => {
    let (state, send) = React.useReducerWithMapState(reducer, (), () =>
      Minesweeper.initializeState(~width, ~height, ~mines)
    )
    let width = Minesweeper.gameWidthSelector(state)
    let height = Minesweeper.gameHeightSelector(state)
    let xs = range(0, width)
    let ys = range(0, height)
    let gameStatus = Minesweeper.gameStatusSelector(state)
    let rows =
      List.map(ys, y =>
        <div className="game__board-row" key={string_of_int(y)}> {List.map(xs, x => {
            let field: Minesweeper.field = {x: x, y: y}
            let fieldState = Minesweeper.fieldStateSelector(state, field)
            let displayedFieldState: Minesweeper.fieldState = switch (gameStatus, fieldState) {
            | (Won, {contents: Mine}) => {
                contents: Mine,
                visibility: Marked,
              }
            | (Won, {contents: Safe} as fieldState) | (Playing | Lost, fieldState) => fieldState
            }
            let onClick = field => send(ToggleMarker(field))
            let onDoubleClick = field => send(Reveal(field))
            let mines = Minesweeper.adjacentMinesCountSelector(state, field)
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
    let startButtonClick = _evt => send(Init(Minesweeper.initializeState(~width, ~height, ~mines)))
    let remainingMines = Minesweeper.remainingMinesCountSelector(state)
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

module App = {
  let logo: string = %bs.raw(`require('./logo.svg')`)

  type difficulty =
    | Easy
    | Normal
    | Hard

  type state = option<difficulty>

  type action = ChooseDifficulty(difficulty)

  let reducer = (_state: state, action: action) =>
    switch action {
    | ChooseDifficulty(difficulty) => Some(difficulty)
    }

  @react.component
  let make = (~message) => {
    let (state, send) = React.useReducer(reducer, None)
    let contents = switch state {
    | None =>
      let choose = (difficulty, _evt) => send(ChooseDifficulty(difficulty))
      <div className="difficulties__wrapper">
        <h3> {React.string("Choose difficulty")} </h3>
        <div className="difficulties">
          <button type_="button" className="difficulty" onClick={choose(Easy)} title="Easy">
            {React.string(`😌`)}
          </button>
          <button type_="button" className="difficulty" onClick={choose(Normal)} title="Normal">
            {React.string(`😐`)}
          </button>
          <button type_="button" className="difficulty" onClick={choose(Hard)} title="Hard">
            {React.string(`😱`)}
          </button>
        </div>
      </div>
    | Some(difficulty) =>
      let (width, height, mines) = switch difficulty {
      | Easy => (9, 9, 10)
      | Normal => (16, 16, 40)
      | Hard => (30, 16, 99)
      }
      <Game width height mines />
    }
    <div className="app">
      <div className="app__header">
        <a href=""> <img src=logo className="app__logo" alt="logo" /> </a>
        <h2> {React.string(message)} </h2>
        <p className="app__credits">
          {React.string("by ")}
          <a href="http://pewniak747.info" target="_blank"> {React.string(`Tomasz Pewiński`)} </a>
          {React.string(` · `)}
          <a href="https://github.com/pewniak747/minesweeper-reasonml" target="_blank">
            {React.string("source")}
          </a>
        </p>
      </div>
      <div className="app__intro"> contents </div>
    </div>
  }
}
