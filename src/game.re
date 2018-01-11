let str = ReasonReact.stringToElement;

let component = ReasonReact.statelessComponent("Game");

let make = _children => {
  ...component,
  render: _self => (
    <div className="game">
      <div className="game__header">
        <button _type="button" className="start-button">
          {str({js|🙂|js})}
        </button>
      </div>
      <div className="game__board">
        <div className="game__board-row">
          <div className="game__board-field">
            <button _type="button">{str("1")}</button>
          </div>
          <div className="game__board-field">
            <button _type="button">{str(" ")}</button>
          </div>
          <div className="game__board-field">
            <button _type="button">{str("")}</button>
          </div>
          <div className="game__board-field">
            <button _type="button">{str("")}</button>
          </div>
        </div>
        <div className="game__board-row">
          <div className="game__board-field">
            <button _type="button">{str("1")}</button>
          </div>
          <div className="game__board-field">
            <button _type="button">{str("")}</button>
          </div>
          <div className="game__board-field">
            <button _type="button">{str("")}</button>
          </div>
          <div className="game__board-field">
            <button _type="button">{str("")}</button>
          </div>
        </div>
      </div>
    </div>
  )
};
