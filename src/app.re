[%bs.raw {|require('./app.css')|}];

[@bs.module] external logo: string = "./logo.svg";

open Utils;

type difficulty =
  | Easy
  | Normal
  | Hard;

type state = option(difficulty);

type action =
  | ChooseDifficulty(difficulty);

let reducer = (state: state, action: action) =>
  switch (action) {
  | ChooseDifficulty(difficulty) => Some(difficulty)
  };

[@react.component]
let make = (~message) => {
  let (state, send) = React.useReducer(reducer, None);
  let contents =
    switch (state) {
    | None =>
      let choose = (difficulty, _evt) => send(ChooseDifficulty(difficulty));
      <div className="difficulties__wrapper">
        <h3> {React.string("Choose difficulty")} </h3>
        <div className="difficulties">
          <button
            type_="button"
            className="difficulty"
            onClick={choose(Easy)}
            title="Easy">
            {React.string({j|😌|j})}
          </button>
          <button
            type_="button"
            className="difficulty"
            onClick={choose(Normal)}
            title="Normal">
            {React.string({j|😐|j})}
          </button>
          <button
            type_="button"
            className="difficulty"
            onClick={choose(Hard)}
            title="Hard">
            {React.string({j|😱|j})}
          </button>
        </div>
      </div>;
    | Some(difficulty) =>
      let (width, height, mines) =
        switch (difficulty) {
        | Easy => (9, 9, 10)
        | Normal => (16, 16, 40)
        | Hard => (30, 16, 99)
        };
      <Ui.Game width height mines />;
    };
  <div className="app">
    <div className="app__header">
      <a href=""> <img src=logo className="app__logo" alt="logo" /> </a>
      <h2> {React.string(message)} </h2>
      <p className="app__credits">
        {React.string("by ")}
        <a href="http://pewniak747.info" target="_blank">
          {React.string({js|Tomasz Pewiński|js})}
        </a>
        {React.string({js| · |js})}
        <a
          href="https://github.com/pewniak747/minesweeper-reasonml"
          target="_blank">
          {React.string("source")}
        </a>
      </p>
    </div>
    <div className="app__intro"> contents </div>
  </div>;
};
