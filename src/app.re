[%bs.raw {|require('./app.css')|}];

[@bs.module] external logo : string = "./logo.svg";

open Utils;

type difficulty =
  | Easy
  | Normal
  | Hard;

type state = option(difficulty);

type action =
  | ChooseDifficulty(difficulty);

let component = ReasonReact.reducerComponent("App");

let make = (~message, _children) => {
  ...component,
  initialState: () => None,
  reducer: (action, _state) =>
    switch action {
    | ChooseDifficulty(difficulty) => ReasonReact.Update(Some(difficulty))
    },
  render: ({state, send}) => {
    let contents =
      switch state {
      | None =>
        let choose = (difficulty, _evt) => send(ChooseDifficulty(difficulty));
        <div className="difficulties__wrapper">
          <h3> (str("Choose difficulty")) </h3>
          <div className="difficulties">
            <button
              _type="button"
              className="difficulty"
              onClick=(choose(Easy))
              title="Easy">
              (str({j|😌|j}))
            </button>
            <button
              _type="button"
              className="difficulty"
              onClick=(choose(Normal))
              title="Normal">
              (str({j|😐|j}))
            </button>
            <button
              _type="button"
              className="difficulty"
              onClick=(choose(Hard))
              title="Hard">
              (str({j|😱|j}))
            </button>
          </div>
        </div>;
      | Some(difficulty) =>
        let (width, height, mines) =
          switch difficulty {
          | Easy => (9, 9, 10)
          | Normal => (16, 16, 40)
          | Hard => (30, 16, 99)
          };
        <Game width height mines />;
      };
    <div className="App">
      <div className="App-header">
        <a href=""> <img src=logo className="App-logo" alt="logo" /> </a>
        <h2> (str(message)) </h2>
      </div>
      <div className="App-intro"> contents </div>
    </div>;
  }
};
