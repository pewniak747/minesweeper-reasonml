open Jest;

open Expect;

open Game;

describe("Game.initializeState", (_) => {
  let state = initializeState(~width=3, ~height=4, ~mines=5, ());
  test("constructs a new Playing game state", (_) => {
    let status = gameStatusSelector(state);
    expect(status) |> toBe(Playing);
  });
  test("constructs a game state with correct board width", () =>
    expect(state.width) |> toBe(3)
  );
  test("constructs a game state with correct board height", () =>
    expect(state.height) |> toBe(4)
  );
  test("constructs a game state with correct number of fields", () => {
    let count = state.fields |> FieldsMap.cardinal;
    expect(count) |> toBe(12);
  });
  test("constructs a game state with correct number of Mine fields", () => {
    let onlyMined = FieldsMap.filter((_, (contents, _)) => contents == Mine);
    let mines = state.fields |> onlyMined |> FieldsMap.cardinal;
    expect(mines) |> toBe(5);
  });
  test("constructs a game state with all fields Hidden", () => {
    let isHidden = (_, (_, visibility)) => visibility == Hidden;
    expect(state.fields |> FieldsMap.for_all(isHidden)) |> toBe(true);
  });
});

let makeState = matrix : state => {
  let height = Array.length(matrix);
  let width = Array.length(matrix[0]);
  let fields: list((int, int)) = cartesian(range(0, width), range(0, height));
  let fieldsWithData =
    List.fold_left(
      (acc, field) => {
        let data =
          switch field {
          | (x, y) => matrix[y][x]
          };
        FieldsMap.add(field, data, acc);
      },
      FieldsMap.empty,
      fields
    );
  {width, height, fields: fieldsWithData};
};

let m = (Mine, Revealed);

let s = (Safe, Revealed);

let x = (Mine, Hidden);

let o = (Safe, Hidden);

let initialState =
  makeState([|
    [|o, o, o, o, o|],
    [|o, o, x, o, o|],
    [|o, o, o, o, o|],
    [|o, o, o, o, x|]
  |]);

let initialLostState =
  makeState([|
    [|s, o, o, o, o|],
    [|o, o, x, s, o|],
    [|o, o, s, s, o|],
    [|o, o, o, o, m|]
  |]);

let initialWonState =
  makeState([|
    [|s, s, s, s, s|],
    [|s, s, x, s, s|],
    [|s, s, s, s, s|],
    [|s, s, s, s, x|]
  |]);

describe("Game.reducer", () => {
  test("Init replaces the state with new one", () => {
    let expectedState = initializeState();
    let state = reducer(Init(expectedState), initialState);
    expect(state) |> toEqual(ReasonReact.Update(expectedState));
  });
  test("Reveal sets a field to Revealed state", () => {
    let action = Reveal((3, 2));
    let expectedState =
      makeState([|
        [|o, o, o, o, o|],
        [|o, o, x, o, o|],
        [|o, o, o, s, o|],
        [|o, o, o, o, x|]
      |]);
    let state = reducer(action, initialState);
    expect(state) |> toEqual(ReasonReact.Update(expectedState));
  });
  test(
    "Reveal sets the field's neighbourhood to Revealed if it has no Mines", () => {
    let action = Reveal((0, 3));
    let expectedState =
      makeState([|
        [|s, s, o, o, o|],
        [|s, s, x, o, o|],
        [|s, s, s, s, o|],
        [|s, s, s, s, x|]
      |]);
    let state = reducer(action, initialState);
    expect(state) |> toEqual(ReasonReact.Update(expectedState));
  });
  test("Reveal sets a Mined field to Revealed state", () => {
    let action = Reveal((2, 1));
    let expectedState =
      makeState([|
        [|o, o, o, o, o|],
        [|o, o, m, o, o|],
        [|o, o, o, o, o|],
        [|o, o, o, o, x|]
      |]);
    let state = reducer(action, initialState);
    expect(state) |> toEqual(ReasonReact.Update(expectedState));
  });
  test("Reveal does nothing if field already Revealed", () => {
    let initialState =
      makeState([|
        [|o, o, o, o, o|],
        [|o, o, x, o, o|],
        [|o, o, o, o, o|],
        [|o, o, s, o, x|]
      |]);
    let action = Reveal((2, 3));
    let state = reducer(action, initialState);
    expect(state) |> toEqual(ReasonReact.NoUpdate);
  });
  test("Reveal does nothing in Lost state", () => {
    let action = Reveal((3, 0));
    let state = reducer(action, initialLostState);
    expect(state) |> toEqual(ReasonReact.NoUpdate);
  });
  test("Reveal does nothing in Won state", () => {
    let action = Reveal((3, 1));
    let state = reducer(action, initialLostState);
    expect(state) |> toEqual(ReasonReact.NoUpdate);
  });
});

describe("Game.gameStatusSelector", () => {
  test("is Lost if any of the Mine fields are Revealed", () => {
    let status = gameStatusSelector(initialLostState);
    expect(status) |> toBe(Lost);
  });
  test("is Won if all of the Safe and no Mine fields are Revealed", () => {
    let status = gameStatusSelector(initialWonState);
    expect(status) |> toBe(Won);
  });
  test("is Playing if not Lost, and some Safe fields remain un-Revealed", () => {
    let status = gameStatusSelector(initialState);
    expect(status) |> toBe(Playing);
  });
});

describe("Game.adjacentMinesSelector", () => {
  test("returns 0 if no Mines in field neighbourhood", () => {
    let mines = adjacentMinesSelector(initialState, (2, 1));
    expect(mines) |> toBe(0);
  });
  test("returns 0 if no Mines in field neighbourhood (board corner)", () => {
    let mines = adjacentMinesSelector(initialState, (0, 0));
    expect(mines) |> toBe(0);
  });
  test("returns 0 if no Mines in field neighbourhood (board side)", () => {
    let mines = adjacentMinesSelector(initialState, (4, 1));
    expect(mines) |> toBe(0);
  });
  test("returns 1 if one Mine in field neighbourhood", () => {
    let mines = adjacentMinesSelector(initialState, (1, 1));
    expect(mines) |> toBe(1);
  });
  test("returns n if n Mines in field neighbourhood", () => {
    let mines = adjacentMinesSelector(initialState, (3, 2));
    expect(mines) |> toBe(2);
  });
});
