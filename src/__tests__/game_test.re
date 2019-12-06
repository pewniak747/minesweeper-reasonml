open Jest;

open Expect;

open Game;

open Utils;

module List = Belt.List;

describe("Game.initializeState", _ => {
  let state = initializeState(~width=3, ~height=4, ~mines=5);
  test("constructs a new Playing game state", _ => {
    let status = gameStatusSelector(state);
    expect(status) |> toBe(Playing);
  });
  test("constructs a game state with correct board width", () =>
    expect(Game.gameWidthSelector(state)) |> toBe(3)
  );
  test("constructs a game state with correct board height", () =>
    expect(Game.gameHeightSelector(state)) |> toBe(4)
  );
  test("constructs a game state with correct number of fields", () => {
    let count = state |> Game.fieldsSelector |> List.length;
    expect(count) |> toBe(12);
  });
  test("constructs a game state with correct number of Mine fields", () => {
    let onlyMined = fields =>
      List.keep(fields, ((_, (contents, _))) => contents == Mine);
    let mines = state |> Game.fieldsSelector |> onlyMined |> List.length;
    expect(mines) |> toBe(5);
  });
  test("constructs a game state with all fields Hidden", () => {
    let isHidden = ((_field, (_, visibility))) => visibility == Hidden;
    expect(List.every(Game.fieldsSelector(state), isHidden)) |> toBe(true);
  });
});

let makeState = (matrix): state => {
  let height = Array.length(matrix);
  let width = Array.length(matrix[0]);
  let fields: list(Game.field) =
    cartesian(range(0, width), range(0, height))
    ->List.map(((x, y)) => {x, y});
  let fieldsWithState =
    List.map(
      fields,
      field => {
        let {x, y} = field;
        let data = matrix[y][x];
        (field, data);
      },
    );
  Game.makeStateWithFieldsState(~width, ~height, ~fieldsWithState);
};

let m = (Mine, Revealed);

let s = (Safe, Revealed);

let x = (Mine, Hidden);

let o = (Safe, Hidden);

let fx = (Mine, Marked);

let fo = (Safe, Marked);

let initialState =
  makeState([|
    [|o, o, o, o, o|],
    [|o, o, x, o, o|],
    [|o, o, o, o, o|],
    [|o, o, o, o, x|],
  |]);

let initialLostState =
  makeState([|
    [|s, o, o, o, o|],
    [|o, o, x, s, o|],
    [|o, o, s, s, o|],
    [|o, o, o, o, m|],
  |]);

let initialWonState =
  makeState([|
    [|s, s, s, s, s|],
    [|s, s, x, s, s|],
    [|s, s, s, s, s|],
    [|s, s, s, s, x|],
  |]);

describe("Game.update", () => {
  describe("Init action", () =>
    test("replaces the state with new one", () => {
      let expectedState = initializeState(~width=10, ~height=8, ~mines=5);
      let update = update(Init(expectedState), initialState);
      expect(update) |> toEqual(expectedState);
    })
  );
  describe("Reveal action", () => {
    test("sets a field to Revealed state", () => {
      let action = Reveal({x: 3, y: 2});
      let expectedState =
        makeState([|
          [|o, o, o, o, o|],
          [|o, o, x, o, o|],
          [|o, o, o, s, o|],
          [|o, o, o, o, x|],
        |]);
      let update = update(action, initialState);
      expect(update) |> toEqual(expectedState);
    });
    test("sets the field's neighbourhood to Revealed if it has no Mines", () => {
      let action = Reveal({x: 0, y: 3});
      let expectedState =
        makeState([|
          [|s, s, o, o, o|],
          [|s, s, x, o, o|],
          [|s, s, s, s, o|],
          [|s, s, s, s, x|],
        |]);
      let update = update(action, initialState);
      expect(update) |> toEqual(expectedState);
    });
    test("sets a Mined field to Revealed state", () => {
      let initialState =
        makeState([|
          [|s, o, o, o, o|],
          [|o, o, x, o, o|],
          [|o, o, o, o, o|],
          [|o, o, o, o, x|],
        |]);
      let action = Reveal({x: 2, y: 1});
      let expectedState =
        makeState([|
          [|s, o, o, o, o|],
          [|o, o, m, o, o|],
          [|o, o, o, o, o|],
          [|o, o, o, o, x|],
        |]);
      let update = update(action, initialState);
      expect(update) |> toEqual(expectedState);
    });
    test(
      "reveals the un-Marked neighbouring fields if field already Revealed", () => {
      let initialState =
        makeState([|
          [|o, o, o, o, o|],
          [|o, o, fx, o, o|],
          [|o, o, o, s, o|],
          [|x, o, o, o, fx|],
        |]);
      let action = Reveal({x: 3, y: 2});
      let update = update(action, initialState);
      /* TODO: flood the upper left corner? */
      let expectedState =
        makeState([|
          [|o, o, o, s, s|],
          [|o, o, fx, s, s|],
          [|o, s, s, s, s|],
          [|x, s, s, s, fx|],
        |]);
      expect(update) |> toEqual(expectedState);
    });
    test(
      "does nothing if already Revealed field does not have matching number of Marked neighbours",
      () => {
        let initialState =
          makeState([|
            [|o, o, o, o, o|],
            [|o, o, fx, o, o|],
            [|o, o, o, s, o|],
            [|x, o, o, o, x|],
          |]);
        let action = Reveal({x: 3, y: 2});
        let update = update(action, initialState);
        expect(update) |> toBe(initialState);
      },
    );
    test(
      "creates a new arrangement of fields if first revealed field would be a mine",
      () => {
      let initialState =
        makeState([|
          [|x, x, x, x, x|],
          [|x, x, x, x, x|],
          [|x, o, x, x, x|],
          [|x, x, x, x, x|],
        |]);
      let action = Reveal({x: 3, y: 2});
      let update = update(action, initialState);
      let expectedState =
        makeState([|
          [|x, x, x, x, x|],
          [|x, x, x, x, x|],
          [|x, x, x, s, x|],
          [|x, x, x, x, x|],
        |]);
      expect(update) |> toEqual(expectedState);
    });
    test("does nothing in Lost state", () => {
      let action = Reveal({x: 3, y: 0});
      let update = update(action, initialLostState);
      expect(update) |> toBe(initialLostState);
    });
    test("does nothing in Won state", () => {
      let action = Reveal({x: 3, y: 1});
      let update = update(action, initialWonState);
      expect(update) |> toBe(initialWonState);
    });
  });
  describe("ToggleMarker action", () => {
    test("sets a Hidden field to Marked state", () => {
      let action = ToggleMarker({x: 3, y: 2});
      let expectedState =
        makeState([|
          [|o, o, o, o, o|],
          [|o, o, x, o, o|],
          [|o, o, o, fo, o|],
          [|o, o, o, o, x|],
        |]);
      let update = update(action, initialState);
      expect(update) |> toEqual(expectedState);
    });
    test("sets a Marked field to Hidden state", () => {
      let initialState =
        makeState([|
          [|o, o, o, o, o|],
          [|o, o, x, o, o|],
          [|o, o, o, fo, o|],
          [|o, o, o, o, x|],
        |]);
      let action = ToggleMarker({x: 3, y: 2});
      let expectedState =
        makeState([|
          [|o, o, o, o, o|],
          [|o, o, x, o, o|],
          [|o, o, o, o, o|],
          [|o, o, o, o, x|],
        |]);
      let update = update(action, initialState);
      expect(update) |> toEqual(expectedState);
    });
    test("does nothing if field already Revealed", () => {
      let initialState =
        makeState([|
          [|o, o, o, o, o|],
          [|o, o, x, o, o|],
          [|o, o, o, s, o|],
          [|o, o, o, o, x|],
        |]);
      let action = ToggleMarker({x: 3, y: 2});
      let update = update(action, initialState);
      expect(update) |> toBe(initialState);
    });
    test("does nothing in Lost state", () => {
      let action = ToggleMarker({x: 3, y: 0});
      let update = update(action, initialLostState);
      expect(update) |> toBe(initialLostState);
    });
    test("does nothing in Won state", () => {
      let action = ToggleMarker({x: 3, y: 1});
      let update = update(action, initialWonState);
      expect(update) |> toBe(initialWonState);
    });
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

describe("Game.adjacentMinesCountSelector", () => {
  test("returns 0 if no Mines in field neighbourhood", () => {
    let mines = adjacentMinesCountSelector(initialState, {x: 2, y: 1});
    expect(mines) |> toBe(0);
  });
  test("returns 0 if no Mines in field neighbourhood (board corner)", () => {
    let mines = adjacentMinesCountSelector(initialState, {x: 0, y: 0});
    expect(mines) |> toBe(0);
  });
  test("returns 0 if no Mines in field neighbourhood (board side)", () => {
    let mines = adjacentMinesCountSelector(initialState, {x: 4, y: 1});
    expect(mines) |> toBe(0);
  });
  test("returns 1 if one Mine in field neighbourhood", () => {
    let mines = adjacentMinesCountSelector(initialState, {x: 1, y: 1});
    expect(mines) |> toBe(1);
  });
  test("returns n if n Mines in field neighbourhood", () => {
    let mines = adjacentMinesCountSelector(initialState, {x: 3, y: 2});
    expect(mines) |> toBe(2);
  });
});

describe("Game.remainingMinesCountSelector", () =>
  test("returns the number of mines minus the number of marked fields", () => {
    let state =
      makeState([|
        [|o, o, o, o, o|],
        [|o, o, fx, o, o|],
        [|o, o, o, s, o|],
        [|x, o, o, o, fx|],
      |]);
    expect(remainingMinesCountSelector(state)) |> toBe(1);
  })
);
