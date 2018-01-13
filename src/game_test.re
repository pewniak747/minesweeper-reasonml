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
