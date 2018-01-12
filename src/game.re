let str = ReasonReact.stringToElement;
let arr = ReasonReact.arrayToElement;

type field = (int, int);
type fieldVisibility = Hidden | Revealed;
type fieldContents = Mine | Safe;
type fieldData = (fieldContents, fieldVisibility);

module OrderedFields = {
  type t = field;
  let compare = ((x0,y0): t, (x1,y1): t) =>
    switch (Pervasives.compare(x0, x1)) {
      | 0 => Pervasives.compare(y0, y1)
      | c => c
    };
};

module FieldsMap = Map.Make(OrderedFields);
module FieldsSet = Set.Make(OrderedFields);

type action = Init | Reveal(field);

type state = {
  width: int,
  height: int,
  fields: FieldsMap.t(fieldData)
};

let rec range = (start: int, end_: int) =>
  if (start >= end_) {
    [];
  } else {
    [start, ...range(start + 1, end_)];
  };

let cartesian = (l1, l2) => List.concat(List.map(e => List.map(e' => (e,e'), l2), l1));

let compareFst = ((fst1: int, _), (fst2: int, _)) => Pervasives.compare(fst1, fst2);

let shuffle = (d) => {
  let nd = List.map((c) => (Random.bits(), c), d);
  let sond = List.sort(compareFst, nd);
  List.map(snd, sond)
};

let rec take = (n, lst) => {
  switch (n) {
    | 0 => []
    | n => [List.hd(lst), ...take(n - 1, List.tl(lst))]
  };
};

let initializeState = () => {
  let width = 10;
  let height = 8;
  let mines = 20;
  let xs = range(0, width);
  let ys = range(0, height);
  let fields = cartesian(xs, ys);
  let minedFields = fields |> shuffle |> take(mines) |> FieldsSet.of_list;
  let reduceFields = (acc, field) => {
    let contents = FieldsSet.mem(field, minedFields) ? Mine : Safe;
    let data = (contents, Hidden);
    FieldsMap.add(field, data, acc);
  };
  let fieldsWithData = List.fold_left(reduceFields, FieldsMap.empty, fields);
  { width, height, fields: fieldsWithData };
};

let component = ReasonReact.reducerComponent("Game");

let make = _children => {
  ...component,
  initialState: initializeState,
  reducer: (action, state) =>
    switch (action) {
      | Init => ReasonReact.Update(initializeState()) /* TODO: purify the reducer */
      | Reveal(field) => {
        let data = FieldsMap.find(field, state.fields);
        let newData = switch (data) {
          | (contents, _) => (contents, Revealed)
        };
        let fields = FieldsMap.add(field, newData, state.fields);
        /* ReasonReact.SideEffects((_self) => Js.log(field)) */
        ReasonReact.Update({ ...state, fields });
      }
    },
  render: ({ state, send }) => {
    let xs = range(0, state.width);
    let ys = range(0, state.height);

    let rows = Array.of_list @@ List.map(y =>
      <div className="game__board-row" key={string_of_int(y)}>{
        arr(Array.of_list @@ List.map(x => {
          let field = (x, y);
          switch (FieldsMap.find(field, state.fields)) {
            | exception Not_found => ReasonReact.nullElement
            | data => {
              let contents = switch (data) {
                | (_, Hidden) => ""
                | (Safe, Revealed) => "1"
                | (Mine, Revealed) => {js|💥|js}
              };
              let onClick = _event => send(Reveal(field));
              <div className="game__board-field" key={string_of_int(x)}>
                <button _type="button" onClick>{str(contents)}</button>
              </div>
              }
          }
        }
          , xs))
      }</div>
    , ys);

    <div className="game">
      <div className="game__header">
        <button _type="button" className="start-button" onClick={(_evt) => send(Init)}>
          {str({js|🙂|js})}
        </button>
      </div>
      <div className="game__board">
        {arr(rows)}
      </div>
    </div>
  }
};
