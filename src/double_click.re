type state = {
  lastClickAt: ref(float),
  timeoutId: ref(option(Js.Global.timeoutId)),
};

type action = unit;

let component = ReasonReact.reducerComponent("DoubleClick");

let thresholdMs = 200.0;

let make = (~onClick as onSingleClick, ~onDoubleClick, children) => {
  ...component,
  initialState: () => {lastClickAt: ref(0.0), timeoutId: ref(None)},
  reducer: (_action: action, _state: state) => ReasonReact.NoUpdate,
  render: ({state}) => {
    let onClick = evt => {
      ReactEvent.Synthetic.preventDefault(evt);
      let now = Js.Date.now();
      let lastClickAt = state.lastClickAt.contents;
      let isDoubleClick = lastClickAt +. thresholdMs > now;
      state.lastClickAt := now;

      switch (state.timeoutId.contents) {
      | Some(id) => Js.Global.clearTimeout(id)
      | None => ()
      };

      if (isDoubleClick) {
        onDoubleClick(evt);
      } else {
        ReactEvent.Synthetic.persist(evt);
        let timeoutId =
          thresholdMs
          |> int_of_float
          |> Js.Global.setTimeout(() => onSingleClick(evt));
        state.timeoutId := Some(timeoutId);
      };
    };

    <div onClick> children </div>;
  },
};
