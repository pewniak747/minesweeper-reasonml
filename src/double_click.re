let thresholdMs = 200.0;

[@react.component]
let make = (~onClick as onSingleClick, ~onDoubleClick, ~children) => {
  let lastClickAtRef = React.useRef(0.0);
  let timeoutIdRef = React.useRef(None);

  let onClick = evt => {
    ReactEvent.Synthetic.preventDefault(evt);
    let now = Js.Date.now();
    let lastClickAt = React.Ref.current(lastClickAtRef);
    let isDoubleClick = lastClickAt +. thresholdMs > now;
    React.Ref.setCurrent(lastClickAtRef, now);

    switch (React.Ref.current(timeoutIdRef)) {
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
      React.Ref.setCurrent(timeoutIdRef, Some(timeoutId));
    };
  };

  <div onClick> children </div>;
};
