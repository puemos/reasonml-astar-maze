open Cell;

module Grid = {
  type action =
    | Tick;

  type state = {
    count: int,
    timerId: ref(option(Js.Global.intervalId)),
  };
  let styles =
    Css.(
      {"row": [display(flexBox), flexDirection(row), alignItems(stretch)]}
    );
  let component = ReasonReact.statelessComponent("Grid");
  let make = (~steps, _) => {
    ...component,
    render: _ => {
      let step = Belt.List.getExn(steps, Belt.List.length(steps) - 1);
      let matrix =
        step
        |> Belt.Array.map(_, Belt.Array.map(_, type_ => <Cell type_ />))
        |> Belt.Array.map(_, xs =>
             <div className=(Css.style(styles##row))>
               (ReasonReact.array(xs))
             </div>
           )
        |> ReasonReact.array;
      <div> matrix </div>;
    },
  };
};