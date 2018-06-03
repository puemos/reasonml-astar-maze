open Rationale;
open World;
open Cell;

module Styles = {
  open Css;
  let row =
    style([display(flexBox), flexDirection(row), alignItems(stretch)]);
  let tileWrapper = (~isWall) =>
    style([
      width(px(20)),
      height(px(20)),
      flexDirection(column),
      alignItems(center),
      justifyContent(center),
      backgroundColor(isWall ? black : grey),
      position(relative),
    ]);
  let tile =
    style([
      display(flexBox),
      width(px(10)),
      height(px(10)),
      backgroundColor(pink),
    ]);
  let food =
    style([
      position(absolute),
      display(flexBox),
      top(px(8)),
      right(px(8)),
      width(px(4)),
      height(px(4)),
      backgroundColor(green),
    ]);
};

type action =
  | Tick;

type state = {
  count: int,
  timerId: ref(option(Js.Global.intervalId)),
};

let component = ReasonReact.statelessComponent("Grid");

let make = (~steps, _children) => {
  ...component,
  render: self => {
    let step = Belt.List.getExn(steps, Belt.List.length(steps) - 1);
    let matrix =
      step
      |> Belt.Array.map(_, Belt.Array.map(_, type_ => <Cell type_ />))
      |> Belt.Array.map(_, xs =>
           <div className=Styles.row> (ReasonReact.array(xs)) </div>
         )
      |> ReasonReact.array;
    <div> matrix </div>;
  },
};