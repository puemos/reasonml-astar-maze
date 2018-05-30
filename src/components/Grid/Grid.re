open Rationale;
open World;
open GameState;

module Styles = {
  open Css;
  let row =
    style([display(flexBox), flexDirection(row), alignItems(stretch)]);
  let tileWrapper = (~isWall) =>
    style([
      display(flexBox),
      width(px(20)),
      height(px(20)),
      flexDirection(column),
      alignItems(stretch),
      backgroundColor(isWall ? black : grey),
      position(relative),
    ]);
  let tile = (~isPlayer) =>
    style([
      display(flexBox),
      margin(px(5)),
      width(px(10)),
      height(px(10)),
      backgroundColor(isPlayer ? pink : transparent),
    ]);
};

type action =
  | Tick;

type state = {
  count: int,
  timerId: ref(option(Js.Global.intervalId)),
};

let component = ReasonReact.reducerComponent("Grid");

let make = (~steps, ~startState, _children) => {
  ...component,
  initialState: () => {count: 0, timerId: ref(None)},
  reducer: (action, state) =>
    switch (action, Belt.List.size(steps) - state.count) {
    | (Tick, 1) =>
      switch (state.timerId^) {
      | Some(id) =>
        Js.Global.clearInterval(id);
        ReasonReact.NoUpdate;
      | None => ReasonReact.NoUpdate
      }
    | (Tick, _) => ReasonReact.Update({...state, count: state.count + 1})
    },
  didMount: self =>
    self.state.timerId :=
      Some(Js.Global.setInterval(() => self.send(Tick), 200)),
  willUnmount: self =>
    switch (self.state.timerId^) {
    | Some(id) => Js.Global.clearInterval(id)
    | None => ()
    },
  render: ({state}) => {
    let currState =
      Belt.List.get(steps, state.count)
      |> Belt.Option.getWithDefault(_, startState);
    let {height, width, walls, food} = startState.world;
    Belt.Array.range(0, height * width - 1)
    |> Belt_Array.reverse
    |> Array.to_list
    |> RList.splitEvery(width)
    |> List.map(row =>
         row
         |> Belt.List.map(_, World.getXY(width))
         |> Belt.List.map(_, ((x, y)) =>
              <div
                key=(string_of_int(x) ++ string_of_int(y))
                className=(
                  Styles.tileWrapper(~isWall=RList.contains((x, y), walls))
                )>
                <div
                  key=(string_of_int(x) ++ string_of_int(y))
                  className=(
                    Styles.tile(
                      ~isPlayer=RList.contains((x, y), currState.path),
                    )
                  )>
                  (
                    ReasonReact.string(
                      RList.contains((x, y), food) ? "*" : "",
                    )
                  )
                </div>
              </div>
            )
         |> Array.of_list
         |> Belt_Array.reverse
       )
    |> Array.of_list
    |> Array.mapi(
         (i, row) =>
           <div key=(string_of_int(i)) className=Styles.row>
             (ReasonReact.array(row))
           </div>,
         _,
       )
    |> ReasonReact.array;
  },
};