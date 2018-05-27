open Rationale;

open World;

open GameState;

open Tile;

module Styles = {
  open Css;
  let row =
    style([display(flexBox), flexDirection(row), alignItems(stretch)]);
  let tile = (~isWall, ~isFood, ~isPlayer) =>
    style([
      display(flexBox),
      width(px(20)),
      height(px(20)),
      flexDirection(column),
      alignItems(stretch),
      backgroundColor(
        isPlayer ? pink : isFood ? green : isWall ? black : grey,
      ),
      position(relative),
    ]);
};

type action =
  | Tick;

type state = {
  count: int,
  timerId: ref(option(Js.Global.intervalId)),
};

let component = ReasonReact.reducerComponent("Grid");

let make = (~steps, _children) => {
  ...component,
  initialState: () => {count: 0, timerId: ref(None)},
  reducer: (action, state) =>
    switch (action, Belt.List.size(steps) - state.count) {
    | (Tick, 0) =>
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
      Some(Js.Global.setInterval(() => self.send(Tick), 1000)),
  willUnmount: self =>
    switch (self.state.timerId^) {
    | Some(id) => Js.Global.clearInterval(id)
    | None => ()
    },
  render: ({state}) => {
    let currState =
      switch (Belt.List.get(steps, state.count)) {
      | None => Belt.List.get(steps, 0) |> Belt.Option.getExn
      | Some(s) => s
      };
    let {height, width, walls, food} = currState.world;
    Belt_Array.range(0, height * width - 1)
    |> Belt_Array.reverse
    |> Array.to_list
    |> RList.splitEvery(width)
    |> List.map(row =>
         row
         |> List.map(
              i =>
                <span
                  key=(string_of_int(i))
                  className=(
                    Styles.tile(
                      ~isWall=RList.contains(i, walls),
                      ~isFood=RList.contains(i, food),
                      ~isPlayer=
                        i
                        == World.getNodeId(
                             currState.player.x,
                             currState.player.y,
                             width,
                           ),
                    )
                  )
                />,
              _,
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