open Rationale;
open World;

type tile =
  | Empty
  | Wall
  | Food
  | PlayerFood
  | Player;

module TileHash =
  Hashtbl.Make({
    type t = pos;
    let hash = ((x, y): t) => x + y * 10;
    let equal = ((x1, y1): t, (x2, y2): t) => x1 == x2 && y1 == y2;
  });

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
  let tile =
    style([
      display(flexBox),
      margin(px(5)),
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

let make = (~world, ~path, _children) => {
  ...component,
  /* initialState: () => {count: 0, timerId: ref(None)},
     reducer: (action, state) =>
       switch (action, Belt.List.size(steps) - state.count) {
       | (Tick, 2) =>
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
         Some(Js.Global.setInterval(() => self.send(Tick), 20)),
     willUnmount: self =>
       switch (self.state.timerId^) {
       | Some(id) => Js.Global.clearInterval(id)
       | None => ()
       }, */
  render: self => {
    let matrixSize = world.width * world.height;
    let tileHash = TileHash.create(matrixSize);

    let matrix =
      matrixSize
      |> Belt.List.make(_, None)
      |> Belt.List.mapWithIndex(_, (idx, _) =>
           World.getXY(world.width, idx)
         )
      |> Belt.List.toArray
      |> Belt.Array.reverse
      |> Belt.List.fromArray;

    matrix
    |> Belt.List.forEach(_, point => TileHash.add(tileHash, point, Empty));

    world.walls
    |> Belt.List.forEach(_, point => TileHash.replace(tileHash, point, Wall));

    world.food
    |> Belt.List.forEach(_, point => TileHash.replace(tileHash, point, Food));

    [(0, 0), ...path]
    |> Belt.List.forEach(
         _,
         point => {
           let nextType =
             TileHash.find(tileHash, point) == Food ? PlayerFood : Player;
           TileHash.replace(tileHash, point, nextType);
         },
       );

    let matrix =
      matrix
      |> Belt.List.map(_, TileHash.find(tileHash))
      |> Belt.List.map(_, tile =>
           switch (tile) {
           | Empty => <span className=(Styles.tileWrapper(~isWall=false)) />
           | Wall => <span className=(Styles.tileWrapper(~isWall=true)) />
           | Player =>
             <span className=(Styles.tileWrapper(~isWall=false))>
               <span className=Styles.tile />
             </span>
           | Food =>
             <span className=(Styles.tileWrapper(~isWall=false))>
               <span className=Styles.food />
             </span>
           | PlayerFood =>
             <span className=(Styles.tileWrapper(~isWall=false))>
               <span className=Styles.tile>
                 <span className=Styles.food />
               </span>
             </span>
           }
         )
      |> Rationale.RList.splitEvery(world.width)
      |> Belt.List.map(_, xs =>
           xs |> Belt.List.toArray |> Belt.Array.reverse |> ReasonReact.array
         )
      |> Belt.List.map(_, xs => <div className=Styles.row> xs </div>)
      |> Belt.List.toArray
      |> ReasonReact.array;
    <div> matrix </div>;
  },
};