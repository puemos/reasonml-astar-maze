open World;
open Cell;

module Problem = SearchProblem.Make(PositionSearchProblem);
module GS = GraphSearch.Make(Problem);

let createMatrix = (~world, ~path) => {
  let matrixSize = world.width * world.height;
  let cellHash = Belt.HashMap.make(~hintSize=10, ~id=(module CellID));

  world.walls
  |> Belt.List.forEach(_, point => Belt.HashMap.set(cellHash, point, Wall));

  world.food
  |> Belt.List.forEach(_, point => Belt.HashMap.set(cellHash, point, Food));

  [(0, 0), ...path]
  |> Belt.List.forEach(
       _,
       point => {
         let nextType =
           switch (Belt.HashMap.get(cellHash, point)) {
           | None => Player
           | Some(Food) => PlayerFood
           | _ => Player
           };
         Belt.HashMap.set(cellHash, point, nextType);
       },
     );

  matrixSize
  |> Belt.List.make(_, None)
  |> Belt.List.mapWithIndex(_, (idx, _) => World.getXY(world.width, idx))
  |> Belt.List.map(_, point =>
       switch (Belt.HashMap.get(cellHash, point)) {
       | None => Empty
       | Some(type_) => type_
       }
     )
  |> Belt.List.toArray
  |> Belt.Array.reverse
  |> Belt.List.fromArray
  |> Rationale.RList.splitEvery(world.width)
  |> Belt.List.map(_, xs => xs |> Belt.List.toArray |> Belt.Array.reverse)
  |> Belt.List.toArray;
};

let search = () => {
  let startState =
    PositionSearchProblem.getStartState({
      world: World.make(World.testMap),
      player: (0, 0),
      path: [],
    });

  GS.search(startState, FoodAgent.heuristic)
  |> Belt.List.map(_, ({path, world}) => createMatrix(~path, ~world));
};

let component = ReasonReact.statelessComponent("Game");
module Styles = {
  open Css;
};

let make = _children => {
  ...component,
  render: self => <Grid steps=(search()) />,
};