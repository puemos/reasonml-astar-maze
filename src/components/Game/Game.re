open World;
open Grid;
open Cell;

module Problem = SearchProblem.Make(PositionSearchProblem);
module GS = GraphSearch.Make(Problem);

let createMatrix = (~world, ~path, ~starting) => {
  let matrixSize = world.width * world.height;
  let cellHash = Belt.HashMap.make(~hintSize=10, ~id=(module CellID));

  world.walls
  |> Belt.List.forEach(_, point => Belt.HashMap.set(cellHash, point, Wall));

  world.food
  |> Belt.List.forEach(_, point => Belt.HashMap.set(cellHash, point, Food));

  [starting, ...path]
  |> Belt.List.forEach(
       _,
       point => {
         let type_ =
           switch (Belt.HashMap.get(cellHash, point)) {
           | None => Player
           | Some(Food) => PlayerFood
           | _ => Player
           };
         Belt.HashMap.set(cellHash, point, type_);
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
  |> Belt.List.map(_, ({path}) =>
       createMatrix(
         ~path,
         ~world=startState.world,
         ~starting=startState.player,
       )
     );
};

module Game = {
  let styles =
    Css.(
      {
        "page": [
          boxSizing(borderBox),
          background(linearGradient(deg(45), [(0, red), (100, blue)])),
          width(vw(100.)),
          height(vh(100.)),
        ],
      }
    );
  let component = ReasonReact.statelessComponent("Game");
  let make = _ => {...component, render: _ => <Grid steps=(search()) />};
};