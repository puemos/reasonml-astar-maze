open GameState;

module Problem = SearchProblem.Make(PositionSearchProblem);

let startState =
  PositionSearchProblem.getStartState({
    world: World.make(World.testMap),
    player: (0, 0),
    path: [],
  });
module GS = GraphSearch.Make(Problem);

let steps = GS.search(startState, FoodAgent.heuristic);

let {path}: PositionSearchProblem.state =
  Belt.List.get(steps, Belt.List.length(steps) - 1) |> Belt.Option.getExn;

let component = ReasonReact.statelessComponent("Game");
module Styles = {
  open Css;
};

let make = _children => {
  ...component,
  render: self => <Grid world=startState.world path />,
};