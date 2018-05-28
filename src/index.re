open GameState;
open PositionSearchProblem;

let startState = PositionSearchProblem.getStartState();
let heuristic = (state: gameStateData) =>
  state.world.food
  |> Belt.List.map(_, World.getXY(state.world.width))
  |> Belt.List.map(
       _,
       Distance.euclideanDistance((state.player.x, state.player.y)),
     )
  |> Belt.List.sort(_, (a, b) => a - b)
  |> Belt.List.head
  |> Belt.Option.getWithDefault(_, 1);

let (result, steps) = GraphSearch.graphSearch(startState, heuristic);

ReactDOMRe.renderToElementWithId(
  <div> <div> <Grid steps /> </div> </div>,
  "root",
);