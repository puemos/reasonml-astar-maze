open GameState;
open PositionSearchProblem;

let startState = PositionSearchProblem.getStartState();

let closet = (prevPos, foodPos) =>
  foodPos
  |> Belt.List.map(_, pos =>
       (pos, Distance.euclideanDistance(pos, prevPos))
     )
  |> Belt.List.sort(_, ((_, a), (_, b)) => a - b)
  |> Belt.List.head
  |> Belt.Option.getWithDefault(_, (prevPos, 0));
let rec closetPath = (prevPos, foodPos) =>
  switch (foodPos) {
  | [] => 0
  | _ =>
    let (pos, cost) = closet(prevPos, foodPos);
    let (cx, cy) = pos;
    let nextFoodPos =
      Belt.List.keep(foodPos, ((a, b)) => a != cx || b != cy);
    cost + closetPath(pos, nextFoodPos);
  };
let heuristic = (state: gameState) => {
  let foodPos =
    state.world.food |> Belt.List.map(_, World.getXY(state.world.width));
  closetPath((state.player.x, state.player.y), foodPos);
};

let (result, steps) = GraphSearch.graphSearch(startState, heuristic);

ReactDOMRe.renderToElementWithId(
  <div> <div> <Grid startState steps /> </div> </div>,
  "root",
);