open GameState;
open PositionSearchProblem;

let startState = GraphSearch.Problem.getStartState();

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

let heuristic = (state: GraphSearch.Problem.t) =>
  closetPath(state.player, state.world.food);

let (result, steps) = GraphSearch.graphSearch(startState, heuristic);

ReactDOMRe.renderToElementWithId(
  <div> <div> <Grid startState steps /> </div> </div>,
  "root",
);