open GameState;

open Tile;

open Rationale;

open Rationale.Function;

open PositionSearchProblem;

let startState = PositionSearchProblem.getStartState();

let (result, steps) =
  GraphSearch.graphSearch(startState, (state, actions) => 1);


ReactDOMRe.renderToElementWithId(
  <div> <div> <Grid steps /> </div> </div>,
  "root",
);