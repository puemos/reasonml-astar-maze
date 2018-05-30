open GameState;
open World;

module PositionSearchProblem = {
  type state = (pos, list(pos), list(pos));
  type action = GameState.dir;
  let equal = (a, b) => {};
  let getStartState = s => GameState.make();
  let isGoalState = state => Belt.List.length(state.world.food) == 0;
  let getCostOfActions = actions => List.length(actions);
  let getSuccessors = state => {
    let legalActions = GameState.getLegalActions(state);
    Belt.List.map(
      legalActions,
      action => {
        let (dx, dy) = GameState.actionToVector(action);
        let ((px, py), food, path) = state;
        let nextPlayer = (px + dx, py + dy);
        let nextFood = Belt.List.keep(food, i => i != nextPlayer);
        let nextPath = Belt.List.add(path, nextPlayer);
        ((nextPlayer, nextFood, nextPath), action, 1);
      },
    );
  };
};