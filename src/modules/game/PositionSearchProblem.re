open SearchProblem;

open GameState;

module PositionSearchProblem = {
  type state = gameStateData;
  type action = dir;
  let equal = (a, b) => GameState.eq(a, b);
  let hash = a => GameState.hash(a);
  let getStartState = s => GameState.make();
  let isGoalState = state => state.player == {x: 0, y: 0};
  let getCostOfActions = actions => List.length(actions);
  let getSuccessors = state => {
    let legalAction = GameState.getLegalActions(state);
    Belt.List.map(
      legalAction,
      action => {
        let (dx, dy) = actionToVector(action);
        (
          {
            ...state,
            player: {
              x: state.player.x + dx,
              y: state.player.y + dy,
            },
          },
          action,
          1,
        );
      },
    );
  };
};