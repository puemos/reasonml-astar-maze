open SearchProblem;

open Tile;

open GameState;

module PositionSearchProblem = {
  type state = GameState.gameStateData;
  type action = GameState.dir;
  let compare = GameState.compare;
  let eq = GameState.eq;
  let getStartState = s => GameState.make();
  let isGoalState = state => Belt.List.length(state.world.food) == 0;
  let getCostOfActions = actions => List.length(actions);
  let getSuccessors = state => {
    let legalActions = GameState.getLegalActions(state);
    Belt.List.map(
      legalActions,
      action => {
        let (dx, dy) = GameState.actionToVector(action);
        let nextPlayer = {x: state.player.x + dx, y: state.player.y + dy};
        let nextPlayerNodeId =
          World.getNodeId(
            ~x=nextPlayer.x,
            ~y=nextPlayer.y,
            ~width=state.world.width,
          );
        let food =
          Belt_List.keep(state.world.food, i => i != nextPlayerNodeId);
        let path = Belt.List.add(state.path, nextPlayerNodeId);
        (
          {
            ...state,
            world: {
              ...state.world,
              food,
            },
            player: nextPlayer,
            path,
          },
          action,
          1,
        );
      },
    );
  };
};