open SearchProblem;

open Tile;

open GameState;

let euclideanDistance = ((x1, y1), (x2, y2)) =>
  int_of_float(
    (
      (float_of_int(x1) -. float_of_int(x2))
      ** 2.0
      +. (float_of_int(y1) -. float_of_int(y2))
      ** 2.0
    )
    ** 0.5,
  );

module PositionSearchProblem = {
  type state = gameStateData;
  type action = dir;
  let equal = (a, b) => GameState.eq(a, b);
  let hash = a => GameState.hash(a);
  let getStartState = s => GameState.make();
  let isGoalState = state => {
    Js.log2("state.world.food", Array.of_list(state.world.food));
    Belt.List.length(state.world.food) == 0;
  };
  let getCostOfActions = actions => List.length(actions);
  let getSuccessors = state => {
    let legalActions = GameState.getLegalActions(state);
    Belt.List.map(
      legalActions,
      action => {
        let (dx, dy) = actionToVector(action);
        let nextPlayer = {x: state.player.x + dx, y: state.player.y + dy};
        let nextPlayerNodeId =
          World.getNodeId(
            ~x=nextPlayer.x,
            ~y=nextPlayer.y,
            ~width=state.world.width,
          );
        Js.log2("nextPlayerNodeId", nextPlayerNodeId);
        let food =
          Belt_List.keep(state.world.food, i => i != nextPlayerNodeId);
        Js.log2("food", Array.of_list(food));
        (
          {
            ...state,
            world: {
              ...state.world,
              food,
            },
            player: nextPlayer,
          },
          action,
          1,
        );
      },
    );
  };
};