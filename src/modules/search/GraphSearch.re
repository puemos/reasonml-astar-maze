open PositionSearchProblem;

open Rationale;

open GameState;

type frontierLine = (
  PositionSearchProblem.state,
  list(PositionSearchProblem.action),
);

let rec loop = (~frontier, ~explored, ~steps, ~heuristic) => {
  let (_, next, frontier) =
    try (PrioQueue.PrioQueue.extract(frontier)) {
    | PrioQueue.PrioQueue.Queue_is_empty => (0, None, frontier)
    };
  switch (next) {
  | None => ([], steps)
  | Some((state, actions)) =>
    let steps = steps @ [state];
    let isGoalState = PositionSearchProblem.isGoalState(state);
    let isUnexplored = ! RList.contains({...state, path: []}, explored);
    if (isGoalState) {
      (actions, steps);
    } else if (isUnexplored) {
      let explored = RList.append({...state, path: []}, explored);

      let successors =
        state
        |> PositionSearchProblem.getSuccessors
        |> Belt.List.keep(
             _,
             ((successor, action, _)) => {
               let isContains =
                 RList.containsWith(
                   PositionSearchProblem.eq(successor),
                   explored,
                 );
               if (state.player.x === 2 && state.player.y === 2) {
                 Js.log2("successor", Array.of_list(successor.world.food));
                 Js.log3("action", action, isContains);
                 Js.log2(state, successor);
               };
               ! isContains;
             },
           )
        |> Belt.List.map(
             _,
             ((successor, action, _)) => {
               let nextActions = actions @ [action];
               let cost =
                 PositionSearchProblem.getCostOfActions(nextActions)
                 + heuristic(successor);

               (cost, Some((successor, nextActions)));
             },
           );

      let frontier = PrioQueue.PrioQueue.insertAll(frontier, successors);
      loop(~frontier, ~explored, ~steps, ~heuristic);
    } else {
      loop(~frontier, ~explored, ~steps, ~heuristic);
    };
  };
};

let graphSearch = (startState, heuristic) => {
  let frontier = PrioQueue.PrioQueue.empty;
  let explored = [];
  let frontier =
    PrioQueue.PrioQueue.insert(frontier, 0, Some((startState, [])));
  loop(~frontier, ~explored, ~steps=[], ~heuristic);
};