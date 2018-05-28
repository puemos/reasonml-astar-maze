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
        |> Belt.List.keep(_, ((successor, _, _)) =>
             ! RList.contains({...successor, path: []}, explored)
           )
        |> Belt.List.map(
             _,
             ((state, action, _)) => {
               let nextActions = actions @ [action];
               (
                 PositionSearchProblem.getCostOfActions(nextActions),
                 Some((state, nextActions)),
               );
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
    PrioQueue.PrioQueue.insert(
      frontier,
      heuristic(startState, []),
      Some((startState, [])),
    );
  loop(~frontier, ~explored, ~steps=[], ~heuristic);
};