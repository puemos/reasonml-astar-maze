open Rationale;

module Problem =
  PositionSearchProblem.Make({});
module StatePriorityQueue =
  PriorityQueue.Make({
    type t = (int, Problem.t, list(Problem.action));
    let equal = ((_, a, _), (_, b, _)) => Problem.equal(a, b);
    let hash = ((_, a, _)) => Problem.hash(a);
  });

type state = Problem.t;
type action = Problem.action;

type frontierLine = (state, list(action));

let popFrontierLine = frontier =>
  if (StatePriorityQueue.is_empty(frontier)) {
    None;
  } else {
    let next = StatePriorityQueue.first(frontier);
    StatePriorityQueue.remove(frontier, next);

    Some(next);
  };

let rec loop = (~frontier, ~explored, ~steps, ~heuristic) => {
  let next = popFrontierLine(frontier);

  switch (next) {
  | None => ([], steps)
  | Some((_, state, actions)) =>
    let steps = steps @ [state];
    let isGoalState = Problem.isGoalState(state);
    let isUnexplored = ! RList.contains(state, explored);
    if (isGoalState) {
      (actions, steps);
    } else if (isUnexplored) {
      let explored = RList.append(state, explored);

      state
      |> Problem.getSuccessors
      |> Belt.List.keep(_, ((successor, _, _)) =>
           ! RList.containsWith(Problem.equal(successor), explored)
         )
      |> Belt.List.forEach(
           _,
           ((successor, action, _)) => {
             let nextActions = actions @ [action];
             let cost =
               Problem.getCostOfActions(nextActions) + heuristic(successor);
             StatePriorityQueue.add(
               frontier,
               (cost, successor, nextActions),
             );
           },
         );

      loop(~frontier, ~explored, ~steps, ~heuristic);
    } else {
      loop(~frontier, ~explored, ~steps, ~heuristic);
    };
  };
};

let graphSearch = (startState, heuristic) => {
  let frontier = StatePriorityQueue.make(((a, _, _), (b, _, _)) => a < b);
  StatePriorityQueue.add(frontier, (0, startState, []));

  loop(~frontier, ~explored=[], ~steps=[], ~heuristic);
};