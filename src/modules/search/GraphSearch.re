open Rationale;
open SearchProblem;

module type S = {
  type state;
  let search: (state, state => int) => list(state);
};

module Make = (P: SearchProblem) : (S with type state := P.state) => {
  module StatePriorityQueue =
    PriorityQueue.Make({
      type t = (int, P.state, list(P.action));
      let equal = ((_, a, _), (_, b, _)) => P.equal(a, b);
      let hash = ((_, a, _)) => P.hash(a);
    });

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
    | None => steps
    | Some((_, state, actions)) =>
      let steps = steps @ [state];
      let isGoalState = P.isGoalState(state);
      let isUnexplored = !RList.containsWith(P.equal(state), explored);
      if (isGoalState) {
        steps;
      } else if (isUnexplored) {
        let explored = RList.append(state, explored);

        state
        |> P.getSuccessors
        |> Belt.List.keep(_, ((successor, _, _)) =>
             ! RList.containsWith(P.equal(successor), explored)
           )
        |> Belt.List.forEach(
             _,
             ((successor, action, _)) => {
               let nextActions = actions @ [action];
               let cost =
                 P.getCostOfActions(nextActions) + heuristic(successor);
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

  let search = (startState, heuristic) => {
    let frontier = StatePriorityQueue.make(((a, _, _), (b, _, _)) => a < b);
    StatePriorityQueue.add(frontier, (0, startState, []));

    loop(~frontier, ~explored=[], ~steps=[], ~heuristic);
  };
};