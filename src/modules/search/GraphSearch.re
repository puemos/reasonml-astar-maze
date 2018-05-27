open PositionSearchProblem;

open GameState;

type frontierLine = (
  PositionSearchProblem.state,
  list(PositionSearchProblem.action),
);

module GameStateHashSet =
  Belt.Id.MakeHashable(
    {
      type t = PositionSearchProblem.state;
      let eq = PositionSearchProblem.equal;
      let hash = PositionSearchProblem.hash;
    },
  );

let rec loop = (~frontier, ~explored, ~steps, ~heuristic) => {
  let (_, next, frontier) = try (PrioQueue.PrioQueue.extract(frontier)){
  | PrioQueue.PrioQueue.Queue_is_empty=>(0,None,frontier);
  };
  switch (next) {
  | None => ([], [])
  | Some((state, actions)) =>
    let steps = steps @ [state];
    let isGoalState = PositionSearchProblem.isGoalState(state);
    if (isGoalState) {
      (actions, steps);
    } else if (! Belt_HashSet.has(explored, state)) {
      Belt_HashSet.add(explored, state);
      let successors =
        state
        |> PositionSearchProblem.getSuccessors
        |> Belt.List.keep(_, ((state, _, _)) =>
             ! Belt_HashSet.has(explored, state)
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
  let explored =
    Belt_HashSet.make(~hintSize=10, ~id=(module GameStateHashSet));
  let frontier =
    PrioQueue.PrioQueue.insert(
      frontier,
      heuristic(startState, []),
      Some((startState, [])),
    );
  loop(
    ~frontier,
    ~explored=Belt_HashSet.copy(explored),
    ~steps=[],
    ~heuristic,
  );
};