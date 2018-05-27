open PositionSearchProblem;

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

let rec loop = (~frontier, ~explored) => {
  let next: option(frontierLine) = Belt_MutableStack.pop(frontier);
  switch (next) {
  | None => []
  | Some((state, actions)) =>
    let isGoalState = PositionSearchProblem.isGoalState(state);
    if (isGoalState) {
      actions;
    } else if (! Belt_HashSet.has(explored, state)) {
      Belt_HashSet.add(explored, state);
      let successors = PositionSearchProblem.getSuccessors(state);
      Belt.List.forEach(successors, ((state, action, _)) =>
        if (! Belt_HashSet.has(explored, state)) {
          let nextActions = Belt.List.add(actions, action);
          let successorNode = (state, nextActions);
          Belt_MutableStack.push(frontier, successorNode);
        }
      );
      loop(~frontier, ~explored);
    } else {
      loop(~frontier, ~explored);
    };
  };
};

let graphSearch = () => {
  let frontier = Belt.MutableStack.make();
  let explored =
    Belt_HashSet.make(~hintSize=10, ~id=(module GameStateHashSet));
  let startState = PositionSearchProblem.getStartState();
  Belt.MutableStack.push(frontier, (startState, []));
  loop(~frontier, ~explored=Belt_HashSet.copy(explored));
};