module type SearchProblem = {
  type state;
  type action;
  let equal: (state, state) => bool;
  let hash: state => int;
  let getStartState: state => state;
  let isGoalState: state => bool;
  let getSuccessors: state => list((state, action, (int, int)));
  let getCostOfActions: list(action) => int;
};

module Make =
       (SP: SearchProblem)
       : (SearchProblem with type state = SP.state and type action = SP.action) => {
  type state = SP.state;
  type action = SP.action;
  let equal = SP.equal;
  let hash = SP.hash;
  let getStartState = SP.getStartState;
  let isGoalState = SP.isGoalState;
  let getSuccessors = SP.getSuccessors;
  let getCostOfActions = SP.getCostOfActions;
};