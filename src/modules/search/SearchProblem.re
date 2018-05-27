module type SearchProblem = {
  type state;
  type action;
  let eq: (state, state) => bool;
  let hash: state => int;
  let getStartState: state => state;
  let isGoalState: state => bool;
  let getSuccessors: state => list((state, action, int));
  let getCostOfActions: list(action) => int;
};

module Make = (SP: SearchProblem) : SearchProblem => {
  type state = SP.state;
  type action = SP.action;
  let eq = SP.eq;
  let hash = SP.hash;
  let getStartState = SP.getStartState;
  let isGoalState = SP.isGoalState;
  let getSuccessors = SP.getSuccessors;
  let getCostOfActions = SP.getCostOfActions;
};