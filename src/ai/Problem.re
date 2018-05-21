open Maze;

type state = {
  maze,
  height: int,
  width: int,
  agent: coord,
  goal: coord,
};

type action =
  | North
  | South
  | West
  | East;

let goalTest = (state: state) => state.agent == state.goal;

let filterNorth = (state: state) =>
  List.filter(ac => ac == North && state.agent.y < 0);

let filterSouth = (state: state) =>
  List.filter(ac => ac == South && state.agent.y > state.height);

let filterWest = (state: state) =>
  List.filter(ac => ac == West && state.agent.x > state.width);

let filterEast = (state: state) =>
  List.filter(ac => ac == East && state.agent.x < 0);

let availableActions = (state: state) : list(action) => {
  let allActions = [North, South, West, East];
  allActions
  |> filterEast(state)
  |> filterWest(state)
  |> filterNorth(state)
  |> filterSouth(state);
};

let result = (state: state, action: action) =>
  switch (action) {
  | North => {
      ...state,
      agent: {
        x: state.agent.x + 1,
        y: state.agent.y,
      },
    }
  | South => {
      ...state,
      agent: {
        x: state.agent.x - 1,
        y: state.agent.y,
      },
    }
  | West => {
      ...state,
      agent: {
        x: state.agent.x,
        y: state.agent.y - 1,
      },
    }
  | East => {
      ...state,
      agent: {
        x: state.agent.x,
        y: state.agent.y + 1,
      },
    }
  };

let pathCost = (cost: int) => cost + 1;