open Maze;

let search =
    (
      start: coord,
      isAGoal: coord => bool,
      nextNodeFn,
      heuristic: coord => int,
    ) =>
  ();