open World;
open Rationale;

type dir =
  | Right
  | Left
  | Top
  | Bottom;

let checkRight = (~world as {walls, width}, ~x: int, ~y: int) =>
  x + 1 >= width || RList.contains((x + 1, y), walls);

let checkLeft = (~world as {walls}, ~x: int, ~y: int) =>
  x - 1 < 0 || RList.contains((x - 1, y), walls);

let checkTop = (~world as {walls}, ~x: int, ~y: int) =>
  y - 1 < 0 || RList.contains((x, y - 1), walls);

let checkBottom = (~world as {walls, width}, ~x: int, ~y: int) =>
  y + 1 >= width || RList.contains((x, y + 1), walls);

let getLegalActions = (world: world, player: pos) => {
  let world = world;
  let (x, y) = player;
  Belt.List.keep([Right, Left, Top, Bottom], d =>
    switch (d) {
    | Right => ! checkRight(~world, ~x, ~y)
    | Left => ! checkLeft(~world, ~x, ~y)
    | Top => ! checkTop(~world, ~x, ~y)
    | Bottom => ! checkBottom(~world, ~x, ~y)
    }
  );
};

let actionToVector = (action: dir) =>
  switch (action) {
  | Right => (1, 0)
  | Left => ((-1), 0)
  | Top => (0, (-1))
  | Bottom => (0, 1)
  };