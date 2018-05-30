open World;
open Rationale;

type dir =
  | Right
  | Left
  | Top
  | Bottom;

type gameState = {
  world,
  player: pos,
  foodEaten: int,
  capsuleEaten: int,
  agentMoved: int,
  lose: bool,
  win: bool,
  scoreChange: int,
  path: list(pos),
};

let isWin = (data: gameState) => data.win;

let isLose = (data: gameState) => data.lose;

let checkRight = (~world as {walls, width}, ~x: int, ~y: int) =>
  x + 1 >= width || RList.contains((x + 1, y), walls);

let checkLeft = (~world as {walls}, ~x: int, ~y: int) =>
  x - 1 < 0 || RList.contains((x - 1, y), walls);

let checkTop = (~world as {walls}, ~x: int, ~y: int) =>
  y - 1 < 0 || RList.contains((x, y - 1), walls);

let checkBottom = (~world as {walls, width}, ~x: int, ~y: int) =>
  y + 1 >= width || RList.contains((x, y + 1), walls);

let getLegalActions = (data: gameState) =>
  if (isWin(data) || isLose(data)) {
    [];
  } else {
    let world = data.world;
    let (x, y) = data.player;
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

let make = () => {
  world: World.make(),
  player: (0, 0),
  foodEaten: 0,
  capsuleEaten: 0,
  agentMoved: 0,
  lose: false,
  win: false,
  scoreChange: 0,
  path: [],
};