open World;

open Rationale;

open Belt;

open Tile;

type dir =
  | Right
  | Left
  | Top
  | Bottom;

type player = {
  x: int,
  y: int,
};

type gameStateData = {
  world,
  player,
  foodEaten: int,
  capsuleEaten: int,
  agentMoved: int,
  lose: bool,
  win: bool,
  scoreChange: int,
};

let isWin = (data: gameStateData) => data.win;

let isLose = (data: gameStateData) => data.lose;

let checkRight = (~world as {walls, width, height}, ~x: int, ~y: int) =>
  if (x + 1 >= width) {
    true;
  } else {
    let right = y * width + x + 1;
    RList.contains(right, walls);
  };

let checkLeft = (~world as {walls, width, height}, ~x: int, ~y: int) =>
  if (x - 1 < 0) {
    true;
  } else {
    let left = y * width + x - 1;
    RList.contains(left, walls);
  };

let checkTop = (~world as {walls, width, height}, ~x: int, ~y: int) =>
  if (y - 1 < 0) {
    true;
  } else {
    let top = (y - 1) * width + x;
    RList.contains(top, walls);
  };

let checkBottom = (~world as {walls, width, height}, ~x: int, ~y: int) =>
  if (y + 1 >= height) {
    true;
  } else {
    let bottom = (y + 1) * width + x;
    RList.contains(bottom, walls);
  };

let getLegalActions = (data: gameStateData) =>
  if (isWin(data) || isLose(data)) {
    [];
  } else {
    let world = data.world;
    let x = data.player.x;
    let y = data.player.y;
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

let eq = (a, b) => {
  let playerMoved = a.player.x == b.player.x && a.player.y == b.player.y;
  let foodEaten = Belt_List.cmpByLength(a.world.food, b.world.food) !== 0;
  playerMoved || foodEaten;
};

let hash = a => {
  let playerHash = a.player.x * 10 + a.player.y;
  Belt_List.mapWithIndex(a.world.food, (i, f) =>
    float_of_int(f) *. 10.0 ** (float_of_int(i) +. 3.0)
  )
  |> Belt.List.reduce(_, playerHash, (a, b) => a + int_of_float(b));
};

let make = () => {
  world: World.make(),
  player: {
    x: 0,
    y: 0,
  },
  foodEaten: 0,
  capsuleEaten: 0,
  agentMoved: 0,
  lose: false,
  win: false,
  scoreChange: 0,
};