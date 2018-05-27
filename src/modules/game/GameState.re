open World;

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

let getLegalActions = (data: gameStateData) =>
  if (isWin(data) || isLose(data)) {
    [];
  } else {
    let nodeId =
      getNodeId(~x=data.player.x, ~y=data.player.y, ~width=data.world.width);
    let node = data.world.tiles[nodeId];
    Belt.List.keep([Right, Left, Top, Bottom], d =>
      switch (d) {
      | Right => node.right
      | Left => node.left
      | Top => node.top
      | Bottom => node.bottom
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

let eq = (a, b) => a.player.x == b.player.x && a.player.y == b.player.y;

let hash = a => a.player.x * 10 + a.player.y;

let make = () => {
  world: World.makeTestWorld(),
  player: {
    x: 5,
    y: 5,
  },
  foodEaten: 0,
  capsuleEaten: 0,
  agentMoved: 0,
  lose: false,
  win: false,
  scoreChange: 0,
};