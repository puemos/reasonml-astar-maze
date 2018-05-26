type coord = {
  x: int,
  y: int,
};

type cellType =
  | Wall
  | Empty;

type cell = {
  cellType,
  visits: int,
};

type maze = array(array(cell));

let isWall = (maze: maze, coord: coord) : bool => {
  let {x, y} = coord;
  if (x < 0 || y < 0) {
    true;
  } else {
    maze[x][y].cellType == Wall;
  };
};

let isOpen = (maze: maze, coord: coord) : bool => ! isWall(maze, coord);