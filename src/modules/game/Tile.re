type tileType = Wall | Empty | Food; 

type tile = {
  x: int,
  y: int,
  tileType: tileType,
  node_id: int,
  freq: int,
};


let make = (~x: int, ~y: int, ~node_id: int, ~tileType: tileType) => {
  x,
  y,
  tileType,
  node_id,
  freq:0,
};