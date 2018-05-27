type tile = {
  x: int,
  y: int,
  node_id: int,
  parent_id: int,
  top: bool,
  bottom: bool,
  left: bool,
  right: bool,
  is_wall: bool,
  h: int,
  g: int,
  f: int,
};

let create = (~x: int, ~y: int, ~node_id: int, ~is_wall: bool) => {
  x,
  y,
  is_wall,
  node_id,
  parent_id: (-1),
  top: false,
  bottom: false,
  left: false,
  right: false,
  h: 0,
  g: 0,
  f: 0,
};

let move_cost = 10;

let calc_h = (end_node: tile, self: tile) =>
  if (self.is_wall) {
    self.h;
  } else {
    let x_diff = self.x - end_node.x;
    let y_diff = self.y - end_node.y;
    (x_diff + y_diff) * move_cost;
  };

let calc_f_g = (parent_g: int, self: tile) => {
  ...self,
  g: parent_g + move_cost,
  f: self.g + self.h,
};

let reset = (end_node: tile, self: tile) => {
  ...self,
  parent_id: (-1),
  g: 0,
  f: 0,
  h: calc_h(self, end_node),
};