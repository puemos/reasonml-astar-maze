type tile = {
  x_id: int,
  y_id: int,
  node_id: int,
  parent_id: int,
  top: int,
  bottom: int,
  left: int,
  right: int,
  is_wall: bool,
  h: int,
  g: int,
  f: int,
};

let create = (~x_id: int, ~y_id: int, ~node_id: int, ~is_wall: bool) => {
  x_id,
  y_id,
  is_wall,
  node_id,
  parent_id: (-1),
  top: (-1),
  bottom: (-1),
  left: (-1),
  right: (-1),
  h: 0,
  g: 0,
  f: 0,
};

let move_cost = 10;

let calc_h = (self: tile, end_node: tile) =>
  if (self.is_wall) {
    self.h;
  } else {
    let x_diff = self.x_id - end_node.x_id;
    let y_diff = self.y_id - end_node.y_id;
    (x_diff + y_diff) * move_cost;
  };

let calc_f_g = (self: tile, parent_g: int) => {
  ...self,
  g: parent_g + move_cost,
  f: self.g + self.h,
};

let reset = (self: tile, end_node: tile) => {
  ...self,
  parent_id: (-1),
  g: 0,
  f: 0,
  h: calc_h(self, end_node),
};