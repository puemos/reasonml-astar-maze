type pos = (int, int);

type world = {
  width: int,
  height: int,
  walls: list(pos),
  food: list(pos),
};

type cellT =
  | Empty
  | Wall
  | Food
  | PlayerFood
  | Player;

module CellID =
  Belt.Id.MakeHashable({
    type t = pos;
    let hash = ((x, y): t) => x + y * 10;
    let eq = ((x1, y1): t, (x2, y2): t) => x1 == x2 && y1 == y2;
  });

let intOfCell = i =>
  switch (i) {
  | Empty => 0
  | Wall => 1
  | Food => 2
  | _ => 0
  };

let cellOfInt = cellInt =>
  switch (cellInt) {
  | 0 => Empty
  | 1 => Wall
  | 2 => Food
  | _ => Empty
  };

let getNodeId = (width: int, x: int, y: int) => y * width + x;

let getXY = (width: int, nodeId: int) => (nodeId mod width, nodeId / width);

let hashPosVector = (width: int, xs: list(pos)) =>
  xs
  |> Belt.List.map(_, ((x, y)) => getNodeId(width, x, y))
  |> Belt.List.reduce(_, 0, (p, c) => p lxor c);

let rec buildRow =
        (
          (px: int, py: int),
          row,
          ~food: list((int, int)),
          ~walls: list((int, int)),
        ) =>
  switch (row) {
  | [] => (food, walls)

  | [x, ...xs] =>
    switch (x) {
    | Wall =>
      buildRow((px + 1, py), xs, ~food, ~walls=[(px, py), ...walls])
    | Food => buildRow((px + 1, py), xs, ~food=[(px, py), ...food], ~walls)
    | _ => buildRow((px + 1, py), xs, ~food, ~walls)
    }
  };

let rec toState =
        (
          (px: int, py: int),
          matrix,
          ~food: list((int, int)),
          ~walls: list((int, int)),
        ) =>
  switch (matrix) {
  | [] => (food, walls)
  | [x, ...xs] =>
    let (food, walls) = buildRow((px, py), x, ~food, ~walls);
    toState((px, py + 1), xs, ~food, ~walls);
  };

let toCellMatrix = (map: array(array(int))) =>
  map
  |> Belt.Array.map(_, Belt.Array.map(_, cellOfInt))
  |> Belt.Array.map(_, Belt.List.fromArray)
  |> Belt.List.fromArray;

let loadMap = (map: array(array(int))) : world => {
  let cellMap = map |> toCellMatrix;
  let height = Belt.Array.length(map);
  let width =
    cellMap
    |> Belt.List.get(_, 0)
    |> Belt.Option.getWithDefault(_, [])
    |> Belt.List.length;
  let (food, walls) = toState((0, 0), cellMap, ~food=[], ~walls=[]);

  {width, height, food, walls};
};

let fromState = (~world, ~path, ~starting) => {
  let matrixSize = world.width * world.height;
  let cellHash = Belt.HashMap.make(~hintSize=10, ~id=(module CellID));

  world.walls
  |> Belt.List.forEach(_, point => Belt.HashMap.set(cellHash, point, Wall));

  world.food
  |> Belt.List.forEach(_, point => Belt.HashMap.set(cellHash, point, Food));

  [starting, ...path]
  |> Belt.List.forEach(
       _,
       point => {
         let type_ =
           switch (Belt.HashMap.get(cellHash, point)) {
           | None => Player
           | Some(Food) => PlayerFood
           | _ => Player
           };
         Belt.HashMap.set(cellHash, point, type_);
       },
     );

  matrixSize
  |> Belt.List.make(_, None)
  |> Belt.List.mapWithIndex(_, (idx, _) => getXY(world.width, idx))
  |> Belt.List.map(_, point =>
       switch (Belt.HashMap.get(cellHash, point)) {
       | None => Empty
       | Some(type_) => type_
       }
     )
  |> Belt.List.toArray
  /* |> Belt.Array.reverse */
  |> Belt.List.fromArray
  |> Rationale.RList.splitEvery(world.width)
  |> Belt.List.map(_, xs => xs |> Belt.List.toArray)
  |> Belt.List.toArray;
};

let stringToMatrix = (map: string) =>
  Js.String.split("\n", map)
  |> Belt.List.fromArray
  |> Belt.List.map(_, Js.String.split(","))
  |> Belt.List.map(_, Belt.List.fromArray)
  |> Belt.List.map(_, Belt.List.map(_, int_of_string));

let make = map => loadMap(map);