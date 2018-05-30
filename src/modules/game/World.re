type pos = (int, int);

type world = {
  width: int,
  height: int,
  walls: list(pos),
  food: list(pos),
};

let getNodeId = (~x: int, ~y: int, ~width: int) => y * width + x;
let getXY = (width: int, nodeId: int) => (nodeId mod width, nodeId / width);

let rec buildRow =
        (
          (xx: int, yy: int),
          row,
          ~food: list((int, int)),
          ~walls: list((int, int)),
        ) =>
  switch (row) {
  | [] => (food, walls)
  | [x] =>
    switch (x) {
    | "1" => buildRow((xx + 1, yy), [], ~food, ~walls=[(xx, yy), ...walls])
    | "2" => buildRow((xx + 1, yy), [], ~food=[(xx, yy), ...food], ~walls)
    | _ => buildRow((xx + 1, yy), [], ~food, ~walls)
    }
  | [x, ...xs] =>
    switch (x) {
    | "1" => buildRow((xx + 1, yy), xs, ~food, ~walls=[(xx, yy), ...walls])
    | "2" => buildRow((xx + 1, yy), xs, ~food=[(xx, yy), ...food], ~walls)
    | _ => buildRow((xx + 1, yy), xs, ~food, ~walls)
    }
  };

let rec buildMatrix =
        (
          (xx: int, yy: int),
          matrix,
          ~food: list((int, int)),
          ~walls: list((int, int)),
        ) =>
  switch (matrix) {
  | [] => (food, walls)
  | [x] =>
    let (food, walls) = buildRow((xx, yy), x, ~food, ~walls);
    buildMatrix((xx, yy + 1), [], ~food, ~walls);

  | [x, ...xs] =>
    let (food, walls) = buildRow((xx, yy), x, ~food, ~walls);
    buildMatrix((xx, yy + 1), xs, ~food, ~walls);
  };

let loadMap = (map: string) : world => {
  let matrix =
    Js.String.split("\n", map)
    |> Belt.List.fromArray
    |> Belt.List.map(_, Js.String.split(","))
    |> Belt.List.map(_, Belt.List.fromArray);

  let height = Belt.List.length(matrix);
  let width =
    matrix
    |> Belt.List.get(_, 0)
    |> Belt.Option.getWithDefault(_, [])
    |> Belt.List.length;
  let (food, walls) = buildMatrix((0, 0), matrix, ~food=[], ~walls=[]);

  {width, height, food, walls};
};

let makeTestWorld = () => {
  /* let testMap = "0,0,1,1,1,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,1,0,1,0,0,1,0,0,0,1,1,0,1,0,0,0\n0,0,1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,1,1,0,0,0,0,0,0,1,0,0,0,1,0,1,0,1,1,1\n0,1,0,0,1,0,1,0,1,1,0,0,0,0,1,0,0,0,0,1,0,1,0,1,1,0,0,0,1,0,0,1,0,0,1,1\n0,0,0,0,1,0,1,1,0,0,1,0,1,0,0,0,0,0,1,0,0,0,0,1,1,0,0,0,0,0,0,1,0,0,0,1\n1,0,1,0,1,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,1,0,0,0,1,0,0,0\n0,0,0,0,1,1,0,0,0,1,0,0,1,1,1,0,1,0,0,0,0,0,1,0,1,0,0,0,0,1,0,0,1,0,0,0\n1,0,0,0,1,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,2,0,0,1,0,0,0,1,0,0,0,0\n0,2,1,1,1,1,0,1,0,0,1,0,0,0,1,0,0,1,1,0,0,0,0,1,0,0,0,0,1,1,1,0,1,1,1,0\n0,0,0,0,0,1,0,0,0,0,1,1,1,1,1,0,0,1,0,0,0,0,0,1,1,0,0,0,0,0,1,0,1,1,0,1\n0,1,0,1,0,0,0,0,0,0,0,0,1,0,1,0,1,0,0,0,0,0,0,1,0,1,0,0,0,1,0,0,0,0,0,0\n0,1,0,1,0,1,1,0,1,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,1,0,0,0,1,1,0,0,0,1,1,0\n1,0,1,0,1,0,0,0,1,0,1,0,1,1,0,0,1,1,0,1,0,1,0,0,0,1,0,1,0,0,0,0,0,1,0,0\n0,0,0,0,1,0,0,0,1,0,0,1,0,0,1,1,0,0,0,1,1,1,0,0,0,1,0,0,0,0,1,0,1,0,1,0\n0,0,0,0,0,0,1,1,0,0,1,1,0,0,1,0,0,0,1,0,1,1,0,0,0,0,1,1,1,0,0,1,0,0,0,0\n1,0,0,1,0,1,0,0,1,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0\n0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,1,1,1,0,0,0,0,0,1,0,0,1,0,1,0,0,0,1,0\n0,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0,0\n0,0,0,1,0,0,0,0,0,0,0,1,0,0,1,0,1,1,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,1,0,0\n0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,1,0,0,0,1,0,0,1,1,0\n1,0,0,0,0,1,0,0,1,1,0,1,0,0,1,0,1,0,0,0,0,0,0,1,1,1,0,1,0,0,0,0,1,1,0,1\n0,1,1,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0\n1,0,0,0,1,0,1,0,0,1,0,0,1,1,1,0,1,0,0,0,1,1,1,0,0,0,1,1,0,0,0,1,0,0,0,0\n0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,1,0,1,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,1,1,0\n0,0,0,0,0,0,1,1,0,0,0,1,1,1,0,0,0,0,0,0,0,0,1,1,1,0,1,0,1,0,1,0,1,0,0,0"; */
  let testMap = "0,0,0\n0,0,0\n0,0,2\n0,0,1\n0,2,0\n0,2,0";
  testMap |> loadMap;
};

let make = () => makeTestWorld();