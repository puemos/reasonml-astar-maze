open Tile;

open Belt;

type world = {
  width: int,
  height: int,
  tiles: array(tile),
};

let setRight =
    (~tiles: array(tile), ~numXtiles: int, ~numYtiles: int, self: tile) =>
  if (self.x + 1 >= numXtiles) {
    self;
  } else {
    let right = self.y * numXtiles + self.x + 1;
    let rightTile = tiles[right];
    switch (rightTile) {
    | None => self
    | Some(rightTile) => {...self, right: ! rightTile.is_wall}
    };
  };

let setLeft =
    (~tiles: array(tile), ~numXtiles: int, ~numYtiles: int, self: tile) =>
  if (self.x - 1 < 0) {
    self;
  } else {
    let left = self.y * numXtiles + self.x - 1;
    let leftTile = tiles[left];
    switch (leftTile) {
    | None => self
    | Some(leftTile) => {...self, left: ! leftTile.is_wall}
    };
  };

let setTop =
    (~tiles: array(tile), ~numXtiles: int, ~numYtiles: int, self: tile) =>
  if (self.y - 1 < 0) {
    self;
  } else {
    let top = (self.y - 1) * numXtiles + self.x;
    let topTile = tiles[top];
    switch (topTile) {
    | None => self
    | Some(topTile) => {...self, top: ! topTile.is_wall}
    };
  };

let setBottom =
    (~tiles: array(tile), ~numXtiles: int, ~numYtiles: int, self: tile) =>
  if (self.y + 1 >= numYtiles) {
    self;
  } else {
    let bottom = (self.y + 1) * numXtiles + self.x;
    let bottomTile = tiles[bottom];
    switch (bottomTile) {
    | None => self
    | Some(bottomTile) => {...self, bottom: ! bottomTile.is_wall}
    };
  };

let setAllTileSides = (self: world) : world => {
  let numXtiles = self.width;
  let numYtiles = self.height;
  let tiles = self.tiles;
  let setter = tile =>
    tile
    |> setRight(~numXtiles, ~numYtiles, ~tiles)
    |> setLeft(~numXtiles, ~numYtiles, ~tiles)
    |> setTop(~numXtiles, ~numYtiles, ~tiles)
    |> setBottom(~numXtiles, ~numYtiles, ~tiles);
  let tiles = Array.map(tiles, setter);
  {...self, tiles};
};

let getNodeId = (~x: int, ~y: int, ~width: int) => y * width + x;

let loadMap = (map: string) : world => {
  let rows =
    Js.String.split("\n", map) |> Array.map(_, Js.String.split(","));
  let tileMatrix =
    Array.mapWithIndex(rows, (y, row) =>
      Array.mapWithIndex(row, (x, col) =>
        Tile.create(
          ~x,
          ~y,
          ~is_wall=col == "1",
          ~node_id=getNodeId(~y, ~x, ~width=Array.length(row)),
        )
      )
    );
  let tiles =
    tileMatrix
    |> List.fromArray
    |> List.map(_, List.fromArray)
    |> List.flatten
    |> Belt.List.toArray;
  let height = Array.length(tileMatrix);
  let width = Array.length(tiles) / height;
  {width, height, tiles};
};

let makeTestWorld = () : world => {
  let testMap = "0,0,0,0,0,0\n0,0,0,0,0,0\n0,0,0,0,0,0\n0,0,0,0,0,0\n0,0,0,0,0,0\n0,0,0,0,0,0";
  testMap |> loadMap |> setAllTileSides;
};

let make = () : world => {width: 0, height: 0, tiles: [||]};