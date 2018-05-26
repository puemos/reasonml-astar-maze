open Tile;

type world = {
  debug: bool,
  window_width: int,
  window_height: int,
  width: int,
  height: int,
  quality: int,
  tile_size: int,
  start_id: int,
  end_id: int,
  player: (int, int),
  tiles: list(tile),
  recent_regen: bool,
};

let load_map = (map: string) => {
  let new_line = "\n";
  let comma = ",";
  let rows = Js.String.split(new_line, map);
  let rows =
    rows
    |> Array.map(Js.String.split(comma))
    |> Array.map(Array.to_list)
    |> Array.to_list;
  let to_flat =
    List.mapi(
      (i, row) =>
        List.mapi(
          (j, col) =>
            Tile.create(~x_id=i, ~y_id=j, ~is_wall=col == "1", ~node_id=1),
          row,
        ),
      rows,
    );
  List.flatten(to_flat);
};



let create = () : world => {
  let quality: int = 2;
  let width: int = 900 * quality;
  let height: int = 600 * quality;
  let tile_size: int = 50;
  {
    debug: false,
    window_width: 0,
    window_height: 0,
    width,
    height,
    quality,
    tile_size,
    tiles: [],
    player: (0, 0),
    start_id: (-1),
    end_id: (-1),
    recent_regen: false,
  };
};