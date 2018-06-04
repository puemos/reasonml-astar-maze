open World;
open Grid;
open Cell;

let text = ReasonReact.string;

module Problem = SearchProblem.Make(PositionSearchProblem);
module GS = GraphSearch.Make(Problem);

let createMatrix = (~world, ~path, ~starting) => {
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
  |> Belt.List.mapWithIndex(_, (idx, _) => World.getXY(world.width, idx))
  |> Belt.List.map(_, point =>
       switch (Belt.HashMap.get(cellHash, point)) {
       | None => Empty
       | Some(type_) => type_
       }
     )
  |> Belt.List.toArray
  |> Belt.Array.reverse
  |> Belt.List.fromArray
  |> Rationale.RList.splitEvery(world.width)
  |> Belt.List.map(_, xs => xs |> Belt.List.toArray |> Belt.Array.reverse)
  |> Belt.List.toArray;
};

let search = () => {
  let startState =
    PositionSearchProblem.getStartState({
      world: World.make(World.testMap),
      player: (0, 0),
      path: [],
    });

  GS.search(startState, FoodAgent.heuristic)
  |> Belt.List.map(_, ({path}) =>
       createMatrix(
         ~path,
         ~world=startState.world,
         ~starting=startState.player,
       )
     );
};

module Game = {
  let styles =
    Css.(
      {
        "game": [
          display(flexBox),
          flexDirection(column),
          alignItems(center),
          justifyContent(center),
          width(pct(100.)),
          height(pct(100.)),
        ],
        "title": [color(hex("E2E2E2"))],
        "controls": [
          selector(
            "& > button",
            [
              selector("&:hover", [border(px(1), solid, hex("FFE400"))]),
              border(px(1), solid, white),
              background(transparent),
              color(hex("FFE400")),
              width(px(200)),
              paddingTop(px(12)),
              paddingBottom(px(12)),
              textTransform(uppercase),
              margin(px(5)),
              cursor(`pointer),
            ],
          ),
          display(flexBox),
          flexDirection(row),
          alignItems(center),
          justifyContent(center),
          marginBottom(px(16)),
        ],
      }
    );
  type action =
    | Restart
    | Play
    | Pause;

  type state = {
    steps: list(array(array(cellT))),
    pause: bool,
  };
  let component = ReasonReact.reducerComponent("Game");
  let remoteAction = RemoteAction.create();

  let renderValue = (steps, value) =>
    <Grid matrix=(Belt.List.getExn(steps, int_of_float(value))) />;
  let remoteAction = RemoteAction.create();

  let make = _ => {
    ...component,
    initialState: () => {
      let steps = search();
      {steps, pause: true};
    },
    reducer: (action, state) =>
      switch (action) {
      | Restart =>
        RemoteAction.send(remoteAction, ~action=SpringComp.Value(0.0));
        ReasonReact.Update({...state, pause: false});
      | Pause =>
        RemoteAction.send(remoteAction, ~action=SpringComp.Stop);

        ReasonReact.Update({...state, pause: true});
      | Play =>
        RemoteAction.send(remoteAction, ~action=SpringComp.Start);
        ReasonReact.Update({...state, pause: false});
      },
    render: ({send, state: {steps, pause}}) =>
      <div className=(Css.style(styles##game))>
        <h1 className=(Css.style(styles##title))>
          (text("Maze Eat&Find"))
        </h1>
        <div className=(Css.style(styles##controls))>
          <button onClick=(_ => send(Restart))> (text("Restart")) </button>
          <button onClick=(_ => send(pause ? Play : Pause))>
            (text(pause ? "Play" : "Pause"))
          </button>
        </div>
        <SpringComp
          remoteAction
          targetValue=(float_of_int(Belt.List.length(steps) - 1))
          renderValue=(renderValue(steps))
        />
      </div>,
  };
};