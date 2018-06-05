open World;
open Grid;
open Cell;

let text = ReasonReact.string;

module Problem = SearchProblem.Make(PositionSearchProblem);
module GS = GraphSearch.Make(Problem);

let search = () => {
  let startState =
    PositionSearchProblem.getStartState({
      world: World.make(Maps.allMaps[0]),
      player: (0, 0),
      path: [],
    });

  GS.search(startState, FoodAgent.heuristic)
  |> Belt.List.map(_, ({path}) =>
       World.fromState(
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
    | Reset
    | Start;

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
      | Reset =>
        RemoteAction.send(remoteAction, ~action=SpringComp.Start(0.));
        ReasonReact.Update({...state, pause: false});
      | Start =>
        RemoteAction.send(
          remoteAction,
          ~action=
            SpringComp.Start(
              float_of_int(Belt.List.length(state.steps) - 1),
            ),
        );
        ReasonReact.Update({...state, pause: false});
      },
    didMount: ({send}) => send(Reset),
    render: ({send, state: {steps, pause}}) =>
      <div className=(Css.style(styles##game))>
        <h1 className=(Css.style(styles##title))>
          (text("Maze Eat&Find"))
        </h1>
        <div className=(Css.style(styles##controls))>
          <button onClick=(_ => send(Reset))> (text("Reset")) </button>
          <button onClick=(_ => send(Start))> (text("Start")) </button>
        </div>
        <SpringComp remoteAction renderValue=(renderValue(steps)) />
      </div>,
  };
};