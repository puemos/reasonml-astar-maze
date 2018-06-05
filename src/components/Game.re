open World;
open Grid;
open Cell;

let text = ReasonReact.string;

module Problem = SearchProblem.Make(PositionSearchProblem);
module GS = GraphSearch.Make(Problem);

let search = (~player=(0, 0), ~map) => {
  let world = World.make(map);
  let startState =
    PositionSearchProblem.getStartState({world, player, path: []});

  GS.search(startState, FoodAgent.heuristic)
  |> Belt.List.map(_, ({path}) =>
       World.fromState(
         ~path,
         ~world=startState.world,
         ~starting=startState.player,
       )
     );
};

let map2CellMatrix = (ls: array(array(int))) =>
  ls |> Belt.Array.map(_, Belt.Array.map(_, World.cellOfInt));
/* |> Belt.Array.reverse; */

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
        "searching": [color(hex("5764cc")), marginTop(vh(20.))],
        "title": [marginTop(px(50)), color(hex("E2E2E2"))],
        "controls": [
          selector(
            "& > button",
            [
              selector("&:hover", [border(px(1), solid, hex("FFE400"))]),
              selector(
                "&[disabled]",
                [
                  opacity(0.2),
                  cursor(`default),
                  border(px(1), solid, white),
                ],
              ),
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
    | Rollback
    | Start
    | Edit
    | Search
    | ChangeCell(pos, cellT);

  type state = {
    steps: list(array(array(cellT))),
    map: array(array(int)),
    changed: bool,
    edit: bool,
    searching: bool,
  };
  let component = ReasonReact.reducerComponent("Game");

  let renderValue = (steps, value) =>
    <Grid
      onCellClick=((_, _) => ())
      matrix=(
        Belt.List.getExn(
          steps,
          min(Belt.List.length(steps) - 1, int_of_float(value)),
        )
      )
    />;

  let remoteAction = RemoteAction.create();

  let make = _ => {
    ...component,
    initialState: () => {
      let map = Maps.allMaps[0];
      let steps = search(~player=(0, 0), ~map);
      {steps, map, changed: false, edit: false, searching: false};
    },
    reducer: (action, state) =>
      switch (action) {
      | Edit => Update({...state, edit: ! state.edit})
      | ChangeCell((px, py), c) =>
        state.map[py][px] = intOfCell(c);
        Update({...state, map: state.map, changed: true});
      | Rollback =>
        SideEffects(
          (
            _ => RemoteAction.send(remoteAction, ~action=SpringComp.Start(0.))
          ),
        )
      | Search =>
        UpdateWithSideEffects(
          {...state, changed: false, searching: true},
          (
            ({send}) => {
              Js.Global.setTimeout(() => send(Start), 10) |> ignore;
              ();
            }
          ),
        )
      | Start =>
        UpdateWithSideEffects(
          {
            ...state,
            searching: false,
            steps: search(~player=(0, 0), ~map=state.map),
          },
          (
            ({state}) =>
              RemoteAction.send(
                remoteAction,
                ~action=
                  SpringComp.Start(
                    float_of_int(Belt.List.length(state.steps) - 1),
                  ),
              )
          ),
        )
      },
    didMount: ({send}) => send(Rollback),
    render: ({send, state: {map, steps, edit, changed, searching}}) =>
      <div className=(Css.style(styles##game))>
        <h1 className=(Css.style(styles##title))>
          (text("Maze Eat&Find"))
        </h1>
        <div className=(Css.style(styles##controls))>
          <button disabled=(edit || searching) onClick=(_ => send(Rollback))>
            (text("Rollback"))
          </button>
          <button disabled=(edit || searching) onClick=(_ => send(Search))>
            (text("Search"))
          </button>
          <button disabled=searching onClick=(_ => send(Edit))>
            (text(edit ? "Done" : "Edit"))
          </button>
        </div>
        (
          switch (edit, changed, searching) {
          | (_, _, true) =>
            <div className=(Css.style(styles##searching))>
              (text("Searching..."))
            </div>
          | (true, _, _) =>
            <Grid
              matrix=(map2CellMatrix(map))
              onCellClick=((p, c) => send(ChangeCell(p, c)))
            />
          | (false, true, _) =>
            <Grid matrix=(map2CellMatrix(map)) onCellClick=((_, _) => ()) />
          | _ => <SpringComp remoteAction renderValue=(renderValue(steps)) />
          }
        )
      </div>,
  };
};