open World;

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

let tileWrapper = (~isWall) =>
  Css.[
    width(px(20)),
    height(px(20)),
    flexDirection(column),
    alignItems(center),
    justifyContent(center),
    backgroundColor(isWall ? black : grey),
    position(relative),
  ];
let tile =
  Css.[
    display(flexBox),
    width(px(10)),
    height(px(10)),
    backgroundColor(pink),
  ];
let food =
  Css.[
    position(absolute),
    display(flexBox),
    top(px(8)),
    right(px(8)),
    width(px(4)),
    height(px(4)),
    backgroundColor(green),
  ];

let component = ReasonReact.statelessComponent("Cell");

module Cell = {
  let component = ReasonReact.statelessComponent("Cell");
  let make = (~type_, _) => {
    ...component,
    render: _ =>
      switch (type_) {
      | Empty => <div className=(Css.style(tileWrapper(~isWall=false))) />
      | Wall => <div className=(Css.style(tileWrapper(~isWall=true))) />
      | Player =>
        <div className=(Css.style(tileWrapper(~isWall=false)))>
          <div className=(Css.style(tile)) />
        </div>
      | Food =>
        <div className=(Css.style(tileWrapper(~isWall=false)))>
          <div className=(Css.style(food)) />
        </div>
      | PlayerFood =>
        <div className=(Css.style(tileWrapper(~isWall=false)))>
          <div className=(Css.style(tile))>
            <div className=(Css.style(food)) />
          </div>
        </div>
      },
  };
};