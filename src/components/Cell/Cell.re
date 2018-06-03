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

module Styles = {
  open Css;
  let tileWrapper = (~isWall) =>
    style([
      width(px(20)),
      height(px(20)),
      flexDirection(column),
      alignItems(center),
      justifyContent(center),
      backgroundColor(isWall ? black : grey),
      position(relative),
    ]);
  let tile =
    style([
      display(flexBox),
      width(px(10)),
      height(px(10)),
      backgroundColor(pink),
    ]);
  let food =
    style([
      position(absolute),
      display(flexBox),
      top(px(8)),
      right(px(8)),
      width(px(4)),
      height(px(4)),
      backgroundColor(green),
    ]);
};

let component = ReasonReact.statelessComponent("Cell");

let make = (~type_, _children) => {
  ...component,
  render: self =>
    switch (type_) {
    | Empty => <div className=(Styles.tileWrapper(~isWall=false)) />
    | Wall => <div className=(Styles.tileWrapper(~isWall=true)) />
    | Player =>
      <div className=(Styles.tileWrapper(~isWall=false))>
        <div className=Styles.tile />
      </div>
    | Food =>
      <div className=(Styles.tileWrapper(~isWall=false))>
        <div className=Styles.food />
      </div>
    | PlayerFood =>
      <div className=(Styles.tileWrapper(~isWall=false))>
        <div className=Styles.tile> <div className=Styles.food /> </div>
      </div>
    },
};