open World;

let tileWrapper = (~isWall) =>
  Css.[
    selector("&[disabled]", [cursor(`default)]),
    selector(
      "&:not([disabled])",
      [
        cursor(`pointer),
        selector(
          "&:hover",
          [
            zIndex(2),
            boxShadow(~blur=px(0), ~spread=px(1), hex("FFE400")),
          ],
        ),
      ],
    ),
    borderWidth(px(0)),
    outlineStyle(`none),
    cursor(`default),
    minWidth(px(20)),
    minHeight(px(20)),
    flexDirection(column),
    alignItems(center),
    justifyContent(center),
    backgroundColor(isWall ? hex("4E4E50") : transparent),
    position(relative),
  ];
let player =
  Css.[
    position(absolute),
    display(flexBox),
    width(px(10)),
    height(px(10)),
    top(px(5)),
    right(px(5)),
    backgroundColor(hex("385889")),
  ];
let food =
  Css.[
    position(absolute),
    display(flexBox),
    borderRadius(pct(50.0)),
    top(px(8)),
    right(px(8)),
    width(px(4)),
    height(px(4)),
    backgroundColor(hex("FFE400")),
  ];

module Cell = {
  let component = ReasonReact.statelessComponent("Cell");
  let handleClick = (disabled, fn) => disabled ? _ => () : fn;
  let make = (~type_, ~onClick, ~disabled, _) => {
    ...component,
    render: _ =>
      switch (type_) {
      | Empty =>
        <button
          disabled
          onClick=(handleClick(disabled, onClick))
          className=(Css.style(tileWrapper(~isWall=false)))
        />
      | Wall =>
        <button
          disabled
          onClick=(handleClick(disabled, onClick))
          className=(Css.style(tileWrapper(~isWall=true)))
        />
      | Player =>
        <button disabled className=(Css.style(tileWrapper(~isWall=false)))>
          <div className=(Css.style(player)) />
        </button>
      | Food =>
        <button
          disabled
          onClick=(handleClick(disabled, onClick))
          className=(Css.style(tileWrapper(~isWall=false)))>
          <div className=(Css.style(food)) />
        </button>
      | PlayerFood =>
        <button disabled className=(Css.style(tileWrapper(~isWall=false)))>
          <div className=(Css.style(player)) />
          <div className=(Css.style(food)) />
        </button>
      },
  };
};