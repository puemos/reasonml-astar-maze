open Game;

[@bs.module "./registerServiceWorker"]
external register_service_worker : unit => unit = "default";

Css.(
  global(
    "html, body",
    [
      margin(zero),
      marginLeft(px(5)),
      padding(zero),
      background(hex("272727")),
    ],
  )
);

ReactDOMRe.renderToElementWithId(<Game />, "root");

register_service_worker();