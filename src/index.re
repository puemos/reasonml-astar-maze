[@bs.module "./registerServiceWorker"]
external register_service_worker : unit => unit = "default";

open GameState;

let shit = GraphSearch.graphSearch();

let shit =
  List.map(
    ac =>
      switch (ac) {
      | Right => "Right"
      | Left => "Left"
      | Top => "Top"
      | Bottom => "Bottom"
      },
    shit,
  );

Js.log(shit);
ReactDOMRe.renderToElementWithId(<div />, "root");

register_service_worker();