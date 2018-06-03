open Game;

Css.(
  global("html, body", [margin(zero), marginLeft(px(5)), padding(zero)])
);

ReactDOMRe.renderToElementWithId(<Game />, "root");