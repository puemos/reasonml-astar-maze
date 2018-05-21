open Problem;

type node = {
  state,
  parent: node,
  actions: list(action),
  pathCost: int,
  depth: int,
};