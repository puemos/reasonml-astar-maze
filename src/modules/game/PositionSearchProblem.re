open GameState;
open World;

module Make = (()) => {
  type t = {
    player: pos,
    world,
  };

  type action = GameState.dir;

  let euclideanDistanceSort = xs =>
    xs
    |> Belt.List.sort(_, (p1, p2) =>
         Distance.euclideanDistance((0, 0), p1)
         - Distance.euclideanDistance((0, 0), p2)
       );

  let equal = (a: t, b: t) => {
    let (x1, y1) = a.player;
    let (x2, y2) = b.player;
    let playerEq = x1 === x2 && y1 === y2;
    let food1 =
      a.world.food
      |> euclideanDistanceSort
      |> Belt.List.map(_, ((x, y)) => World.getNodeId(a.world.width, x, y));
    let food2 =
      b.world.food
      |> euclideanDistanceSort
      |> Belt.List.map(_, ((x, y)) => World.getNodeId(b.world.width, x, y));
    let foodEq = Belt.List.eq(food1, food2, (i1, i2) => i1 === i2);
    playerEq && foodEq;
  };

  let hash = (a: t) => {
    let (x, y) = a.player;
    hashPosVector(a.world.width, a.world.food) lxor x lxor y;
  };

  let getStartState = () => {world: World.make(), player: (0, 0)};

  let isGoalState = state => Belt.List.length(state.world.food) == 0;

  let getCostOfActions = actions => List.length(actions);

  let getSuccessors = (state: t) =>
    GameState.getLegalActions(state.world, state.player)
    |> Belt.List.map(
         _,
         (action: action) => {
           let (dx, dy) = GameState.actionToVector(action);
           let {player, world} = state;
           let {food} = world;
           let (px, py) = player;
           let nextPlayer = (px + dx, py + dy);
           let nextFood = Belt.List.keep(food, i => i != nextPlayer);
           let nextWorld = {...world, food: nextFood};
           ({world: nextWorld, player: nextPlayer}, action, 1);
         },
       );
};