type state = {
  animation: SpringAnimation.t,
  value: float,
  target: float,
};
type action =
  | Value(float)
  | Start
  | Stop;
let component = ReasonReact.reducerComponent("SpringComp");
let make =
    (~remoteAction, ~renderValue, ~initValue=0.0, ~targetValue=1.0, _children) => {
  ...component,
  initialState: () => {
    animation: SpringAnimation.create(initValue),
    value: initValue,
    target: targetValue,
  },
  didMount: ({state, send, onUnmount}) => {
    state.animation
    |> SpringAnimation.setOnChange(
         ~onChange=value => send(Value(value)),
         ~finalValue=state.target,
       );
    let token = RemoteAction.subscribe(~send, remoteAction);
    let cleanup = () =>
      switch (token) {
      | Some(token) => RemoteAction.unsubscribe(token)
      | None => ()
      };

    onUnmount(() => {
      SpringAnimation.stop(state.animation);
      cleanup();
    });
  },
  reducer: (action, state) =>
    switch (action) {
    | Value(value) => Update({...state, value})
    | Start =>
      SpringAnimation.start(state.animation);
      ReasonReact.NoUpdate;
    | Stop =>
      SpringAnimation.stop(state.animation);
      ReasonReact.NoUpdate;
    },
  render: ({state}) => renderValue(state.value),
};