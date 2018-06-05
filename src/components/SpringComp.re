type state = {
  animation: SpringAnimation.t,
  value: float,
  target: float,
};
type action =
  | Value(float)
  | Start(float)
  | Stop;
let component = ReasonReact.reducerComponent("SpringComp");
let make = (~remoteAction, ~renderValue, ~initValue=0.0, _children) => {
  ...component,
  initialState: () => {
    animation: SpringAnimation.create(initValue),
    value: initValue,
    target: 0.,
  },
  didMount: ({state, send, onUnmount}) => {
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

    send(Start(0.));
  },
  reducer: (action, state) =>
    switch (action) {
    | Value(value) => Update({...state, value})
    | Start(finalValue) =>
      UpdateWithSideEffects(
        {...state, animation: SpringAnimation.create(0.), value: 0.},
        (
          self =>
            state.animation
            |> SpringAnimation.setOnChange(
                 ~onChange=value => self.send(Value(value)),
                 ~finalValue,
                 ~speedup=0.2
               )
        ),
      )

    | Stop =>
      state.animation |> SpringAnimation.stop;
      NoUpdate;
    },
  render: ({state}) => renderValue(state.value),
};