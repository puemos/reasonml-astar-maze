type state = {
  animation: SpringAnimation.t,
  value: float,
};
type action =
  | Value(float)
  | Start(float)
  | Stop;
let component = ReasonReact.reducerComponent("SpringComp");
let make = (~remoteAction, ~renderValue, _children) => {
  ...component,
  initialState: () => {animation: SpringAnimation.create(0.), value: 0.},
  didMount: ({send, onUnmount}) => {
    let token = RemoteAction.subscribe(~send, remoteAction);
    let cleanup = () =>
      switch (token) {
      | Some(token) => RemoteAction.unsubscribe(token)
      | None => ()
      };

    onUnmount(() => cleanup());

    send(Start(0.));
  },
  willUnmount: ({state}) => SpringAnimation.stop(state.animation),
  reducer: (action, state) =>
    switch (action) {
    | Value(value) => Update({...state, value})
    | Start(finalValue) =>
      UpdateWithSideEffects(
        {...state, value: 0.},
        (
          self =>
            state.animation
            |> SpringAnimation.setOnChange(
                 ~onChange=value => self.send(Value(value)),
                 ~finalValue,
                 ~speedup=0.99,
               )
        ),
      )

    | Stop =>
      state.animation |> SpringAnimation.stop;
      NoUpdate;
    },
  render: ({state}) => renderValue(state.value),
};