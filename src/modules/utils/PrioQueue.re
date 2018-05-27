module PrioQueue = {
  type priority = int;
  type queue('a) =
    | Empty
    | Node(priority, 'a, queue('a), queue('a));
  let empty = Empty;
  let rec insert = (queue, prio, elt) =>
    switch (queue) {
    | Empty => [@implicit_arity] Node(prio, elt, Empty, Empty)
    | [@implicit_arity] Node(p, e, left, right) =>
      if (prio <= p) {
        [@implicit_arity] Node(prio, elt, insert(right, p, e), left);
      } else {
        [@implicit_arity] Node(p, e, insert(right, prio, elt), left);
      }
    };
  let rec insertAll = (queue, items: list((priority, 'a))) =>
    switch (items) {
    | [] => queue
    | [head, ...rest] =>
      let (prio, elt) = head;
      let queue = insert(queue, prio, elt);
      insertAll(queue, rest);
    };
  exception Queue_is_empty;
  let rec remove_top =
    fun
    | Empty => raise(Queue_is_empty)
    | [@implicit_arity] Node(prio, elt, left, Empty) => left
    | [@implicit_arity] Node(prio, elt, Empty, right) => right
    | [@implicit_arity]
      Node(
        prio,
        elt,
        [@implicit_arity] Node(lprio, lelt, _, _) as left,
        [@implicit_arity] Node(rprio, relt, _, _) as right,
      ) =>
      if (lprio <= rprio) {
        [@implicit_arity] Node(lprio, lelt, remove_top(left), right);
      } else {
        [@implicit_arity] Node(rprio, relt, left, remove_top(right));
      };
  let extract =
    fun
    | Empty => raise(Queue_is_empty)
    | [@implicit_arity] Node(prio, elt, _, _) as queue => (
        prio,
        elt,
        remove_top(queue),
      );
};