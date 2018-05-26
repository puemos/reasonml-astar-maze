exception Empty;

module type ORDERED = {type t; let compare: (t, t) => int;};

/* a totally ordered type and it's comparison function */
module type HEAP = {
  module Elem: ORDERED;
  type heap;
  let empty: heap;
  let isEmpty: heap => bool;
  let insert: (Elem.t, heap) => heap;
  let merge: (heap, heap) => heap;
  let findMin: heap => Elem.t; /* raises Empty if heap is empty */
  let deleteMin: heap => heap; /* raises Empty if heap is empty */
};

module BinomialHeap = (Element: ORDERED) => {
  module Elem = Element;
  type tree =
    | Node(int, Elem.t, list(tree));
  type heap = list(tree);
  let empty = [];
  let isEmpty =
    fun
    | [] => true
    | _ => false;
  let rank = ([@implicit_arity] Node(r, _, _)) => r;
  let root = ([@implicit_arity] Node(_, x, _)) => x;
  let link =
      (
        [@implicit_arity] Node(r, x1, l1) as t1,
        [@implicit_arity] Node(_, x2, l2) as t2,
      ) =>
    if (Elem.compare(x1, x2) < 0) {
      [@implicit_arity] Node(r + 1, x1, [t2, ...l1]);
    } else {
      [@implicit_arity] Node(r + 1, x2, [t1, ...l2]);
    };
  let rec insTree = (t, h) =>
    switch (h) {
    | [] => [t]
    | [hd, ...tl] =>
      if (rank(t) < rank(hd)) {
        [t, ...h];
      } else {
        insTree(link(hd, t), tl);
      }
    };
  let insert = x => insTree([@implicit_arity] Node(0, x, []));
  let rec merge = (h1, h2) =>
    switch (h1, h2) {
    | ([], _) => h2
    | (_, []) => h1
    | ([hd1, ...tl1], [hd2, ...tl2]) =>
      if (rank(hd1) < rank(hd2)) {
        [hd1, ...merge(tl1, h2)];
      } else if (rank(hd2) < rank(hd1)) {
        [hd2, ...merge(h1, tl2)];
      } else {
        insTree(link(hd1, hd2), merge(tl1, tl2));
      }
    };
  let rec removeMinTree =
    fun
    | [] => raise(Empty)
    | [hd] => (hd, [])
    | [hd, ...tl] => {
        let (t', ts') = removeMinTree(tl);
        if (root(hd) < root(t')) {
          (hd, tl);
        } else {
          (t', [hd, ...ts']);
        };
      };
  let findMin = h => {
    let (t, _) = removeMinTree(h);
    root(t);
  };
  let deleteMin = h => {
    let ([@implicit_arity] Node(_, _, ts'), ts) = removeMinTree(h);
    merge(List.rev(ts'), ts);
  };
};