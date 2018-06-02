type order('a) = ('a, 'a) => bool;

type queue('a) = {
  heap: DynArray.t('a),
  indices: Hashtbl.t('a, int),
  order: order('a),
};

type t('a) = queue('a);

let make = order => {
  heap: DynArray.make(0, Obj.magic(0)),
  indices: Hashtbl.create(32),
  order,
};

let length = h => DynArray.length(h.heap);

let is_empty = h => length(h) == 0;

let get = h => DynArray.unsafe_get(h.heap);

let set = (h, i, x) => {
  DynArray.unsafe_set(h.heap, i, x);
  Hashtbl.replace(h.indices, x, i);
};

let mem = (h, x) => Hashtbl.mem(h.indices, x);

let parent = i => (i - 1) / 2;

let left = i => 2 * i + 1;

let right = i => 2 * i + 2;

let has_left = (h, i) => left(i) < length(h);

let has_right = (h, i) => right(i) < length(h);

let is_heap = h => {
  let ord = h.order;
  let rec is_heap = i =>
    (
      ! has_left(h, i)
      || ord(get(h, i), get(h, left(i)))
      && is_heap(left(i))
    )
    && (
      ! has_right(h, i)
      || ord(get(h, i), get(h, right(i)))
      && is_heap(right(i))
    );
  is_heap(0);
};

let down_heap = (h, i) => {
  let x = get(h, i);
  let ord = h.order;
  let rec down_heap = j =>
    if (has_left(h, j)) {
      let l = left(j);
      let r = right(j);
      let k =
        if (has_right(h, j) && ! ord(get(h, l), get(h, r))) {
          r;
        } else {
          l;
        };
      let y = get(h, k);
      if (ord(x, y)) {
        set(h, j, x);
      } else {
        set(h, j, y);
        down_heap(k);
      };
    } else if (j != i) {
      set(h, j, x);
    };
  down_heap(i);
};

let up_heap = (h, i) => {
  let x = get(h, i);
  let ord = h.order;
  let rec up_heap = j => {
    let k = parent(j);
    let y = get(h, k);
    if (j == 0 || ord(y, x)) {
      set(h, j, x);
    } else {
      set(h, j, y);
      up_heap(k);
    };
  };
  up_heap(i);
};

let make_heap = h =>
  for (i in length(h) / 2 - 1 downto 0) {
    down_heap(h, i);
  };

let first = h =>
  if (is_empty(h)) {
    failwith("PriorityQueue.first: empty queue");
  } else {
    get(h, 0);
  };

let add = (h, x) => {
  let i = length(h);
  DynArray.add(h.heap, x);
  Hashtbl.add(h.indices, x, i);
  up_heap(h, i);
};

let remove_index = (h, i) => {
  let x = get(h, i);
  let y = get(h, length(h) - 1);
  set(h, i, y);
  DynArray.remove_last(h.heap);
  Hashtbl.remove(h.indices, x);
  down_heap(h, i);
};

let remove_first = h =>
  if (is_empty(h)) {
    failwith("PriorityQueue.first: empty queue");
  } else {
    remove_index(h, 0);
  };

let remove = (h, x) =>
  try (remove_index(h, Hashtbl.find(h.indices, x))) {
  | Not_found => ()
  };

let clear = h => {
  DynArray.clear(h.heap);
  Hashtbl.clear(h.indices);
};

let reorder_up = (h, x) =>
  try (up_heap(h, Hashtbl.find(h.indices, x))) {
  | Not_found => ()
  };

let reorder_down = (h, x) =>
  try (down_heap(h, Hashtbl.find(h.indices, x))) {
  | Not_found => ()
  };

module type HashedType = {
  type t;
  let equal: (t, t) => bool;
  let hash: t => int;
};

module type S = {
  type elt;
  type order = (elt, elt) => bool;
  type t;
  let make: order => t;
  let length: t => int;
  let is_empty: t => bool;
  let add: (t, elt) => unit;
  let mem: (t, elt) => bool;
  let first: t => elt;
  let remove_first: t => unit;
  let remove: (t, elt) => unit;
  let clear: t => unit;
  let reorder_up: (t, elt) => unit;
  let reorder_down: (t, elt) => unit;
};

module Make = (H: HashedType) : (S with type elt := H.t) => {
  type elt = H.t;
  type order = (elt, elt) => bool;
  module Tbl = Hashtbl.Make(H);
  type queue = {
    heap: DynArray.t(elt),
    indices: Tbl.t(int),
    order,
  };
  type t = queue;
  let make = order => {
    heap: DynArray.make(0, Obj.magic(0)),
    indices: Tbl.create(32),
    order,
  };
  let length = h => DynArray.length(h.heap);
  let is_empty = h => length(h) == 0;
  let get = h => DynArray.unsafe_get(h.heap);
  let set = (h, i, x) => {
    DynArray.unsafe_set(h.heap, i, x);
    Tbl.replace(h.indices, x, i);
  };
  let mem = (h, x) => Tbl.mem(h.indices, x);
  let parent = i => (i - 1) / 2;
  let left = i => 2 * i + 1;
  let right = i => 2 * i + 2;
  let has_left = (h, i) => left(i) < length(h);
  let has_right = (h, i) => right(i) < length(h);
  let is_heap = h => {
    let ord = h.order;
    let rec is_heap = i =>
      (
        ! has_left(h, i)
        || ord(get(h, i), get(h, left(i)))
        && is_heap(left(i))
      )
      && (
        ! has_right(h, i)
        || ord(get(h, i), get(h, right(i)))
        && is_heap(right(i))
      );
    is_heap(0);
  };
  let down_heap = (h, i) => {
    let x = get(h, i);
    let ord = h.order;
    let rec down_heap = j =>
      if (has_left(h, j)) {
        let l = left(j);
        let r = right(j);
        let k =
          if (has_right(h, j) && ! ord(get(h, l), get(h, r))) {
            r;
          } else {
            l;
          };
        let y = get(h, k);
        if (ord(x, y)) {
          set(h, j, x);
        } else {
          set(h, j, y);
          down_heap(k);
        };
      } else if (j != i) {
        set(h, j, x);
      };
    down_heap(i);
  };
  let up_heap = (h, i) => {
    let x = get(h, i);
    let ord = h.order;
    let rec up_heap = j => {
      let k = parent(j);
      let y = get(h, k);
      if (j == 0 || ord(y, x)) {
        set(h, j, x);
      } else {
        set(h, j, y);
        up_heap(k);
      };
    };
    up_heap(i);
  };
  let make_heap = h =>
    for (i in length(h) / 2 - 1 downto 0) {
      down_heap(h, i);
    };
  let first = h =>
    if (is_empty(h)) {
      failwith("PriorityQueue.first: empty queue");
    } else {
      get(h, 0);
    };
  let add = (h, x) => {
    let i = length(h);
    DynArray.add(h.heap, x);
    Tbl.add(h.indices, x, i);
    up_heap(h, i);
  };
  let remove_index = (h, i) => {
    let x = get(h, i);
    let y = get(h, length(h) - 1);
    set(h, i, y);
    DynArray.remove_last(h.heap);
    Tbl.remove(h.indices, x);
    down_heap(h, i);
  };
  let remove_first = h =>
    if (is_empty(h)) {
      failwith("PriorityQueue.first: empty queue");
    } else {
      remove_index(h, 0);
    };
  let remove = (h, x) =>
    try (remove_index(h, Tbl.find(h.indices, x))) {
    | Not_found => ()
    };
  let clear = h => {
    DynArray.clear(h.heap);
    Tbl.clear(h.indices);
  };
  let reorder_up = (h, x) =>
    try (up_heap(h, Tbl.find(h.indices, x))) {
    | Not_found => ()
    };
  let reorder_down = (h, x) =>
    try (down_heap(h, Tbl.find(h.indices, x))) {
    | Not_found => ()
    };
};