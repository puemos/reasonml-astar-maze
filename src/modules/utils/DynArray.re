type resizer = (~curr_size: int, ~old_length: int, ~new_length: int) => int;

type t('a) = {
  mutable data: array('a),
  mutable size: int,
  mutable length: int,
  mutable resizer,
};

let make_array = (data, resizer) => {
  let n = Array.length(data);
  {data, size: n, length: n, resizer};
};

let is_valid_index = (a, i) => 0 <= i && i < a.length;

let is_valid_insertion_index = (a, i) => 0 <= i && i <= a.length;

let is_valid_sub = (a, i, n) => 0 <= n && 0 <= i && i + n <= a.length;

let get = a => Array.get(a.data);

let set = a => Array.set(a.data);

let unsafe_get = a => Array.unsafe_get(a.data);

let unsafe_set = a => Array.unsafe_set(a.data);

let length = a => a.length;

let is_empty = a => a.length == 0;

let first = a =>
  if (a.length > 0) {
    Some(unsafe_get(a, 0));
  } else {
    None;
  };

let last = a =>
  if (a.length > 0) {
    Some(unsafe_get(a, a.length - 1));
  } else {
    None;
  };

let doubling_resizer_with_shrinking = (~curr_size, ~old_length, ~new_length) => {
  assert(new_length >= 0);
  let rec grow = size =>
    if (size >= new_length) {
      size;
    } else if (size < 0) {
      new_length;
    } else {
      grow(2 * size);
    };
  let rec shrink = size =>
    if (size / 2 <= new_length) {
      size;
    } else {
      shrink(size / 2);
    };
  if (curr_size > 0) {
    if (new_length > curr_size) {
      grow(curr_size);
    } else if (new_length < curr_size / 4) {
      shrink(curr_size);
    } else {
      curr_size;
    };
  } else {
    new_length;
  };
};

let doubling_resizer_without_shrinking =
    (~curr_size, ~old_length, ~new_length) => {
  assert(new_length >= 0);
  let rec grow = size =>
    if (size >= new_length) {
      size;
    } else if (size < 0) {
      new_length;
    } else {
      grow(2 * size);
    };
  if (curr_size > 0) {
    if (new_length > curr_size) {
      grow(curr_size);
    } else {
      curr_size;
    };
  } else {
    new_length;
  };
};

let set_resizer = (a, resizer) => a.resizer = resizer;

let default_resizer = ref(doubling_resizer_without_shrinking);

let set_default_resizer = resizer => default_resizer := resizer;

let compute_size = (a, new_length) => {
  let new_size =
    a.resizer(~curr_size=a.size, ~old_length=a.length, ~new_length);
  assert(new_size >= new_length);
  new_size;
};

let resize = (a, new_length) => {
  assert(new_length >= 0);
  let new_size = compute_size(a, new_length);
  if (new_size != a.size) {
    let new_data = Array.make(new_size, Obj.magic(0));
    Array.blit(a.data, 0, new_data, 0, min(a.length, new_length));
    a.data = new_data;
    a.size = new_size;
  };
};

let add = (a, x) => {
  let new_length = a.length + 1;
  if (a.size < new_length) {
    resize(a, new_length);
  };
  unsafe_set(a, length(a), x);
  a.length = new_length;
};

let append = (a1, a2) => {
  let new_length = a1.length + a2.length;
  if (a2.size < new_length) {
    resize(a2, new_length);
  };
  Array.blit(a1.data, 0, a2.data, a2.length, a1.length);
  a2.length = new_length;
};

let insert = (a, i, x) =>
  if (is_valid_insertion_index(a, i)) {
    let new_length = a.length + 1;
    let new_size = compute_size(a, new_length);
    if (new_size != a.size) {
      let new_data = Array.make(new_size, Obj.magic(0));
      if (i > 0) {
        Array.blit(a.data, 0, new_data, 0, i);
      };
      if (i < a.length) {
        Array.blit(a.data, i, new_data, i + 1, a.length - i);
      };
      a.data = new_data;
      a.size = new_size;
    } else if (i < a.length) {
      Array.blit(a.data, i, a.data, i + 1, a.length - i);
    };
    unsafe_set(a, i, x);
    a.length = new_length;
  } else {
    invalid_arg("DynArray.insert: invalid index");
  };

let insert_range = (a1, i1, a2, i2, n) =>
  if (is_valid_sub(a2, i2, n) && is_valid_insertion_index(a1, i1)) {
    let new_length = a1.length + n;
    let new_size = compute_size(a1, new_length);
    if (new_size != a1.size) {
      let new_data = Array.make(new_size, Obj.magic(0));
      if (i1 > 0) {
        Array.blit(a1.data, 0, new_data, 0, i1);
      };
      if (i1 < a1.length) {
        Array.blit(a1.data, i1, new_data, i1 + n, a1.length - i1);
      };
      a1.data = new_data;
      a1.size = new_size;
    } else if (i1 < a1.length) {
      Array.blit(a1.data, i1, a1.data, i1 + n, a1.length - i1);
    };
    Array.blit(a2.data, i2, a1.data, i1, n);
    a1.length = new_length;
  } else {
    invalid_arg("DynArray.insert_range: invalid index or subarray");
  };

let remove_range = (a, i, n) =>
  if (is_valid_sub(a, i, n)) {
    let new_length = a.length - n;
    let new_size = compute_size(a, new_length);
    if (new_size != a.size) {
      let new_data = Array.make(new_size, Obj.magic(0));
      if (i > 0) {
        Array.blit(a.data, 0, new_data, 0, i);
      };
      if (i + n < a.length) {
        Array.blit(a.data, i + n, new_data, i, a.length - i - n);
      };
      a.data = new_data;
      a.size = new_size;
    } else if (i + n < a.length) {
      Array.blit(a.data, i + n, a.data, i, a.length - i - n);
      Array.fill(a.data, a.length - n, n, Obj.magic(0));
    } else {
      Array.fill(a.data, i, n, Obj.magic(0));
    };
    a.length = new_length;
  } else {
    invalid_arg("DynArray.remove_range: invalid subarray");
  };

let remove = (a, i) => remove_range(a, i, 1);

let remove_last = a => remove_range(a, a.length - 1, 1);

let remove_all = a => {
  let new_size = compute_size(a, 0);
  if (new_size != a.size) {
    let new_data = Array.make(new_size, Obj.magic(0));
    a.data = new_data;
    a.size = new_size;
  };
  a.length = 0;
};

let clear = remove_all;

let make = (n, x) => make_array(Array.make(n, x), default_resizer^);

let init = (n, f) => make_array(Array.init(n, f), default_resizer^);

let sub = (a, i, n) => make_array(Array.sub(a.data, i, n), a.resizer);

let copy = a => make_array(Array.copy(a.data), a.resizer);

let fill = (a, i, n, x) =>
  if (is_valid_sub(a, i, n)) {
    Array.fill(a.data, i, n, x);
  } else {
    invalid_arg("DynArray.fill: invalid subarray");
  };

let blit = (a1, i1, a2, i2, n) =>
  if (is_valid_sub(a1, i1, n) && is_valid_sub(a2, i2, n)) {
    Array.blit(a1.data, i1, a2.data, i2, n);
  } else {
    invalid_arg("DynArray.blit: invalid subarray");
  };

let iter = (f, a) =>
  for (i in 0 to a.length - 1) {
    f(unsafe_get(a, i));
  };

let rev_iter = (f, a) =>
  for (i in a.length - 1 downto 0) {
    f(unsafe_get(a, i));
  };

let iteri = (f, a) =>
  for (i in 0 to a.length - 1) {
    f(i, unsafe_get(a, i));
  };

let rev_iteri = (f, a) =>
  for (i in a.length - 1 downto 0) {
    f(i, unsafe_get(a, i));
  };

let fold_left = (f, x, a) => {
  let rec fold = (x, i) =>
    if (i >= length(a)) {
      x;
    } else {
      fold(f(x, unsafe_get(a, i)), i + 1);
    };
  fold(x, 0);
};

let fold_right = (f, a, x) => {
  let rec fold = (x, i) =>
    if (i < 0) {
      x;
    } else {
      fold(f(unsafe_get(a, i), x), i - 1);
    };
  fold(x, a.length - 1);
};

let fold_left2 = (f, a, b, c) => {
  let rec fold = (a, i) =>
    if (i >= b.length) {
      a;
    } else {
      fold(f(a, unsafe_get(b, i), unsafe_get(c, i)), i + 1);
    };
  if (b.length == c.length) {
    fold(a, 0);
  } else {
    invalid_arg("DynArray.fold_left2: array lengths not equal");
  };
};

let fold_right2 = (f, a, b, c) => {
  let rec fold = (c, i) =>
    if (i < 0) {
      c;
    } else {
      fold(f(unsafe_get(a, i), unsafe_get(b, i), c), i - 1);
    };
  if (a.length == b.length) {
    fold(c, 0);
  } else {
    invalid_arg("DynArray.fold_right2: array lengths not equal");
  };
};

let map = (f, a) =>
  make_array(
    Array.map(f, Array.sub(a.data, 0, a.length)),
    default_resizer^,
  );

let mapi = (f, a) =>
  make_array(
    Array.mapi(f, Array.sub(a.data, 0, a.length)),
    default_resizer^,
  );

let to_list = a => fold_right((x, xs) => [x, ...xs], a, []);

let of_list = l => make_array(Array.of_list(l), default_resizer^);

let to_array = a => Array.sub(a.data, 0, a.length);

let of_array = a => make_array(Array.copy(a), default_resizer^);

let for_all = (p, a) => {
  let rec for_all = i =>
    if (i >= a.length) {
      true;
    } else if (p(unsafe_get(a, i))) {
      for_all(i + 1);
    } else {
      false;
    };
  for_all(0);
};

let exists = (p, a) => {
  let rec exists = i =>
    if (i >= a.length) {
      false;
    } else if (p(unsafe_get(a, i))) {
      true;
    } else {
      exists(i + 1);
    };
  exists(0);
};

let for_all2 = (p, a, b) => {
  let rec for_all = i =>
    if (i >= a.length) {
      false;
    } else if (p(unsafe_get(a, i), unsafe_get(b, i))) {
      for_all(i + 1);
    } else {
      false;
    };
  if (a.length == b.length) {
    for_all(0);
  } else {
    invalid_arg("DynArray.for_all2: array lengths not equal");
  };
};

let exists2 = (p, a, b) => {
  let rec exists = i =>
    if (i >= a.length) {
      false;
    } else if (p(unsafe_get(a, i), unsafe_get(b, i))) {
      true;
    } else {
      exists(i + 1);
    };
  if (a.length == b.length) {
    exists(0);
  } else {
    invalid_arg("DynArray.exists2: array lengths not equal");
  };
};