[@bs.module "./registerServiceWorker"]
external register_service_worker : unit => unit = "default";

module Int = {
  type t = int;
  let compare = (x: t, y: t) => x - y;
};

module IntHeap = Heap.BinomialHeap(Int);

let a = IntHeap.empty;


let a = IntHeap.insert(4, a);
let a = IntHeap.insert(2, a);
let a = IntHeap.insert(1, a);

let a = IntHeap.insert(3, a);


Js.log(a);

ReactDOMRe.renderToElementWithId(
  <div>
    <div> (ReasonReact.string("sdf")) </div>
    <div> (ReasonReact.string(string_of_int(IntHeap.findMin(a)))) </div>
  </div>,
  "root",
);

register_service_worker();