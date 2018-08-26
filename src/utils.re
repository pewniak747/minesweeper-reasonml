/* Utilities */
let str = ReasonReact.string;

let arr = ReasonReact.array;

let rec range = (start: int, end_: int) =>
  if (start >= end_) {
    [];
  } else {
    [start, ...range(start + 1, end_)];
  };

let cartesian = (l1, l2) =>
  List.concat(List.map(e => List.map(e' => (e, e'), l2), l1));

let compareFst = ((fst1: int, _), (fst2: int, _)) =>
  Pervasives.compare(fst1, fst2);

let shuffle = d => {
  let nd = List.map(c => (Random.bits(), c), d);
  let sond = List.sort(compareFst, nd);
  List.map(snd, sond);
};

let rec take = (n, lst) =>
  switch (n) {
  | 0 => []
  | n => [List.hd(lst), ...take(n - 1, List.tl(lst))]
  };
