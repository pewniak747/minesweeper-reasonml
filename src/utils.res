/* Utilities */
module List = Belt.List

let rec range = (start: int, end_: int): list<int> =>
  if start >= end_ {
    list{}
  } else {
    list{start, ...range(start + 1, end_)}
  }

let cartesian = (l1: list<'a>, l2: list<'a>): list<('a, 'a)> =>
  List.flatten(List.map(l1, e => List.map(l2, e' => (e, e'))))

let shuffle = List.shuffle

let take = (n: int, lst: list<'a>): list<'a> =>
  switch List.take(lst, n) {
  | Some(taken) => taken
  | None => lst
  }
