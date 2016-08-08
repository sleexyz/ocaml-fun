
module IntMap = Map.Make(struct
    type t = int
    let compare (x : int) y = compare x y
  end)

let _ = Js.log "hello bucklescript!"
let foo = Js.log "helloworld!"

let test () =
  let m = ref IntMap.empty in
  let count = 100000 in
  for i = 0 to count do
    m := IntMap.add i i !m
  done;

  for j = 0 to count do
    ignore (IntMap.find j !m)
  done;;

test()





