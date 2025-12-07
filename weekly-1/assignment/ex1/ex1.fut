let process (xs: []i32) (ys: []i32) : i32 =
    reduce i32.max 0 (map2 (\x y -> i32.abs(x-y)) xs ys)

let process_idx [n] (xs: [n]i32) (ys: [n]i32) : (i64, i32) =
    let diffs = map2 (\x y -> i32.abs(x-y)) xs ys
    let max (i1, x1) (i2, x2) = if x1 > x2 then (i1, x1) else (i2, x2)
    in reduce max (0, -1) (zip (iota n) (diffs :> [n]i32))

let process_idx_comm [n] (xs: [n]i32) (ys : [n]i32): (i64, i32) =
    let diffs = map2 (\x y -> i32.abs(x-y)) xs ys
    let max (i1, x1) (i2, x2) =
        if x1 > x2 then (i1, x1)
        else if x2 > x1 then (i2, x2)
        else if i1 > i2 then (i1, x1)
        else (i2, x2)
    in reduce_comm max (0, -1) (zip (iota n) (diffs :> [n]i32))

entry test_process (xs: []i32) (ys: []i32) : i32 =
  process xs ys

entry test_process_idx (xs: []i32) (ys: []i32) : (i64, i32) =
  process_idx xs ys

entry test_process_idx_comm (xs: []i32) (ys: []i32) : (i64, i32) =
  process_idx xs ys

-- ==
-- entry: test_process
-- nobench input { [1i32, 4i32, 7i32] [2i32, 10i32, 5i32] }
-- output { 6i32 }
-- nobench input { [-5i32, 0i32, 5i32] [0i32, 0i32, 0i32] }
-- output { 5i32 }
-- notest random input { [100]i32 [100]i32 }
-- notest random input { [1000]i32 [1000]i32 }
-- notest random input { [10000]i32 [10000]i32 }
-- notest random input { [100000]i32 [100000]i32 }
-- notest random input { [1000000]i32 [1000000]i32 }
-- notest random input { [10000000]i32 [10000000]i32 }
-- notest random input { [100000000]i32 [100000000]i32 }

-- ==
-- entry: test_process_idx
-- nobench input { [1i32, 4i32, 7i32] [2i32, 10i32, 5i32] }
-- output { 1i64 6i32 }
-- nobench input { [-5i32, 0i32, 5i32] [0i32, 0i32, 1i32] }
-- output { 0i64 5i32 }
-- notest random input { [100]i32 [100]i32 }
-- notest random input { [1000]i32 [1000]i32 }
-- notest random input { [10000]i32 [10000]i32 }
-- notest random input { [100000]i32 [100000]i32 }
-- notest random input { [1000000]i32 [1000000]i32 }
-- notest random input { [10000000]i32 [10000000]i32 }
-- notest random input { [100000000]i32 [100000000]i32 }

-- ==
-- entry: test_process_idx_comm
-- nobench input { [1i32, 4i32, 7i32] [2i32, 10i32, 5i32] }
-- output { 1i64 6i32 }
-- nobench input { [-5i32, 0i32, 5i32] [0i32, 0i32, 1i32] }
-- output { 0i64 5i32 }
-- notest random input { [100]i32 [100]i32 }
-- notest random input { [1000]i32 [1000]i32 }
-- notest random input { [10000]i32 [10000]i32 }
-- notest random input { [100000]i32 [100000]i32 }
-- notest random input { [1000000]i32 [1000000]i32 }
-- notest random input { [10000000]i32 [10000000]i32 }
-- notest random input { [100000000]i32 [100000000]i32 }




-- let main (xs: []i32) (ys: []i32) : (i64, i32) =
--     process_idx_comm xs ys