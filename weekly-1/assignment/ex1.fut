-- ==
-- entry: test_process
-- random input { [100]i32 [100]i32 } auto output
-- random input { [1000]i32 [1000]i32 } auto output
-- random input { [10000]i32 [10000]i32 } auto output
-- random input { [100000]i32 [100000]i32 } auto output
-- random input { [1000000]i32 [1000000]i32 } auto output
-- random input { [10000000]i32 [10000000]i32 } auto output

-- ==
-- entry: test_process_idx
-- random input { [100]i32 [100]i32 } auto output
-- random input { [1000]i32 [1000]i32 } auto output
-- random input { [10000]i32 [10000]i32 } auto output
-- random input { [100000]i32 [100000]i32 } auto output
-- random input { [1000000]i32 [1000000]i32 } auto output
-- random input { [10000000]i32 [10000000]i32 } auto output

let process (xs: []i32) (ys: []i32) : i32 =
    reduce i32.max 0 (map2 (\x y -> i32.abs(x-y)) xs ys)

-- let process_idx (xs: []i32) (ys: []i32) : (i64, i32) =
--     let n = length xs
--     let idxs = iota n
--     let diffs = map2 (\x y -> i32.abs(x-y)) xs ys
--     let max (i1, x1) (i2, x2) = if x1 > x2 then (i1, x1) else (i2, x2)
--     in reduce max (0, -1) (zip idxs (diffs :> [n]i32))

let process_idx [n] (xs: [n]i32) (ys : [n]i32): (i64, i32) =
    let n = length xs
    let idxs = iota n
    let diffs = map2 (\x y -> i32.abs(x-y)) xs ys
    let max (i1, x1) (i2, x2) =
        if x1 > x2 then (i1, x1)
        else if x2 > x1 then (i2, x2)
        else if i1 > i2 then (i1, x1)
        else (i2, x2)
    in reduce_comm max (0, -1) (zip idxs (diffs :> [n]i32))


entry test_process (xs: []i32) (ys: []i32) : i32 =
  process xs ys

entry test_process_idx (xs: []i32) (ys: []i32) : (i64, i32) =
  process_idx xs ys

let main (xs: []i32) (ys: []i32) : (i64, i32) =
    process_idx xs ys