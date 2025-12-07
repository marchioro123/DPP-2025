-- ==
-- entry: built_in_scan
-- nobench input { [1i32, 2i32, 3i32, 4i32] }
-- output { [1i32, 3i32, 6i32, 10i32] }
-- nobench input { [5i32, -2i32, 7i32, 4i32] }
-- output { [5i32, 3i32, 10i32, 14i32] }
-- notest random input { [128]i32 }
-- notest random input { [1024]i32 }
-- notest random input { [16384]i32 }
-- notest random input { [131072]i32 }
-- notest random input { [1048576]i32 }
-- notest random input { [8388608]i32 }
-- notest random input { [67108864]i32 }

-- ==
-- entry: test_hillis_steele
-- nobench input { [1i32, 2i32, 3i32, 4i32] }
-- output { [1i32, 3i32, 6i32, 10i32] }
-- nobench input { [5i32, -2i32, 7i32, 4i32] }
-- output { [5i32, 3i32, 10i32, 14i32] }
-- notest random input { [128]i32 }
-- notest random input { [1024]i32 }
-- notest random input { [16384]i32 }
-- notest random input { [131072]i32 }
-- notest random input { [1048576]i32 }
-- notest random input { [8388608]i32 }
-- notest random input { [67108864]i32 }

-- ==
-- entry: test_work_efficient
-- nobench input { [1i32, 2i32, 3i32, 4i32] }
-- output { [0i32, 1i32, 3i32, 6i32] }
-- nobench input { [5i32, -2i32, 7i32, 4i32] }
-- output { [0i32, 5i32, 3i32, 10i32] }
-- notest random input { [128]i32 }
-- notest random input { [1024]i32 }
-- notest random input { [16384]i32 }
-- notest random input { [131072]i32 }
-- notest random input { [1048576]i32 }
-- notest random input { [8388608]i32 }
-- notest random input { [67108864]i32 }

def ilog2 (x: i64) = 63 - i64.i32 (i64.clz x)

def hillis_steele [n] (xs: [n]i32) : [n]i32 =
    let m = ilog2 n
    in loop xs = copy xs for d in 0...m-1 do
        let offset = 1 << d
        in map (\i -> if i >= offset
                        then xs[i] + xs[i - offset]
                        else xs[i]) (iota n)


def work_efficient [n] (xs: [n]i32) : [n]i32 =
    let m = ilog2 n
    let upswept =
        loop xs = copy xs for d in m-1..m-2...0 do
            let n' = 1 << d
            let step  = 1 << (m - d)
            let offset = step >> 1
            let idxs = map (\i -> (i+1)*step - 1) (iota n')
            let vals = map (\idx -> xs[idx - offset] + xs[idx]) idxs
            in scatter xs idxs vals

    let upswept[n -1] = 0

    let downswept =
        loop xs = upswept for d in 0...m-1 do
            let n' = 1 << d
            let step  = 1 << (m - d)
            let offset = step >> 1
            let idxs1 = map (\i -> (i+1)*step - 1) (iota n')
            let idxs2 = map (\i -> i*step + offset - 1) (iota n')
            let vals1 = map (\idx -> xs[idx - offset] + xs[idx]) idxs1
            let vals2 = map (\idx -> xs[idx + offset]) idxs2

            let all_idxs = concat idxs1 idxs2
            let all_vals = concat vals1 vals2

            in scatter xs all_idxs all_vals
            
    in downswept

entry built_in_scan [n] (xs: [n]i32) : [n]i32 =
  scan (+) 0 xs

entry test_hillis_steele [n] (xs: [n]i32) : [n]i32 =
  hillis_steele xs

entry test_work_efficient [n] (xs: [n]i32) : [n]i32 =
  work_efficient xs