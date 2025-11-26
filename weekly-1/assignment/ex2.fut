-- ==
-- entry: built_in_scan
-- random input { [128]i32 }
-- random input { [1024]i32 }
-- random input { [16384]i32 }
-- random input { [131072]i32 }
-- random input { [1048576]i32 }
-- random input { [8388608]i32 }
-- random input { [67108864]i32 }

-- ==
-- entry: test_hillis_steele
-- random input { [128]i32 }
-- random input { [1024]i32 }
-- random input { [16384]i32 }
-- random input { [131072]i32 }
-- random input { [1048576]i32 }
-- random input { [8388608]i32 }
-- random input { [67108864]i32 }

-- ==
-- entry: test_work_efficient
-- random input { [128]i32 }
-- random input { [1024]i32 }
-- random input { [16384]i32 }
-- random input { [131072]i32 }
-- random input { [1048576]i32 }
-- random input { [8388608]i32 }
-- random input { [67108864]i32 }

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
        loop xs = copy xs for d in 0...m-1 do
            let offset = 1 << d
            let step = offset << 1
            in map (\i -> if (i+1) % step == 0 
                        then xs[i] + xs[i - offset]
                        else xs[i]) (iota n)

    let upswept[n -1] = 0

    let downswept =
        loop xs = upswept for d in m-1..m-2...0 do
            let offset = 1 << d
            let step = offset << 1
            in map (\i -> if (i+1) % step == 0
                        then xs[i] + xs[i - offset]
                        else if (i+1) % offset == 0
                        then xs[i + offset]
                        else xs[i]) (iota n)

    in downswept

entry built_in_scan [n] (xs: [n]i32) : [n]i32 =
  scan (+) 0 xs

entry test_hillis_steele [n] (xs: [n]i32) : [n]i32 =
  hillis_steele xs

entry test_work_efficient [n] (xs: [n]i32) : [n]i32 =
  work_efficient xs