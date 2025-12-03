import "util"
import "mk-datasets"

---------------------------------------------------------------------
--- This refers to flattening the following nested parallel code: ---
---   def nestedPar [m] (ass: [m][]f32) (bss: [m][]f32)           ---
---                     (cs: [m]f32) (inds: [m]i64) : [m]32 =     ---
---     map4 (\ as bs c ind ->                                    ---
---             let f a = (f32.sqrt a) * bs[ind] + c              ---
---             let tmp1 = map f as                               ---
---             let ioti = iota (length as)                       ---
---             let iotf = map f32.i64 ioti                       ---
---             let tmp2 = map2 (+) tmp1 iotf                     ---
---             in  reduce f32.max f32.lowest tmp2                ---
---          ) ass bss cs inds                                    ---
---                                                               ---
--- Function `classicKer` flattens the code above by utilizing    ---
---   the old/inneficient rules of flattening, e.g., that         ---
---   manifest in memory the replication of free variables        ---
---   and the segmented iotas.                                    ---
---                                                               --- 
--- Your Task 1 is to implement function `optimII1Ker` that       ---
---   performs the flattening of the code above by using the      ---
---   "new" more-efficient rules, which for example rely on the   ---
---   II1 array to avoid manifestation of replicated and iota     ---
---   arrays.                                                     ---
--- Your report should contain the implementation of the          ---
---   `optimII1Ker` function together with the runtimes of both   ---
---   entrypoints on the provided datasets and the speedup of your---
---   implementation in comparison with the provide `classic` one.---
--- Currently, the `optimII1` entrypoint does not validate; of    ---
---   course, after you implement `optimII1Ker`, make sure that   ---
---   it does!
---------------------------------------------------------------------

def classicKer [m][n][q] (Sa: [m]u32, Da: [n]f32)
                         (Sb: [m]u32, Db: [q]f32)
                         (cs: [m]f32) (inds: [m]i64)
                       : [m]f32 = #[unsafe]
  -- make offsets and flag array
  let (Ba, flags) = mkFlagArray Sa false (replicate m true)
  let beg_segs = map2 (\s i -> if s==0 then -1i64 else i64.u32 i) Sa Ba
  let flags = flags :> [n]bool
  --  
  let (Bb, _) = mkFlagArray Sb false (replicate m true)
  -- replicate bofinds inside map
  let bofinds = map2 (\off ind -> Db[i64.u32 off+ind]) Bb inds
  let rep_b = 
    let vls = scatter (replicate n 0) beg_segs bofinds
    in  sgmScan (+) 0 flags vls
  -- replicate cs inside map
  let rep_c =
    let vls = scatter (replicate n 0) beg_segs cs
    in  sgmScan (+) 0 flags vls
  -- map inside map
  let tmp1s = map3 (\ a b c -> (f32.sqrt a) * b + c) Da rep_b rep_c
  -- iota inside map  
  let iotis =
    sgmScan (+) 0i64 flags (replicate n 1i64) |> map (\x -> x-1)
  -- map inside map
  let iotfs = map f32.i64 iotis
  -- map inside map
  let tmp2s = map2 (+) tmp1s iotfs
  -- reduce inside map
  let res =
    let tmp_scan = sgmScan f32.max f32.lowest flags tmp2s
    in  imap2 (iota m) Sa 
          (\i s -> if s <= 0 then f32.lowest 
                   else if i == m-1
                        then tmp_scan[n-1]
                        else tmp_scan[i64.u32 Ba[i+1]-1]
          ) 
  in  res

def optimII1Ker [m][n][q] (Sa: [m]u32, Da: [n]f32)
                          (Sb: [m]u32, Db: [q]f32)
                          (cs: [m]f32) (inds: [m]i64)
                        : [m]f32 =
  let (Ba, flags') = (mkFlagArray Sa 0 (iota m) :> ([m]u32, [n]i64))
  let (Bb, _) = (mkFlagArray Sb 0 (iota m) :> ([m]u32, [n]i64))

  let flags = map bool.i64 flags'
  let II1 = sgmScan (+) 0 flags flags'

  let As' = map f32.sqrt Da
  
  let bofinds = map2 (\off ind -> Db[i64.u32 off+ind]) Bb inds
  let Bs' = map (\i -> bofinds[i]) II1

  let Cs' = map (\i -> cs[i]) II1

  let iotis = map2 (\i sgm -> f32.i64 i - f32.u32 Ba[sgm]) (iota n) II1 -- sgmScan (+) 0 flags (replicate n 1) |> map (\x -> x-1)

  let tmp = map4 (\a b c i -> a * b + c + i) As' Bs' Cs' iotis

  let scanTmp = sgmScan f32.max f32.lowest flags tmp

  let last_idxs = map2 (\x y -> if x == y then -1 else x) II1 (rotate 1 II1)

  in scatter (replicate m f32.lowest) last_idxs scanTmp

-- optimII1Ker ([3,2,2], [1,2,4,3,2,1,1]) ([3,2,2], [1,0,2,3,3,1,0]) [2,3,2] [0,1,1]
-- optimII1Ker ([0,3,0,2,2], [1,2,4,3,2,1,1]) ([0,3,0,2,2], [1,0,2,3,3,1,0]) [0,2,0,3,2] [0,0,0,1,1]

-----------------------------------------
--- dataset generation & entry points ---
-----------------------------------------

entry mkData (m: i64) (q:i64) (p:i64) =
  assert (m > 0 && q > 0 && p > 0) (mkDataset m q p)  

-- Primes: Flat-Parallel Version
-- == entry: classic optimII1
-- "(m,q,p)=(10, 10000000,100000)" script input { mkData 10i64 10000000i64 100000i64 }
-- output @ data/res10x10000000x100000.out
--
-- "(m,q,p)=(100, 1000000, 10000)" script input { mkData 100i64 1000000i64  10000i64 }
-- output @ data/res100x1000000x10000.out
--
-- "(m,q,p)=(1000, 100000,  1000)" script input { mkData 1000i64 100000i64   1000i64 }
-- output @ data/res1000x100000x1000.out
--
-- "(m,q,p)=(10000, 10000,   100)" script input { mkData 10000i64 10000i64    100i64 }
-- output @ data/res10000x10000x100.out
--
-- "(m,q,p)=(100000, 1000,    10)" script input { mkData 100000i64 1000i64     10i64 }
-- output @ data/res100000x1000x10.out


-- Test case 1
-- ==
-- input {
--   [3u32,2u32,2u32]
--   [1f32,2f32,4f32,3f32,2f32,1f32,1f32]
--   [3u32,2u32,2u32]
--   [1f32,0f32,2f32,3f32,3f32,1f32,0f32]
--   [2f32,3f32,2f32]
--   [0i64,1i64,1i64]
-- }
-- output { [6.0f32, 8.2426405f32, 3.0f32] }

-- Test case 2: Empty segments
-- ==
-- input {
--   [0u32,3u32,0u32,2u32,2u32]
--   [1f32,2f32,4f32,3f32,2f32,1f32,1f32]
--   [0u32,3u32,0u32,2u32,2u32]
--   [1f32,0f32,2f32,3f32,3f32,1f32,0f32]
--   [0f32,2f32,0f32,3f32,2f32]
--   [0i64,0i64,0i64,1i64,1i64]
-- }
-- output { [-f32.inf, 6.0f32, -f32.inf, 8.2426405f32, 3.0f32] }

entry classic [m][n][q]  (Sa: [m]u32) (Da: [n]f32)
                         (Sb: [m]u32) (Db: [q]f32)
                         (cs: [m]f32) (inds: [m]i64)
                       : [m]f32 =
  classicKer (Sa, Da) (Sb, Db) cs inds

entry optimII1 [m][n][q] (Sa: [m]u32) (Da: [n]f32)
                         (Sb: [m]u32) (Db: [q]f32)
                         (cs: [m]f32) (inds: [m]i64)
                       : [m]f32 =
  optimII1Ker (Sa, Da) (Sb, Db) cs inds

let main [m][n][q] (Sa: [m]u32) (Da: [n]f32)
                         (Sb: [m]u32) (Db: [q]f32)
                         (cs: [m]f32) (inds: [m]i64)
                       : [m]f32 =
  optimII1Ker (Sa, Da) (Sb, Db) cs inds