import "lib/github.com/diku-dk/sorts/radix_sort"

def segscan [n] 't (op: t -> t -> t) (ne: t)
                   (arr: [n](t, bool)): [n]t =
    scan (\(v1, f1) (v2, f2) -> (if f2 then v2 else op v1 v2, f1 || f2)) 
         (ne, false) arr |> unzip |> (.0)

def segreduce [n] 't (op: t -> t -> t) (ne: t)
                     (arr: [n](t, bool)): []t =
    let last_elm_flags = rotate 1 (map (.1) arr)
    let scanRes = segscan op ne arr
    in map (.0) (filter (\(_, f) -> f) (zip scanRes last_elm_flags)) 
     

def segreduce_index [n] 't (op: t -> t -> t) (ne: t)
                           (arr: [n](t, i64)): []t =
    let (vals, idxs) = unzip arr
    let flags = map (\x -> x != -1) (rotate (-1) idxs)
    let scan_input = zip vals flags
    let scanRes = segscan op ne scan_input
    in scatter (replicate (idxs[n-1]+1) ne) idxs scanRes

def reduce_by_index 'a [n]
                    (f: a -> a -> a) (ne: a)
                    (is: [n]i64) (as: [n]a): []a =
    let sort_arr = radix_sort_int_by_key (\p -> p.1) i64.num_bits i64.get_bit (zip as is)
    let sort_arr_updated = map (\i -> if i == n-1 || sort_arr[i].1 != sort_arr[i+1].1
                                      then sort_arr[i]
                                      else (sort_arr[i].0, -1)) (iota n)
    in segreduce_index f ne sort_arr_updated




-----------------------------------------------------
-- This only works with all non-empty indexes
-----------------------------------------------------
-- def reduce_by_index 'a [n]
--                     (f: a -> a -> a) (ne: a)
--                     (is: [n]i64) (as: [n]a): []a =
--     let sort_arr = radix_sort_int_by_key (\p -> p.1) i64.num_bits i64.get_bit (zip as is)

--     let sort_arr_flags = map (\i -> if i == 0 || sort_arr[i].1 != sort_arr[i-1].1 
--                                     then (sort_arr[i].0, true) 
--                                     else (sort_arr[i].0, false)) (iota n)

--     in segreduce f ne sort_arr_flags