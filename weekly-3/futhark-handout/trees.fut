-- # Tree operations.
--
-- We import a library so we don't have to write a segmented scan
-- ourselves. Remember to run `futhark pkg sync` to download it.
import "lib/github.com/diku-dk/segmented/segmented"
import "lib/github.com/diku-dk/sorts/radix_sort"

-- A traversal is an array of these steps.
type step = #u | #d i32

-- ## Input handling.
--
-- You do not have to modify this. The function 'input.steps' takes as
-- argument a string with steps as discussed in the assignment text,
-- and gives you back an array of type '[]step'.
--
-- Example:
--
-- ```
-- > input.steps "d0 d2 d3 u u d5 u"
-- [#d 0, #d 2, #d 3, #u, #u, #d 5, #u]
-- ```

type char = u8
type string [n] = [n]char

module input
  : {
      -- | Parse a string into an array of commands.
      val steps [n] : string [n] -> []step
    } = {
  def is_space (x: char) = x == ' ' || x == '\n'
  def isnt_space x = !(is_space x)

  def (&&&) f g = \x -> (f x, g x)

  def dtoi (c: char) : i32 = i32.u8 c - '0'

  def is_digit (c: char) = c >= '0' && c <= '9'

  def atoi [n] (s: string [n]) : i32 =
    let (sign, s) = if n > 0 && s[0] == '-' then (-1, drop 1 s) else (1, s)
    in sign
       * (loop (acc, i) = (0, 0)
          while i < length s do
            if is_digit s[i]
            then (acc * 10 + dtoi s[i], i + 1)
            else (acc, n)).0

  def to_step (s: []char) : step =
    match s[0]
    case 'u' -> #u
    case _ -> #d (atoi (drop 1 s))

  type slice = (i64, i64)

  def get 't ((start, end): slice) (xs: []t) =
    xs[start:end]

  def words [n] (s: string [n]) : []slice =
    segmented_scan (+) 0 (map is_space s) (map (isnt_space >-> i64.bool) s)
    |> (id &&& rotate 1)
    |> uncurry zip
    |> zip (indices s)
    |> filter (\(i, (x, y)) -> (i == n - 1 && x > 0) || x > y)
    |> map (\(i, (x, _)) -> (i - x + 1, i + 1))

  def steps [n] (s: string [n]) =
    map (\slice -> to_step (get slice s)) (words s)
}

-- ## Task 2.1

def depths (steps: []step) : [](i64, i32) =
 let n = length steps
 let traversal = map (\s -> match s 
                            case #u -> -1
                            case _ -> 1) steps

 let incScan = scan (+) 0 traversal
 let excScan = ([0] ++ incScan[:n-1]) :> [n]i64

 let filtered = filter (\(_, s) -> match s 
                                   case #u -> false 
                                   case _ -> true) (zip excScan (steps :> [n]step))

 in map (\(d, s) -> match s 
                    case #u -> (d, 0) -- "impossible" branch, but required
                    case #d v -> ((d, v))) filtered


entry test_depths steps =
  let (D, _) = depths (input.steps steps) |> unzip
  in D


-- ==
-- entry: test_depths
-- nobench input { "d0 d2 d3 u u d5 u" }
-- output { [0i64,1i64,2i64,1i64] }
-- nobench input { "d0 d2 u d3 u d0 d4 u d0 u u d0 d5" }
-- output { [0i64,1i64,1i64,1i64,2i64,2i64,1i64,2i64] }
-- nobench input { "d1 d2 d3 d4 d5 u u u" }
-- output { [0i64,1i64,2i64,3i64,4i64] }

-- ## Task 2.2

def binSearch [n] (as: [n](i64, i64)) (x: (i64, i64)) : i64 =
  let (_, _, curr) =
    loop (l, h, curr) = (0, n - 1, 0)
    while l <= h do
      let m  = (l + h) / 2
      let ma = as[m]
      in
      if ma.0 < x.0 then (m+1, h, curr)
      else if x.0 < ma.0 || x.1 < ma.1 then (l, m-1, curr)
      else (m+1, h, ma.1)
  in curr

def parents [n] (D: [n]i64) : [n]i64 =
  let sort_arr = radix_sort_int_by_key (\p -> p.0) i64.num_bits i64.get_bit (zip D (iota n))
  let unsorted_parents = map (\(d,i) -> binSearch sort_arr (d-1, i)) sort_arr
  let (_, is) = unzip sort_arr
  in radix_sort_int_by_key (\p -> p.0) i64.num_bits i64.get_bit (zip is unsorted_parents) |> unzip |> (.1)

-- parents [0,1,2,1]
-- parents [0,1,1,1,2,2,1,2]
-- parents  [0,1,1,2,3]
-- parents  [0,1,2,3,4]

entry test_parents D = parents D

-- ==
-- entry: test_parents
-- nobench input { [0i64,1i64,2i64,1i64] }
-- output { [0i64,0i64,1i64,0i64] }
-- nobench input { [0i64,1i64,1i64,1i64,2i64,2i64,1i64,2i64] }
-- output { [0i64,0i64,0i64,0i64,3i64,3i64,0i64,6i64] }
-- nobench input { [0i64,1i64,2i64,3i64,4i64] }
-- output { [0i64,0i64,1i64,2i64,3i64] }


-- ## Task 2.3

def subtree_sizes [n] (steps: [n]step) : []i64 =
  let (D, V) = depths steps |> unzip
  let maxD = reduce i64.max D[0] D
  let revD = reverse D
  let revV = map i64.i32 (reverse V)

  let res =
    loop sizes = revV for d in maxD..maxD-1...1 do
      let maskedVals = map2 (\v f -> if f then 0 else v) sizes (map (> d) revD)
      let flags = map (< d) revD
      let scannedVals = segmented_scan (+) 0 (rotate (-1) flags) maskedVals
      in map3 (\x y f -> if f then y else x) sizes scannedVals flags
      
  in reverse res

-- subtree_sizes [#d 0, #d 2, #d 3, #u, #u, #d 5, #u]
-- subtree_sizes [#d 0, #d 2, #u, #d 3, #u, #d 0, #d 4, #u, #d 0, #u, #u, #d 0, #d 5]
-- subtree_sizes [#d 1, #d 2, #d 3, #d 4, #d 5]

entry test_subtree_sizes steps = subtree_sizes (input.steps steps)

-- ==
-- entry: test_subtree_sizes
-- nobench input { "d0 d2 d3 u u d5 u" }
-- output { [10i64,5i64,3i64,5i64] }
-- nobench input { "d0 d2 u d3 u d0 d4 u d0 u u d0 d5" }
-- output { [14i64,2i64,3i64,4i64,4i64,0i64,5i64,5i64] }
-- nobench input { "d1 d2 d3 d4 d5" }
-- output { [15i64,14i64,12i64,9i64,5i64] }