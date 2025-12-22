-- Start with some utility definitions for handling directions and positions.

-- | A cardinal direction, with `#c` being current location ("centre").

type dir = #n | #w | #e | #s

-- | Position in a grid.
type pos = (i64, i64)

-- | A representative for an invalid position.
def no_pos : pos = (-1, -1)

-- | Less-than-or-equal comparison of positions. Requires you to pass in the
-- grid width.
def pos_lte (w: i64) ((ax, ay): pos) ((bx, by): pos) : bool =
  ax * w + ay <= bx * w + by

-- | Move along direction.
def move (d: dir) ((i, j): pos) =
  match d
  case #n -> (i - 1, j)
  case #w -> (i, j - 1)
  case #e -> (i, j + 1)
  case #s -> (i + 1, j)

-- | Turn a position into a flat index, given a grid width.
def flat_pos (w: i64) ((x, y): pos) : i64 = x * w + y

-- | Turn a flat index into a position, given a grid width.
def unflat_pos (w: i64) (i: i64) : pos = (i // w, i %% w)

-- | Is this position in bounds in some grid?
def in_bounds [h] [w] 'a (_: [h][w]a) ((i, j): pos) =
  i >= 0 && i < h && j >= 0 && j < w

-- | Get element at position in grid.
def get 'a ((i, j): pos) (g: [][]a) =
  g[i, j]

-- > :img ($loadimg "regions-hard.png")

type dirs = ((i64, u32),(i64, u32),(i64, u32),(i64, u32),(i64, u32))

def max_label [h] [w] 'a (_: [h][w]a) ((i, j): pos) ((center, up, down, left, right): dirs) =
  let up_val = if i == h-1 || center.1 != up.1 then i64.lowest else up.0
  let down_val = if i == 0 || center.1 != down.1 then i64.lowest else down.0
  let left_val = if j == w-1 || center.1 != left.1 then i64.lowest else left.0
  let right_val = if j == 0 || center.1 != right.1 then i64.lowest else right.0

  let max_val = i64.max center.0 up_val |> i64.max down_val |> 
                i64.max left_val |> i64.max right_val

  in (max_val, center.1)

def region_label_naive [h] [w] (img: [h][w]u32) : [h][w]i64 =
  let init = tabulate_2d h w (\i j -> (i * w + j, img[i, j]))
  let indexes = tabulate_2d h w (\i j -> (i,j))

  let (res, _) =
    loop (res, changed) = (copy init, true)
    while changed do
      let shift_up = rotate 1 res
      let shift_down = rotate (-1) res
      let shift_left = map (rotate 1) res
      let shift_right = map (rotate (-1)) res

      let dirs_arr = map5 zip5 res shift_up shift_down shift_left shift_right
      let next = map2 (map2 (\idx dirs -> max_label img idx dirs)) indexes dirs_arr

      let changed = any (==true) (flatten (map2 (map2 (!=)) next res))
      in if changed 
         then (next, true)
         else (next, false)

  in map (map (.0)) res


-- | Could be improved. This is unlikely to produce something very legible.
def colourise_regions [h] [w] (labels: [h][w]i64) : [h][w]u32 =
  let f l = u32.i64 l
  in map (map f) labels

-- > :img (colourise_regions (region_label_naive ($loadimg "regions-hard.png")))

type edge = (pos, pos)

-- | Normalise an edge such that it goes from the lesser index to the greater.
def norm_edge w ((a, b): edge) : edge =
  if pos_lte w a b then (a, b) else (a, b)

-- | Create normalised edges linking all neighbouring pixels with the same
-- colour.
def mk_edges [h] [w] (img: [h][w]u32) =
  let right_edge =
    tabulate_2d h w (\i j ->
      if j != w-1 && img[i,j] == img[i, j+1]
      then (i*w+j, i*w+j+1)
      else (-1, -1))

  let down_edge =
    tabulate_2d h w (\i j ->
      if i != h-1 && img[i,j] == img[i+1, j]
      then (i*w+j, (i+1)*w+j)
      else (-1, -1))

  in right_edge ++ down_edge |> flatten |> filter (\(u,_) -> u != -1)

def region_label_smarter [h] [w] (img: [h][w]u32) =
  let edges = mk_edges img
  let forest = flatten (tabulate_2d h w (\i j -> flat_pos w (i, j)))
  let (forest', _) =
    loop (forest, edges) while length edges > 0 do

      let eligible_edges = filter (\(a,_) -> forest[a] == a) edges
      let is = map (.0) eligible_edges
      let as = map (.1) eligible_edges
      let updated_forest = reduce_by_index (copy forest) i64.max (-1) is as

      let filtered_edges = filter (\(a,b) -> forest[a] != a || updated_forest[a] != b) edges
      let updated_edges =
        map (\(a,b) ->
              let x = updated_forest[a]
              let y = updated_forest[b]
              in if x < y then (x,y) else (y,x)) filtered_edges
        
      in (updated_forest, updated_edges)

  let ids = iota (h*w)
  let (res, _) =
    loop (p, changed) = (forest', true) while changed do
      let next = map (\i -> p[p[i]]) ids
      in (next, any (\i -> next[i] != p[i]) ids)

  in unflatten (res :> [h*w]i64)

-- > :img (colourise_regions (region_label_smarter ($loadimg "regions-hard.png")))
