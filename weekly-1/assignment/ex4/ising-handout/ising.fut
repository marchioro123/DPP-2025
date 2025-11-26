-- We represent a spin as a single byte.  In principle, we need only
-- two values (-1 or 1), but Futhark represents booleans a a full byte
-- entirely, so using an i8 instead takes no more space, and makes the
-- arithmetic simpler.
type spin = i8

import "lib/github.com/diku-dk/cpprandom/random"

-- Pick an RNG engine and define random distributions for specific types.
module rng_engine = minstd_rand
module rand_f32 = uniform_real_distribution f32 rng_engine
module rand_i8 = uniform_int_distribution i8 rng_engine

-- We can create an few RNG state with 'rng_engine.rng_from_seed [x]',
-- where 'x' is some seed.  We can split one RNG state into many with
-- 'rng_engine.split_rng'.
--
-- For an RNG state 'r', we can generate random integers that are
-- either 0 or 1 by calling 'rand_i8.rand (0i8, 1i8) r'.
--
-- For an RNG state 'r', we can generate random floats in the range
-- (0,1) by calling 'rand_f32.rand (0f32, 1f32) r'.
--
-- Remember to consult
-- https://futhark-lang.org/pkgs/github.com/diku-dk/cpprandom/latest/

def rand = rand_f32.rand (0f32, 1f32)

-- Create a new grid of a given size.  Also produce an identically
-- sized array of RNG states.
def random_grid (seed: i32) (h: i64) (w: i64)
              : ([h][w]rng_engine.rng, [h][w]spin) =
  let init_rngs = rng_engine.split_rng (h*w) (rng_engine.rng_from_seed [seed])
  let (flat_rngs, flat_spins) = 
    unzip (map (\r -> let (r', x) = rand_i8.rand (0i8, 1i8) r
                      in (r', x*2-1)) init_rngs)
  in (unflatten (flat_rngs :> [h*w]rng_engine.rng), 
      unflatten (flat_spins :> [h*w]spin))


-- Compute $\Delta_e$ for each spin in the grid, using wraparound at
-- the edges.
def deltas [h][w] (spins: [h][w]spin): [h][w]i8 =
  let up    = rotate 1 spins
  let down  = rotate (-1) spins
  let left  = map (rotate 1) spins
  let right = map (rotate (-1)) spins
  in map5 (\row_c row_u row_d row_l row_r -> 
              map5 (\c u d l r -> 2*c*(u+d+l+r)) row_c row_u row_d row_l row_r)
          spins up down left right

-- The sum of all deltas of a grid.  The result is a measure of how
-- ordered the grid is.
def delta_sum [h][w] (spins: [w][h]spin): i32 =
  reduce (+) 0 (map i32.i8 (flatten (deltas spins)))

-- Take one step in the Ising 2D simulation.
def step [h][w] (abs_temp: f32) (samplerate: f32)
                (rngs: [h][w]rng_engine.rng) (spins: [h][w]spin)
              : ([h][w]rng_engine.rng, [h][w]spin) =
  let pairs = map3 (\row_rng row_spin row_delta ->
                map3 (\r c d ->
                        let (r1, a) = rand r
                        let (r2, b) = rand r1
                        let flip = a < samplerate && 
                                   (d < 0 || b < f32.exp (-f32.i8 d/abs_temp))
                        in (r2, if flip then -c else c))
                      row_rng row_spin row_delta)
              rngs spins (deltas spins)

  in unzip (map unzip pairs)


-- | Just for benchmarking.
def main (abs_temp: f32) (samplerate: f32)
         (h: i64) (w: i64) (n: i32): [h][w]spin =
  (loop (rngs, spins) = random_grid 1337 h w for _i < n do
     step abs_temp samplerate rngs spins).1

-- ==
-- entry: main
-- input { 0.5f32 0.1f32 10i64 10i64 2 } auto output
-- input { 0.5f32 0.1f32 100i64 100i64 2 } auto output
-- input { 0.5f32 0.1f32 1000i64 1000i64 2 } auto output
-- input { 0.5f32 0.1f32 10000i64 10000i64 2 } auto output
-- input { 0.5f32 0.1f32 10i64 10i64 1 } auto output
-- input { 0.5f32 0.1f32 10i64 10i64 10 } auto output
-- input { 0.5f32 0.1f32 10i64 10i64 100 } auto output
-- input { 0.5f32 0.1f32 10i64 10i64 1000 } auto output


-- The following definitions are for the visualisation and need not be modified.

type~ state = {cells: [][](rng_engine.rng, spin)}

entry tui_init seed h w : state =
  let (rngs, spins) = random_grid seed h w
  in {cells=map (uncurry zip) (zip rngs spins)}

entry tui_render (s: state) = map (map (.1)) s.cells

entry tui_step (abs_temp: f32) (samplerate: f32) (s: state) : state =
  let rngs = (map (map (.0)) s.cells)
  let spins = map (map (.1)) s.cells
  let (rngs', spins') = step abs_temp samplerate rngs spins
  in {cells=map (uncurry zip) (zip rngs' spins')}
