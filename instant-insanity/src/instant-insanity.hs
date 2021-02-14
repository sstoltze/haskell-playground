{-# LANGUAGE UndecidableInstances #-}
import Prelude hiding (flip)

cubes = ["BGWGBR", "WGBWRR", "GWRBRR", "BRGGWW"]

-- Rotate 90 degrees around the z-axis
rot [u, f, r, b, l, d] = [u, r, b, l, f, d]

-- Twist along the axis from upper-front-right corner to back-down-left corrner
twist [u, f, r, b, l, d] = [f, r, u, l, d, b]

flip [u, f, r, b, l, d] = [d, l, b, r, f, u]

-- Compute all 24 ways to orient a cube
orientations c = [c''' |
                   c'   <- [c, rot c, rot (rot c), rot (rot (rot c))],
                   c''  <- [c', twist c', twist (twist c')],
                   c''' <- [c'', flip c'']]

-- Compute visible faces when stacked
visible [u, f, r, b, l, d] = [f, r, b, l]

-- Two cubes are compatible if they have different colours on every visible face
compatible c c' = and [x /= x' | (x, x') <- zip (visible c) (visible c')]

-- Can the cube be added?
allowed c cs = and [compatible c c' | c' <- cs]

solutions [] = [[]]
solutions (c:cs) = [c' : cs' | cs' <- solutions cs,
                               c' <- orientations c,
                               allowed c' cs']

main = print $ solutions cubes
