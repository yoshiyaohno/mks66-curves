import Line
import Transform

main = do
    let cubepts = [Vect x y z 1 | x <- [100, 400],
                                  y <- [100, 400],
                                  z <- [100, 400]]
        edges   = concat [[a, b] | a <- cubepts, b <- cubepts, a /= b]
        drawing = (drawEdges red edges) mempty
    writeFile "out.ppm" (printPixels (600, 600) drawing)
