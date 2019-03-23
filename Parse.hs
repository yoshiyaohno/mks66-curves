{-# LANGUAGE FlexibleContexts #-}
import Line
import qualified Transform as T

import System.Directory
import System.IO
import System.Environment
import Control.Monad.State
import System.Process
import qualified Data.Map.Strict as M
import qualified Data.List as L

type DrawMats = (Screen, T.Transform Double, [Vect Double])
type Args = [String]

noArgs :: (MonadState DrawMats m, MonadIO m) => [(String, m ())]
noArgs = [ ("ident", ident)
         , ("apply", apply)
         , ("display", display)
         , ("clear", clear)
         ]

wArgs :: (MonadState DrawMats m, MonadIO m) => [(String, Args -> m ())]
wArgs = [ ("save", save)
        , ("line", line)
        , ("scale", scale)
        , ("move", move)
        , ("rotate", rote)
        , ("hermite", hermite)
        , ("bezier", bezier)
        , ("circle", circle)
        ]

main = do
    args <- getArgs
    script <- readFile (head args)
    let cmds = parse $ lines script :: [StateT DrawMats IO ()]
    runStateT (sequence_ cmds) (M.empty, T.ident, [])

parse :: (MonadState DrawMats m, MonadIO m) => Args -> [m ()]
parse []  = []
parse [a] =
    case lookup a noArgs of
        Just c  -> [c]
        Nothing -> []
parse (a:b:xs) =
    case lookup a noArgs of
        Just c0 -> c0 : (parse (b:xs))
        Nothing -> 
            case lookup a wArgs of
                Just c1 -> (c1 $ words b) : (parse xs)
                Nothing -> parse (b:xs)

circle :: (MonadState DrawMats m) => Args -> m ()
circle args =
    modify $ \(s, t, e) -> (s, t, e ++ pts)
        where [cx, cy, cz, r] = map read args
              pts = connectPts $ T.circle cx cy cz r

hermite :: (MonadState DrawMats m) => Args -> m ()
hermite args = do
    let (fX, fY) = T.genHermFxns args
        pts = L.zipWith4 Vect (T.sampleParam 128 fX) (T.sampleParam 128 fY)
                       (repeat 0) (repeat 1)
    modify $ \(s, t, e) -> (s, t,  e ++ (connectPts pts)) 

bezier :: (MonadState DrawMats m) => Args -> m ()
bezier args = do
    let (fX, fY) = T.genBezFxns args
        pts = L.zipWith4 Vect (T.sampleParam 128 fX) (T.sampleParam 128 fY)
                       (repeat 0) (repeat 1)
    modify $ \(s, t, e) -> (s, t,  e ++ (connectPts pts)) 

save :: (MonadState DrawMats m, MonadIO m) => Args -> m ()
save args = do
    let path = head args
    modify $ \(s, t, e) -> (T.drawEdges red e M.empty, t, e)
    (scrn, _, _) <- get
    liftIO $ do
        writeFile ".tempimg.ppm" (printPixels (500, 500) scrn)
        callProcess "convert" [".tempimg.ppm", path]
        removeFile ".tempimg.ppm"

display :: (MonadState DrawMats m, MonadIO m) => m ()
display = do
    modify $ \(s, t, e) -> (T.drawEdges red e M.empty, t, e)
    (scrn, _, _) <- get
    liftIO $ do
        writeFile ".tempimg.ppm" (printPixels (500, 500) scrn)
        callProcess "eog" [".tempimg.ppm"]
        removeFile ".tempimg.ppm"
--      (tempName, tempHandle) <- openTempFile "." "disp.ppm"
--      hPutStrLn tempHandle (printPixels (500, 500) scrn)
--      callProcess "eog" [tempName]
--      hClose tempHandle 
--      removeFile tempName
--              god damn why doesn't this work

line :: (MonadState DrawMats m) => Args -> m ()
line args = do
    let [x0, y0, z0, x1, y1, z1] = map read args
        ln = Line (Vect x0 y0 z0 1) (Vect x1 y1 z1 1)
            in modify $ \(s, t, edges) -> (s, t, addLine ln edges)

ident :: (MonadState DrawMats m) => m ()
ident = modify $
    \(s, _, es) -> (s, T.ident, es)

scale :: (MonadState DrawMats m) => Args -> m ()
scale args = modify $
    \(scrn, tform, edges) -> (scrn, tform `mappend` T.scale x y z, edges)
    where [x, y, z] = map read args

rote :: (MonadState DrawMats m) => Args -> m ()
rote s = modify $
    \(scrn, tform, edges) -> (scrn, tform `mappend` roti s, edges)
    where roti args
            | axis == "x"   = T.rotX theta
            | axis == "y"   = T.rotY theta
            | axis == "z"   = T.rotZ theta
            where axis  = args !! 0
                  theta = read $ args !! 1

move :: (MonadState DrawMats m) => Args -> m ()
move args = modify $
    \(scrn, tform, edges) -> (scrn, tform `mappend` T.trans x y z, edges)
    where [x, y, z] = map read args

clear :: (MonadState DrawMats m) => m ()
clear = modify $
    \(scrn, tform, _) -> (scrn, tform, [])

apply :: (MonadState DrawMats m) => m ()
apply = modify $
    \(scrn, tform, edges) -> (scrn, tform, T.mmult tform edges)
