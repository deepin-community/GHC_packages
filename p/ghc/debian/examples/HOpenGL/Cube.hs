
-- Written by Ian Lynagh <igloo@debian.org> in 2003.
-- Released into the public domain.

module Main (main) where

import Graphics.UI.GLUT
import System(ExitCode(..), exitWith)
import System.Time (getClockTime, ClockTime(..))
import Data.IORef

type St = (ClockTime, GLfloat, GLfloat, GLfloat)

dx, dy, dz :: GLfloat
dx = 0.02
dy = 0.05
dz = 0.09

red, green, yellow, blue, purple, cyan :: Color3 GLfloat
red    = Color3 1 0 0
green  = Color3 0 1 0
yellow = Color3 1 1 0
blue   = Color3 0 0 1
purple = Color3 1 0 1
cyan   = Color3 0 1 1

main :: IO ()
main = do getArgsAndInitialize
          initialDisplayMode $= [ WithDepthBuffer, DoubleBuffered, RGBAMode ]
          _win <- createWindow "Hello World"
          myInit
          depthFunc $= Just Less
          drawBuffer $= BackBuffers
          t <- getClockTime
          ref <- newIORef (t, 0, 0, 0)
          displayCallback $= (display ref)
          keyboardMouseCallback $= Just keyboardMouse
          idleCallback $= Just (inc_anim ref)
          mainLoop

inc_anim :: IORef St -> IO ()
inc_anim x = do (t, r_x, r_y, r_z) <- readIORef x
                t' <- getClockTime
                let d = fromIntegral $ getMilliSecDiff t' t
                writeIORef x (t', r_x + d*dx, r_y + d*dy, r_z + d*dz)
                draw_colourful_cube x

myInit :: IO () 
myInit = do clearColor $= (Color4 0.0 0.0 0.0 0.0)
            matrixMode $= Projection
            loadIdentity
            -- ortho 0.0 1.0 0.0 1.0 (-1.0) 1.0
            -- frustum left right bottom top near far
            frustum (-1) 1 (-1) 1 (-1) 40
            matrixMode $= Modelview 0

keyboardMouse :: KeyboardMouseCallback
keyboardMouse (Char '\27') _ _ _ = exitWith ExitSuccess
keyboardMouse (Char 'q')   _ _ _ = exitWith ExitSuccess
keyboardMouse _ _ _ _ = return ()

display :: IORef St -> DisplayCallback
display x = draw_colourful_cube x

draw_colourful_cube :: IORef St -> IO ()
draw_colourful_cube x =
               do (_, r_x, r_y, r_z) <- readIORef x
                  clear [DepthBuffer, ColorBuffer]
                  loadIdentity
                  rotate r_x (Vector3 1 0 0 :: Vector3 GLfloat)
                  rotate r_y (Vector3 0 1 0 :: Vector3 GLfloat)
                  rotate r_z (Vector3 0 0 1 :: Vector3 GLfloat)
                  mapM_ draw_face (zip colours faces)
                  swapBuffers
    where draw_face :: (Color3 GLfloat, IO ()) -> IO ()
          draw_face (colour, face) = do color colour
                                        renderPrimitive Quads face
          faces = map (mapM_ vertex) face_vertices :: [IO ()]
          colours = [red, green, yellow, blue, purple, cyan]
          face_vertices = [
                 [Vertex3     to     to     to,
                  Vertex3   from     to     to,
                  Vertex3   from   from     to,
                  Vertex3     to   from     to],
                 [Vertex3     to     to   from,
                  Vertex3   from     to   from,
                  Vertex3   from   from   from,
                  Vertex3     to   from   from],
                 [Vertex3     to     to     to,
                  Vertex3   from     to     to,
                  Vertex3   from     to   from,
                  Vertex3     to     to   from],
                 [Vertex3     to   from     to,
                  Vertex3   from   from     to,
                  Vertex3   from   from   from,
                  Vertex3     to   from   from],
                 [Vertex3     to     to     to,
                  Vertex3     to   from     to,
                  Vertex3     to   from   from,
                  Vertex3     to     to   from],
                 [Vertex3   from     to     to,
                  Vertex3   from   from     to,
                  Vertex3   from   from   from,
                  Vertex3   from     to   from]]

from, to :: GLfloat
from = -0.4
to   =  0.4

getMilliSecDiff :: ClockTime -> ClockTime -> Integer
getMilliSecDiff (TOD s1 u1) (TOD s2 u2) =
    let d = (s1 - s2) * sec + u1 `div` to_milli - u2 `div` to_milli
    in if d >= 0 then d else error "Time going backwards"
  where sec = 10^(3 :: Int)
        to_milli = 10^(9 :: Int)

