module Graphics.HaGL.Lib.Image (
    ImagePos, ImageColor, Image, ImageTransform,
    rgb1,
    fromImage,
    fromImageInteractive,
    liftToImage1,
    liftToImage2,
    liftToImage3
) where

import Graphics.HaGL

type ImagePos = FragExpr (Vec 2 Float)
type ImageColor = FragExpr (Vec 4 Float)
type Image = ImagePos -> ImageColor
type ImageTransform = ImagePos -> ImagePos

rgb1 :: FragExpr (Vec 3 Float) -> ImageColor
rgb1 c = app c 1

quad :: (VertExpr (Vec 4 Float), FragExpr (Vec 2 Float))
quad = let 
    quadPos = vert 
        [(vec2 (-1) (-1)), 
         (vec2 (-1) 1), 
         (vec2 1 (-1)), 
         (vec2 1 1)]
    in (quadPos $- vec2 0 1, frag quadPos)

fromImage :: Image -> GLObj
fromImage im = triangleStrip { position = vpos, color = color } where
    (vpos, pos) = quad
    color = im pos

fromImageInteractive :: Image -> GLObj
fromImageInteractive im = triangleStrip { position = vpos, color = color } where
    (vpos, pos) = quad
    dx = cond mouseLeft (mouseX - (prec mouseX mouseX)) 0
    dy = cond mouseLeft (mouseY - (prec mouseY mouseY)) 0
    zf = prec 1 (zf * (-0.5 * mouseWheel + 1))
    off = prec (vec2 0 0) (off - zf .# (vec2 dx dy))
    pos' = pos + uniform off
    pos'' = (uniform zf) .# (pos' - uniform off) + uniform off
    color = im pos''

liftToImage1 :: (ImageColor -> ImageColor) -> Image -> Image
liftToImage1 f im x = f (im x)

liftToImage2 :: (ImageColor -> ImageColor -> ImageColor) -> Image -> Image -> Image
liftToImage2 f im1 im2 x = f (im1 x) (im2 x)

liftToImage3 :: (ImageColor -> ImageColor -> ImageColor -> ImageColor) -> Image -> Image -> Image -> Image
liftToImage3 f im1 im2 im3 x = f (im1 x) (im2 x) (im3 x)
