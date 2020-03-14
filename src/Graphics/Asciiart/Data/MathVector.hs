
module Graphics.Asciiart.Data.MathVector where
import Graphics.Asciiart.Type
import Graphics.Asciiart.Data.Type

-- | Represent One Vector image.
--
--
data MathVector
    -- | Represent simple Line.
    --
    -- Line a b lt represents math equation:
    -- y = ax + b
    = Line Float Float LetterType
    -- | Represent simple Circle.
    --
    -- Circle a b lt represents math equation:
    -- (x / a)^2 + (y / b)^2 = 1
    | Circle Float Float LetterType
    -- | Represent simple Parabola
    --
    -- Parabola a b c represents math equation:
    -- y = a(x - b)^2 + c
    | Parabola Float Float Float LetterType
    | Many [MathVector]
    -- | Limit of equation
    | Limit MathVector Float Float

instance Scalable MathVector where
    scaleX am m = case m of
        Line a b lt       -> Line (a * am) b lt
        Circle a b lt     -> Circle (a * am) b lt
        Parabola a b c lt -> Parabola (a * am) (b / am) c lt
        Many ms           -> Many $ map (scaleX am) ms
        Limit m' s e      -> Limit (scaleX am m') (s * am) (e * am)
    scaleY am m = case m of
        Line a b lt       -> Line (a / am) (b / am) lt
        Circle a b lt     -> Circle a (b * am) lt
        Parabola a b c lt -> Parabola (a / am) b (c / am) lt
        Many ms           -> Many $ map (scaleY am) ms
        Limit m' s e      -> Limit (scaleY am m') s e


type Space = (Int, Int)
data VectorSpace = VectorSpace MathVector Space

