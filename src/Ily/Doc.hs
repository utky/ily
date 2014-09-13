module Doc
    ( Scene(..)
    , Chapter(..)
    , Act(..)
    , (###)
    ) where

type Body = String
data Scene = Scene [Body]
data Chapter = Chapter [Scene]
data Act = Act [Chapter]


(###) :: String -> Scene
(###) = Scene . (:[])


