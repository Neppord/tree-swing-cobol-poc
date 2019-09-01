module Minimal where

data Expr a
  = Value a
  | Add (Expr a) (Expr a)
  | Id String
  | Function [String] (Block a)
  | Call String [Expr a]
  deriving (Show)

data Block a =
  Block [(String, Expr a)]
  deriving (Show)

data Program a =
  Program String (Block a)
  deriving (Show)

evalProgram :: (Num a) => Program a -> Maybe a
evalProgram (Program key (Block m)) = do
  expr <- lookup key m 
  eval m expr

eval :: (Num a) => [(String, Expr a)] -> Expr a -> Maybe a
eval m (Value a) = Just a
eval m (Add b c) = (+) <$> (eval m b) <*> (eval m c)
eval m (Function args block) = Nothing
eval m (Call name args) = do
  (Function names (Block block)) <- lookup name m
  let m' = zip names args ++ block ++ m
  eval m' ( snd (last block))
eval m (Id key) = do
  expr <- lookup key m
  eval m expr

example = 
  Program  "x"
    ( Block
        [ ("f"
          , Function ["a", "b"] 
            ( Block 
                [ ("more", Add (Id "a") (Value 10))
                , ("f", Add (Id "more") (Id "b"))
                ]
            )
          )
        , ("x"
          , Call "f" [Value 3, Value 4]
          )
        ]
    )
