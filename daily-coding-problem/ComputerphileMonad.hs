module ComputerpjileMonad where

    data Expr = Val Int | Div Expr Expr

    eval :: Expr -> Maybe Int
    eval (Val i) = Just i
    eval (Div expr1 expr2) = case eval expr1 of 
                                Nothing -> Nothing
                                Just i  -> case eval expr2 of 
                                             Nothing -> Nothing
                                             Just j  -> safeDiv i j
                    
    eval' :: Expr -> Maybe Int
    eval' (Val a)   = return a
    evla' (Div x y) = do n <- eval' x
                         m <- eval' y
                         safeDiv n m

    safeDiv :: Int -> Int -> Maybe Int
    safeDiv x y = if y == 0 then 
                     Nothing 
                  else 
                     Just (x `div` y)