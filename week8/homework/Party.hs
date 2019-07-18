{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where

    import Employee
    import Data.Tree
    import Data.List(sort)

    instance Semigroup GuestList where
        (GL gl1 f1) <> (GL gl2 f2) = GL (gl1 ++ gl2) (f1 + f2)

    instance Monoid GuestList where
        mempty = GL [] 0

    glCons :: Employee -> GuestList -> GuestList
    glCons emp (GL employees f) = GL (employees ++ [emp]) (f + empFun emp)

    moreFun :: GuestList -> GuestList -> GuestList
    moreFun gl1@(GL _ f1) gl2@(GL _ f2) = if f1 >= f2 then gl1 else gl2

    treeFold :: (a -> [b] -> b) -> Tree a -> b
    treeFold f = fold' where
        fold' (Node x ts) = f x (map fold' ts)

    testGls = [
        (GL [Emp "Bob" 2] 2, GL [Emp "John" 4] 4),
        (GL [Emp "Said" 5, Emp "Oumaima" 7] 12, GL [Emp "Lara" 15, Emp "Hassan" 4] 19)
        ]

    testBoss = Emp "Boss" 6

    empList = [Emp "Name" 1, Emp "Name2" 2]

    --foldMap folds and apply mconcat
    nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
    nextLevel boss gls = (withBoss, withoutBoss) where
        withBoss    = glCons boss $ foldMap fst gls
        withoutBoss = foldMap (uncurry moreFun) gls

    maxFun :: Tree Employee -> GuestList
    maxFun = uncurry moreFun . treeFold nextLevel

    readHirarchy :: FilePath -> IO (Tree Employee)
    readHirarchy path = do
        hirarchy <- readFile path
        return (read hirarchy)

    main :: IO ()
    main = readFile "company.txt" >>= putStrLn . pretty . maxFun . read

    pretty :: GuestList -> String
    pretty (GL xs f) = "Total Fun: " ++ show f 

    -- sortedGl :: [Employee] -> String
    -- sortedGl gl = map (show empName) gl
