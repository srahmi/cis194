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

        -- f Node (Emp "Stan" 9) [f Node (Emp "Bob" 2) [ f Node (Emp "Joe" 5) [ f Node (Emp "John" 1) [] , f Node (Emp "Sue" 5) []], f Node (Emp "Fred" 3) []], f Node (Emp "Sarah" 17) [ f Node (Emp "Sam" 4) []]]
        -- f Node (Emp "Stan" 9) [f Node (Emp "Bob" 2) [ f Node (Emp "Joe" 5) [(GL [Emp "John" 1] 1, GL [] 0), (GL [Emp "Sue" 5] 5, GL [] 0)], (GL [Emp "Fred" 3] 3, GL [] 0)], f Node (Emp "Sarah" 17) [(GL [Emp "Sam" 4] 4, GL [] 0)]]
        -- f Node (Emp "Stan" 9) [f Node (Emp "Bob" 2) [ (GL [Emp "Joe" 5, Emp "John" 1, Emp "Sue" 5] 11, GL [Emp "Sue" 5] 5) , (GL [Emp "Fred" 3] 3, GL [] 0)], (GL [Emp "Sarah" 17, Emp "Sam" 4] 21, GL [Emp "Sam" 4] 4)]
        -- f Node (Emp "Stan" 9) [(GL [Emp "Bob" 2, Emp "Fred" 3, Emp "Joe" 5, Emp "John" 1, Emp "Sue" 5] 16, GL [Emp "Sue" 5] 5), (GL [Emp "Sarah" 17, Emp "Sam" 4] 21, GL [Emp "Sam" 4] 4)]                                                                                          
        -- (GL [Emp "Stan" 9, Emp "Bob" 2, Emp "Fred" 3, Emp "Joe" 5, Emp "John" 1, Emp "Sue" 5, Emp "Sarah" 17, Emp "Sam" 4] 46, GL [Emp "Sarah" 17, Emp "Sam" 4, Emp "Bob" 2, Emp "Fred" 3, Emp "Joe" 5, Emp "John" 1, Emp "Sue" 5] 37)
    testGls :: [(GuestList, GuestList)]
    testGls = [
        (GL [Emp "Bob" 2] 2, GL [Emp "John" 4] 4),
        (GL [Emp "Said" 5, Emp "Oumaima" 7] 12, GL [Emp "Lara" 15, Emp "Hassan" 4] 19)
        ]

    testBoss :: Employee
    testBoss = Emp "Boss" 6

    empList :: [Employee]
    empList = [Emp "Name" 1, Emp "Name2" 3, Emp "Name3" 2]


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
    pretty (GL xs f) = "Total Fun: " ++ show f ++ "\n" ++ unlines (sortedGl xs)

    sortedGl :: [Employee] -> [String]
    sortedGl = map empName . sort