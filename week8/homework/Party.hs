{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where
    
    import Employee

    instance Semigroup GuestList where
        (GL gl1 f1) <> (GL gl2 f2) = GL (gl1 ++ gl2) (f1 + f2)

    instance Monoid GuestList where
        mempty = GL [] 0

    glCons :: Employee -> GuestList -> GuestList
    glCons emp (GL employees f) = GL (employees ++ [emp]) (f + empFun emp)  

    moreFun :: GuestList -> GuestList -> GuestList
    moreFun gl1@(GL _ f1) gl2@(GL _ f2) = if f1 >= f2 then gl1 else gl2