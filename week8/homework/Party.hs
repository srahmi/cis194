module Party where
    
    import Employee

    glCons :: Employee -> GuestList -> GuestList
    glCons emp (GL employees f) = GL (employees ++ [emp]) (f + empFun emp)