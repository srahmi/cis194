module Week9 where

    type Name = String

    data Employee = Employee { name :: Name
                             , phone :: String }
                    deriving Show         

    name1, name2 :: Maybe Name
    name1 = Nothing
    name2 = Just "Brent"

    phone1, phone2 :: Maybe String
    phone1 = Nothing
    phone2 = Just "06-38-52-90-20"

    ex0 = Employee <$> name1 <*> phone1
    ex1 = Employee <$> name1 <*> phone2
    ex2 = Employee <$> name2 <*> phone1
    ex3 = Employee <$> name2 <*> phone2