import Others.RecursiveDataStructures  

data TrafficLight = Red | Yellow | Green  

instance Eq TrafficLight where  
    Red == Red = True  
    Green == Green = True  
    Yellow == Yellow = True  
    _ == _ = False

instance Show TrafficLight where  
    show Red = "Red light"  
    show Yellow = "Yellow light"  
    show Green = "Green light"  

class YesNo a where  
    yesno :: a -> Bool  

instance YesNo Int where  
    yesno 0 = False  
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    yesno = id

instance YesNo (Maybe a) where
    yesno Nothing = False
    yesno (Just _) = True

instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _ = True

yesnoIf :: (YesNo y) => y -> a -> a -> a  
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult

instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x leftSub rightSub) = Node (f x) (fmap f leftSub) (fmap f rightSub)


