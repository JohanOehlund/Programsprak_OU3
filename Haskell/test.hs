module Stack (Stack, empty, isEmpty, push, top, pop) where
 
empty :: Stack a
isEmpty :: Stack a -> Bool
push :: a -> Stack a -> Stack a
top :: Stack a -> a
pop :: Stack a -> (a,Stack a)
 
newtype Stack a = StackImpl [a] deriving(Show, Eq, Ord) -- opaque!
empty = StackImpl []
isEmpty (StackImpl s) = null s
push x (StackImpl s) = StackImpl (x:s)
top (StackImpl s) = head s
pop (StackImpl (s:ss)) = (s,StackImpl ss)

test :: (Num a)=>Integer-> a-> Stack a-> Stack a 
test num item st
    | num > 0 = ((test) (num-1) item (push item st))
    | num < 0 = ((test) (num+1) item (push item st))
    | num== 0 = st




-- Typsynonym
type StringPair = (String, String)
-- Typinkapsling med deriverade typklassinstanser
newtype NamnTyp =  Namn (String, String) deriving (Eq, Ord, Show)

efternamn :: NamnTyp -> String
efternamn (Namn (_,y)) = y

-- Hade funkat för type NamnTyp = (String, String)
-- efternamn = snd

-- Enkel ny datatyp med värdekonstruktorer
data WeekEndDay = Friday | Saturday | Sunday deriving (Eq, Ord, Show)

-- Parametriserad, rekursiv datatyp
data List' a = EmptyList | ListItem a (List' a) deriving (Eq, Ord, Show)

-- Datatyp med flera olika distinkta värdekonstruktorer
data UserType = NotAUser | User String Int | Uid Int | Username String

-- En instans av typklassen Show
instance Show UserType where
  show (NotAUser) = "Not a user"
  show (User x y) = x ++ " " ++ (show y)
  show (Uid x)  = show x
  show (Username x) = x
