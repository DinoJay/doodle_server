import Doodle (Doodle(initialize, add, remove, toogle), Pool(freshKey, get, set), run) 
import Data.List
import System.Locale
import Data.Time 
import Data.Time.Format
import qualified Data.Map.Strict as Map

-- Test with the following Strings
-- Just (Right ("2015-08-19T12:00+01:00", "2015-09-19T12:00+01:00"))
-- Just (Right ("2015-09-19T12:00+01:00", "2015-10-19T12:00+01:00"))
-- Right 1
-- Just (Left 1)
-- Data for time slots in Doodle 
data Slot t = Slot { slot :: (t, t)
                 , participants :: [String] } deriving (Show)


instance Eq t => Eq (Slot t) where
	(==) slot1 slot2 = fst (slot slot1) == fst (slot slot2) && snd (slot slot1) == snd (slot slot2)

instance Ord t => Ord (Slot t) where
	(<=) slot1 slot2 = fst (slot slot1) <= fst (slot slot2)
	(>=) slot1 slot2 = fst (slot slot1) >= fst (slot slot2)
	(<) slot1 slot2 = fst (slot slot1) < fst (slot slot2)
	(>) slot1 slot2 = fst (slot slot1) > fst (slot slot2)

-- Data and Instance for MyDoodle
data MyDoodle t = MyDoodle { title :: String
                   , slots :: [Slot t] } 

instance Doodle MyDoodle where
    initialize newTitle = MyDoodle {title = newTitle, slots = []}
    add (s,e) doodle = MyDoodle {title = title doodle, slots = insertSlot (slots doodle) (s,e)} 
    remove key doodle = MyDoodle {title = title doodle, slots = removeNth key (slots doodle) }
    toogle name key doodle = MyDoodle {title = title doodle, slots = myToogle name key (slots doodle) } 

instance (Show t, Ord t, Eq t) => Show (MyDoodle t) where
	show doodle = myShow doodle   

insertSlot :: (Ord t) =>  [Slot t] -> (t, t) -> [Slot t]
insertSlot slots (start, end) = -- if noConflict slots (start,end)
								insert (Slot {slot = (start, end), participants = []}) slots
								-- else slots

removeNth :: Int -> [Slot t] -> [Slot t]
removeNth key [] = []
removeNth key slots = let (xs, ys) = splitAt key slots in xs ++ (tail ys)

myToogle :: String -> Int -> [Slot t] -> [Slot t]
myToogle name key slots = let theSlot = slots !! key in replaceNth key ( Slot { slot = slot theSlot, participants = participants theSlot ++ [name] }) slots

myShow :: Show t => (MyDoodle t) -> String
myShow doodle = let
					myTitle = title doodle 
					rows = [[myTitle]]++[(\(start, end) -> [show start, show end]) (slot s) ++ participants s | s <- (slots doodle)]
					widths = [sum [length r + 3 | r <- s] - 1| s <- rows]
					header = "+" ++ replicate (maximum widths) '-' ++ "+"
				in 
					unlines $ [header] ++ tail (concat [[separator row header] ++ [fillCols row header] | row <- rows ]) ++ [header]

-- Helper function to replace the Nth element.
replaceNth :: Int -> a -> [a] -> [a]
replaceNth n newVal (x:xs)
     | n == 0 = newVal:xs
     | otherwise = x:replaceNth (n-1) newVal xs

-- Helper function to detect time clash
noConflict :: Ord t => [Slot t] -> (t, t) -> Bool
noConflict slots (start,  end) = all (conflictTest (start, end)) slots

conflictTest :: Ord t => (t, t) -> Slot t -> Bool
conflictTest (aStart, aEnd) aSlot = let (start, end) = slot aSlot in (aStart < start && aEnd <= end || aStart >= end) 

-- Helper function to put '|' between strings
fillCols :: [String] -> String -> String
fillCols xs header = let 
						front = "| " ++ intercalate " | " xs
						width = length header - length front - 1
					 in 
					 	front ++ replicate (width-1) ' ' ++ " |"

separator :: [String] -> String -> String
separator xs header = let 
						front = "+-" ++ intercalate "-+-" [replicate (length x) '-'| x <-xs]
						width = length header - length front - 1
					 in 
					 	front ++ replicate (width-1) '-' ++ "-+"
-- Data and Instance for Pool.
data MyPool k d = MyPool { keys :: [k]
                     , pairs :: Map.Map k d} deriving Show

instance Pool MyPool where
	freshKey pool =  head $ keys pool
	get key pool = Map.lookup key (pairs pool) 
	set key doodle pool = MyPool { keys = tail (keys pool), pairs = Map.insert key doodle (pairs pool)  }


emptyDoodle :: MyPool Int (MyDoodle String)
emptyDoodle = MyPool { keys = [1..], pairs = Map.empty }

main :: IO ()
main = run emptyDoodle