import Network.Socket
import System.IO
-- import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM
-- import Control.Concurrent.Chan
-- import Control.Monad
-- import Control.Monad.Fix (fix)
-- easier access to lists
-- import Control.Lens
import System.Environment
import Data.Maybe
import Data.String.Utils
import Data.List
import Data.List.Split
-- import System.IO.Unsafe
import Data.Function
import System.Random
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set

import System.Locale
import Data.Time

data UserType = Teacher | Student | Administrator deriving (Show, Eq)

data User = User {uid :: String, pw :: String, category :: UserType, subs :: Set String  } deriving (Show, Eq)

data Slot = Slot {start :: UTCTime, end :: UTCTime}


instance Show Slot where
    show s = show "(" ++ (show $ start s) ++ ", " ++ (show $ end s ) ++")"

instance Ord Slot where
    (<=) (a) (b) = start a <= start b && end a <= end b

instance Eq Slot where
    (==) (a) (b) = start a == start b && end a == end b


data Doodle = Doodle {name :: String, timeslots :: Map Slot (Set String)} deriving (Show)

instance Eq Doodle where
    (==) (a) (b) = name a == name b

stripChars :: String -> String -> String
stripChars = filter . flip notElem

main :: IO ()
main = withSocketsDo $ do
    [login, token] <- getArgs
    let userMap = Map.insert login User {
                                    uid      = login,
                                    pw       = token,
                                    category = Administrator,
                                    subs     = Set.empty
                                    } Map.empty
    transUserMap <- newTVarIO (userMap)
    let doodleMap = Map.empty :: Map String Doodle
    transDoodleMap <- newTVarIO (doodleMap)
    channel <- newChan
    -- AF_INET means support Internetwork such as UDP TCP
    -- Stream means it is a Stream sockets which use TCP and SCTP
    mySocket <- socket AF_INET Stream defaultProtocol
    bind mySocket (SockAddrInet 5002 0)
    listen mySocket 5
    loop mySocket channel transUserMap transDoodleMap

loop :: Socket -> t -> TVar (Map [Char] User) -> TVar (Map [Char] Doodle) -> IO b
loop s channel transUserMap transDoodleMap = do
    (sock, _) <- Network.Socket.accept s
    h <- socketToHandle sock ReadWriteMode
    forkIO $ handleConnection h transUserMap transDoodleMap
    loop s channel transUserMap transDoodleMap

handleConnection :: Handle -> TVar (Map [Char] User) -> TVar (Map [Char] Doodle) -> IO ()
handleConnection h  transUserMap transDoodleMap = do
    hSetBuffering h LineBuffering
    handleRequest h transUserMap transDoodleMap
    hClose h

-- Code block to h login
getUser :: [Char] -> Map [Char] User -> Maybe User
getUser loginInfo userMap
        | maybeUser == Nothing = Nothing
        | otherwise =
            let user = fromJust maybeUser
              in if pw user == enterPw then maybeUser else Nothing
            -- maybeUser
        where [uidStr, enterPw] = Data.String.Utils.split "@" loginInfo
              maybeUser        = Map.lookup uidStr userMap

handleRequest::Handle -> TVar (Map [Char] User) -> TVar ( Map [Char] Doodle ) -> IO b
handleRequest h transUserMap transDoodleMap =
    do
        maybeTokens <- tokenizeRule h
        if maybeTokens == Nothing
            then do
                    hPutStrLn h "invalid inputString\n"
                    handleRequest h transUserMap transDoodleMap
        else do
            let tokens = fromJust maybeTokens
            let login = tokens !! 1
            -- tokens@(_: login: _) <- tokenizeRule h
            hPutStrLn h $ "tokens\n" ++ ( show tokens )
            userMap <- atomically $ readTVar transUserMap
            doodleMap <- atomically $ readTVar transDoodleMap
            let user = getUser login userMap
            if user /= Nothing
                then respAction tokens h (fromJust user) transUserMap userMap transDoodleMap doodleMap
            else hPutStrLn h "wrong login"

            newUserMap <- atomically $ readTVar transUserMap
            newDoodleMap <- atomically $ readTVar transDoodleMap
            hPutStrLn h ("userMap\n" ++ show newUserMap)
            hPutStrLn h ("doodleMap\n" ++ show newDoodleMap)

            handleRequest h transUserMap transDoodleMap

respAction :: [[Char]] -> Handle -> User -> TVar (Map [Char] User)
    -> Map [Char] User -> TVar (Map [Char] Doodle) -> Map [Char] Doodle -> IO ()
respAction [action, _, identifier] h user transUserMap userMap _ _
    | addRules && not isAdmin = hPutStrLn h "you are not admin!"
    | addRules && userExists = hPutStrLn h "id taken"
    | addRules =
            let newUserMap = Map.insert (identifier) User {
                                            uid      = identifier,
                                            pw       = "aaaa",
                                            category = usertype,
                                            subs     = Set.empty
                                            } userMap
            in do
                atomically $ writeTVar transUserMap (newUserMap)
                hPutStrLn h "ok"
    where
          addRules = action == "add-teacher" || action == "add-student"
          isAdmin = category user == Administrator
          userExists = userExist identifier userMap
          usertype = if action == "add-teacher" then Teacher else Student
          -- TODO
          -- randomStr = take 4 $ randomRs ('a','z') $ unsafePerformIO newStdGen

respAction ["set-doodle", _, doodleId, slotStr] h user _ _ transDoodleMap doodleMap
    | not isAdmin = hPutStrLn h "you are not Admin"
    | maybeTs == Nothing = hPutStrLn h "slot strings are malformatted"
    | not (doodleExist doodleId doodleMap) && isAdmin =
        let newDoodle = Doodle {name = doodleId,
                                timeslots = fromJust maybeTs
                               }
        in do
            -- let tss = splitOn "," $ stripChars "[]" $ init slotStr
            -- hPutStrLn h ("timeslots" ++ show ts)
            atomically $ writeTVar transDoodleMap (Map.insert doodleId newDoodle doodleMap)
            hPutStrLn h "ok"
    | otherwise = hPutStrLn h "id-taken"
    where timeList  = splitOn "," $ stripChars "[]" $ init slotStr
          maybeTs   = parseSlots timeList
          isAdmin   = category user == Administrator

respAction ["subscribe", _, doodleId] h user transUserMap userMap _ doodleMap
    | maybeDoodle == Nothing = hPutStrLn h "doodle does not exist"
    | subscribed = hPutStrLn h "user is already subscribed"
    | maybeDoodle /= Nothing =
        let newUserMap = Map.insert (uid user) User {
                                                uid      = uid user,
                                                pw       = pw user,
                                                category = category user,
                                                subs     = newSubs
                                               } userMap
        in do
              atomically $ writeTVar transUserMap (newUserMap)
              hPutStrLn h "ok"
    where maybeDoodle = Map.lookup doodleId doodleMap
          doodle      = fromJust maybeDoodle
          subscribed  = Set.member (uid user) (subs user)
          newSubs     = Set.insert (name doodle) $ subs user

respAction ["prefer", _, doodleId, slotString] h user _ _ transDoodleMap doodleMap
    | maybeDoodle == Nothing = hPutStrLn h "no-such-id"
    | not subscribed  = hPutStrLn h "not-subscribed"
    | maybeOldPrefs == Nothing = hPutStrLn h "timeslot does not exist!"
    | subscribed =
        let newDoodleMap = Map.insert doodleId Doodle {
                                                      name = doodleId,
                                                      timeslots = newTimeslots
                                               } doodleMap
        in do
            atomically $ writeTVar transDoodleMap (newDoodleMap)
            hPutStrLn h "ok"
    where
          doodle        = fromJust maybeDoodle
          subscribed    = Set.member (name doodle) (subs user)
          maybeDoodle   = Map.lookup doodleId doodleMap
          ts            = fromJust $ parseSlot slotString
          maybeOldPrefs = Map.lookup ts (timeslots doodle)
          oldPrefs      = fromJust maybeOldPrefs
          newPrefs      = Set.insert (uid user) oldPrefs
          newTimeslots  = Map.insert ts newPrefs (timeslots doodle)

respAction ["exam-schedule", _] h _ _ userMap _ doodleMap =
        -- let sch = calcSchedule userMap doodleMap
        -- All possible schedules
        let sch = sequence [[(name d, ts, getVotes ts d)
                               | ts <- (Map.keys $ timeslots d)]
                               | d <- (Map.elems doodleMap)]
            sumSeq = [(Map.fromList $ map (\(did, t, _) -> (did, t)) s, _sumSnd s)| s <- sch]
            sortSeq = reverse $ sortBy (compare `on` snd) sumSeq
            times = _getBestSchedule sortSeq userMap
        in do
              hPutStrLn h $ "COMBS" ++ show times

_getBestSchedule :: [(Map String Slot, t)] -> Map k User -> Maybe (Map String Slot, t, Bool)
_getBestSchedule sch userMap =
     find (\(_, _, b)-> b==True) [(s, p, all (==False) [
                                 allOverlap $ map fromJust $ filter (/=Nothing) userSlotList |
                                 userSlotList <- takenSlots userMap s]
                                ) | (s, p) <- sch
                               ]
    where takenSlots uMap schedule = [[Map.lookup did schedule |
                          did <- Set.elems $ subs user]|
                          user <- Map.elems uMap
                          ]
_sumSnd :: [(t, t1, Int)] -> Int
_sumSnd = foldr (\(_, _, c) acc -> c + acc) 0
-- calcSchedule userMap doodleMap =

-- hasNoDups:: [Slot] -> Bool
-- hasNoDups l = ( length $ nubBy allOverlap l ) == length l
_overlap:: Eq a => (a, Slot) -> (a, Slot) -> Bool
_overlap (i,a) (j, b) = helper a b && i /= j
    where helper x y = start x >= start y && start x <= end y || end x >= start y && end x <= end y

allOverlap:: [Slot] -> Bool
allOverlap xs =
        any (==True) $ concat [[_overlap x y |x <- zipped]| y <- zipped]
        where zipped = zip [0 ..] xs

-- -- error state
-- respAction _ h _ _ _ _ _=
--     do
--         hPutStrLn h "entered: error_state"

userExist :: (Eq a, Ord k) => k -> Map k a -> Bool
userExist login userMap = if user /= Nothing then True else False
    where user = Map.lookup login userMap

doodleExist :: (Eq a, Ord k) => k -> Map k a -> Bool
doodleExist identifier doodleMap =
    if maybeDoodle/= Nothing then True else False
    where maybeDoodle = Map.lookup identifier doodleMap

-- remove whitespace in slot string
removeWSslotString :: [[Char]] -> [[Char]]
removeWSslotString ("set-doodle": login : doodleId : rest)
        | startswith "[" $ head rest =
        ["set-doodle", login, doodleId, restStr]
        where restStr = concat rest
removeWSslotString args = args

tokenizeRule :: Handle -> IO (Maybe [[Char]])
tokenizeRule h =
    do
        line <- hGetLine h
        -- whitespace rmoval from time slot string
        let args = removeWSslotString $ splitWs line
        -- hPutStrLn h $ "chArgs" ++ show args
        tokens <- _tokenizeRule h args
        return (tokens)

_tokenizeRule :: Handle -> [[Char]] -> IO (Maybe [[Char]])
_tokenizeRule _ (["exam-schedule", login]) =
        return (Just["exam-schedule", login])

_tokenizeRule h (action : oldArgs)
    | length oldArgs < 2 =
        do
            inpStr <- hGetLine h
            let args = splitWs inpStr
            _tokenizeRule h ((action : oldArgs) ++ args)


_tokenizeRule h (["set-doodle", login, doodleId]) =
    do
        line <- hGetLine h
        maybeSlotStr <- parseDoodleStr h line
        if maybeSlotStr /= Nothing
            then return (Just ["set-doodle", login, doodleId, fromJust maybeSlotStr])
        else return Nothing

_tokenizeRule h ["prefer", login, token] =
    do
        inpStr <- hGetLine h
        let args = splitWs inpStr
        _tokenizeRule h (["prefer", login, token] ++ args)

_tokenizeRule _ (["prefer", login, token, slotStr]) =
        return (Just ["prefer", login, token, slotStr])

-- all other actions
_tokenizeRule _ ([action, login, token]) =
        return (Just [action, login, token])

_tokenizeRule h tokens@(["set-doodle", login, doodleId, str])
    | startswith "[" str && endswith "]" str =
            return (Just tokens)
    | startswith "[" str =
            do
               maybeSlotStr <- _parseDoodleStr h str
               if maybeSlotStr /= Nothing
                   then return (Just ["set-doodle", login, doodleId, fromJust maybeSlotStr])
                else return Nothing
    | otherwise =
            do
               return Nothing

_tokenizeRule h (action : oldArgs)
    | length oldArgs < 2 =
        do
            inpStr <- hGetLine h
            let args = splitWs inpStr
            _tokenizeRule h ((action : oldArgs) ++ args)


_tokenizeRule _ _ = return Nothing

parseDoodleStr:: Handle -> [Char] -> IO (Maybe String)
parseDoodleStr h line
    | startswith "[" line && endswith "]" ( strip line ) =
        return (Just $ stripChars "\t" line)
    | startswith "[" line =
        _parseDoodleStr h (stripChars " \t" line)
    | otherwise = return Nothing

_parseDoodleStr :: Handle -> [Char] -> IO (Maybe String)
_parseDoodleStr h acc =
    do
        line <- hGetLine h
        let count = countLetters line ']'
        if ( endswith "]" line )
            then return (Just $ stripChars "\t" $ acc ++ line)
            else if count == 0
                     then (_parseDoodleStr h (acc ++ line))
                else return Nothing
    where countLetters str c = length $ filter (== c) str

getVotes :: Slot -> Doodle -> Int
getVotes ts doodle =
        if preferences /= Nothing then Set.size (fromJust preferences) else 0
    where preferences = Map.lookup ts $ timeslots doodle

parseSlots :: [String] -> Maybe (Map Slot (Set String))
parseSlots tss = foldl (\acc str ->
                    let ts = parseSlot str
                        tsExistNot = Map.lookup (fromJust ts) ( fromJust acc ) == Nothing
                    in if ts /= Nothing && tsExistNot
                           then Just $ Map.insert (fromJust ts) Set.empty (fromJust acc)
                       else Nothing
                 ) (Just Map.empty) tss

parseSlot :: [Char] -> Maybe Slot
parseSlot ts
    | length tsList == 2 && checkTimeStr st && checkTimeStr et =
    Just Slot {start = timeFromString st, end = timeFromString et}
    | otherwise = Nothing
    where tsList@(st : et : _) = splitOn "/" $ stripChars " " ts

timeFromString:: String -> UTCTime
timeFromString ds = readTime defaultTimeLocale "%FT%R%z" ( ds :: String ) :: UTCTime

checkTimeStr:: String -> Bool
checkTimeStr ds = if ( utcTime ds ) /= Nothing then True else False
    where utcTime str = parseTime defaultTimeLocale "%FT%R%z" str :: Maybe UTCTime
