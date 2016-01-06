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
import System.IO.Unsafe
import System.Random
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set

data UserType = Teacher | Student | Administrator deriving (Show, Eq)

data User = User {uid :: String, pw :: String, category :: UserType} deriving (Show, Eq)

data Slot = Slot {start :: String, end :: String} deriving Show

instance Ord Slot where
    (<=) (a) (b) = start a <= start b && end a <= end b

instance Eq Slot where
    (==) (a) (b) = start a == start b && end a == end b

data Doodle = Doodle { name :: String, timeslots :: Map Slot (Set String), subs :: Set String } deriving (Show)


instance Eq Doodle where
    (==) (a) (b) = name a == name b

-- data Request = Request { action :: String, login :: String, doodle :: Doodle, slot ::  } deriving (Show, Eq)

stripChars :: String -> String -> String
stripChars = filter . flip notElem

main :: IO ()
main = withSocketsDo $ do
    [login, token] <- getArgs
    let userMap = Map.insert login User {uid = login,
                                    pw = token,
                                    category = Administrator
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
    loop mySocket channel transUserMap transDoodleMap 0

loop socket channel transUserMap transDoodleMap number = do
    (sock, addr) <- Network.Socket.accept socket
    h <- socketToHandle sock ReadWriteMode
    forkIO $ handleConnection h channel transUserMap transDoodleMap number
    loop socket channel transUserMap transDoodleMap $ number + 1

--handleConnection :: (Socket, SockAddr) -> Chan String -> Int -> IO ()
handleConnection h channel transUserMap transDoodleMap number = do
    hSetBuffering h LineBuffering
    handleRequest h transUserMap transDoodleMap
    hClose h

-- Code block to h login
getUser loginInfo userMap =
    do
        let [login, myToken] = Data.String.Utils.split "@" loginInfo
        Map.lookup login userMap

userLoginTest [myLogin, myToken] user =
    if (uid user) == myLogin && (pw user) == myToken
    then True
    else False

handleRequest::Handle -> TVar (Map [Char] User) -> TVar ( Map [Char] Doodle ) -> IO b
handleRequest h transUserMap transDoodleMap =
    do
        tokens@(_: login: _) <- tokenizeRule h
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

respAction [action, _, identifier] h user transUserMap userMap _ _
    | not isAdmin = hPutStrLn h "you are not admin!"
    | userExists = hPutStrLn h "id taken"
    | action == "add-teacher" || action == "add-student" =
            let newUserMap = Map.insert identifier User {
                                            uid = identifier,
                                            pw = randomStr,
                                            category = usertype
                                            } userMap
            in
            atomically $ writeTVar transUserMap (newUserMap)
    where
          isAdmin = category user == Administrator
          userExists = userExist identifier userMap
          usertype = if action == "add-teacher" then Teacher else Student
          -- TODO
          randomStr = take 4 $ randomRs ('a','z') $ unsafePerformIO newStdGen

respAction ["set-doodle", _, doodleId, slotStr] h user _ _ transDoodleMap doodleMap
    | not isAdmin = hPutStrLn h "you are not Admin"
    | maybeTs == Nothing = hPutStrLn h "slot strings are malformatted"
    | not (doodleExist doodleId doodleMap) && isAdmin =
        let newDoodle = Doodle {name = doodleId,
                                timeslots = fromJust maybeTs,
                                subs = Set.empty
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

respAction ["subscribe", login, doodleId] h user _ _ transDoodleMap doodleMap
    | maybeDoodle == Nothing = hPutStrLn h "doodle does not exist"
    | subscribed = hPutStrLn h "user is already subscribed"
    | maybeDoodle /= Nothing =
        let newDoodleMap = Map.insert doodleId Doodle {name = doodleId,
                                           timeslots = timeslots doodle,
                                           subs = newSubs
                                           }
                                           doodleMap
        in do
              atomically $ writeTVar transDoodleMap (newDoodleMap)
              hPutStrLn h "ok"
    where maybeDoodle = Map.lookup doodleId doodleMap
          doodle      = fromJust maybeDoodle
          subscribed  = Set.member (uid user) (subs doodle)
          newSubs     = Set.insert (uid user) $ subs doodle

respAction ["prefer", login, doodleId, slotString] h user transUserMap userMap transDoodleMap doodleMap
    | maybeDoodle == Nothing = hPutStrLn h "no-such-id"
    | not subscribed  = hPutStrLn h "not-subscribed"
    | subscribed =
        let newDoodleMap = Map.insert doodleId Doodle {name = doodleId,
                                                 timeslots = newTimeslots,
                                                 subs = subs doodle
                                               } doodleMap
        in do
        atomically $ writeTVar transDoodleMap (newDoodleMap)
        hPutStrLn h "ok"
    where
          subscribed    = Set.member (uid user) (subs doodle)
          maybeDoodle   = Map.lookup doodleId doodleMap
          doodle        = fromJust maybeDoodle
          ts            = fromJust $ parseSlot slotString
          maybeOldPrefs = Map.lookup ts (timeslots doodle)
          oldPrefs      = if maybeOldPrefs /= Nothing
                              then fromJust maybeOldPrefs
                          else Set.empty
          newPrefs      = Set.insert (uid user) oldPrefs
          newTimeslots  = Map.insert ts newPrefs (timeslots doodle)




-- error state
respAction _ h _ _ _ _ _=
    do
        hPutStrLn h "entered: error_state"

userExist login userMap = if user /= Nothing then True else False
    where user = Map.lookup login userMap

doodleExist identifier doodleMap =
    if maybeDoodle/= Nothing then True else False
    where maybeDoodle = Map.lookup identifier doodleMap

-- remove whitespace in slot
removeWs :: [[Char]] -> [[Char]]
removeWs ("set-doodle": login : doodleId : rest)
        | startswith "[" $ head rest =
        ["set-doodle", login, doodleId, restStr]
        where restStr = concat rest
removeWs args = args

tokenizeRule :: Handle -> IO [String]
tokenizeRule h =
    do
        line <- hGetLine h
        -- whitespace rmoval from time slot string
        let args = removeWs $ splitWs line
        -- hPutStrLn h $ "chArgs" ++ show args
        tokens <- _tokenizeRule h args
        return (tokens)

_tokenizeRule :: Handle -> [String] -> IO [String]
_tokenizeRule _ (["exam-schedule", login]) = return (["exam-schedule", login])

_tokenizeRule h (action : oldArgs)
    | length oldArgs < 2 =
        do
            inpStr <- hGetLine h
            let args = splitWs inpStr
            _tokenizeRule h ((action : oldArgs) ++ args)


_tokenizeRule h (["set-doodle", login, doodleId]) =
    do
        line <- hGetLine h
        slotStr <- readDoodle h line
        return (["set-doodle", login, doodleId, slotStr])

_tokenizeRule h ["prefer", login, token] =
    do
        inpStr <- hGetLine h
        let args = splitWs inpStr
        _tokenizeRule h (["prefer", login, token] ++ args)

_tokenizeRule _ (["prefer", login, token, slotStr]) =
        return (["prefer", login, token, slotStr])

-- all other actions
_tokenizeRule _ ([action, login, token]) =
        return ([action, login, token])

_tokenizeRule h tokens@(["set-doodle", login, doodleId, str])
    -- TODO: regex
    | startswith "[" str && endswith "]" str =
            return (tokens)
    | startswith "[" str =
            do
               slotStr <- readDoodle' h str
               return (["set-doodle", login, doodleId, slotStr])
    | otherwise =
            do
               hPutStrLn h "error slotstring does not start with ["
               return ["error"]

_tokenizeRule h (action : oldArgs)
    | length oldArgs < 2 =
        do
            inpStr <- hGetLine h
            let args = splitWs inpStr
            _tokenizeRule h ((action : oldArgs) ++ args)


_tokenizeRule h _ =
    do
       hPutStrLn h "error state"
       return ["error"]



readDoodle :: Handle -> [Char] -> IO [Char]
readDoodle h line
    | startswith "[" line && endswith "]" stLine =
        return (stripChars "\t" line)
    | startswith "[" line =
        do
            readDoodle' h (stripChars "\t" line)
    | otherwise = error "doole does not begin with ["
    where stLine = strip line

readDoodle' :: Handle -> [Char] -> IO [Char]
readDoodle' h acc =
    do
        line <- hGetLine h
        if (endswith "]" line)
            then return (stripChars "\t" $ acc ++ line)
            else (readDoodle' h (acc ++ line))

-- -- parseSlots :: [[Char]] -> [Slot]

getVotes ts doodleMap =
        if userSet /= Nothing then Just $ length (fromJust userSet) else Nothing
    where userSet = Map.lookup ts doodleMap

parseSlots :: [String] -> Maybe (Map Slot (Set String))
parseSlots tss = foldl (\acc str ->
                    let ts = parseSlot str
                        tsExistNot = Map.lookup (fromJust ts) ( fromJust acc ) == Nothing
                    in if ts /= Nothing && tsExistNot
                           then Just $ Map.insert (fromJust ts) Set.empty (fromJust acc)
                       else Nothing
                 ) (Just Map.empty) tss

parseSlot ts | length tsList == 2 =
    -- TODO:
    Just Slot {start = st, end = et}
    where tsList@(st : et : _) = splitOn "/" $ stripChars " " ts

parseSlot _ = Nothing

-- parseSlots [] = []
-- parseSlots (slot:slots) =
--         [Slot {start = st, end = et, scores = 0}] ++ (parseSlots slots)
--         where [st, et] = splitOn "/" slot


-- doodleNameTest doodleName doodle = doodleName == name doodle
--
-- printDoodle h (doodle:[]) = do
--                                let (start, end) = slot doodle
--                                hPutStrLn h $ "\t" ++ start ++ " / " ++ end
--                                hPutStrLn h "]"
--
-- printDoodle h (doodle:doodles) = do
--                                         let (start, end) = slot doodle
--                                         hPutStrLn h "ok ["
--                                         hPutStrLn h $ "\t" ++ start ++ " / " ++ end ++ ","
--                                         printDoodle h doodles

-- slotExistenceTest [start, end] mySlot = (start, end) == slot mySlot

-- updatePreference h user transDoodleMap name [start, end] = do  let subscribeList = subscribe user
--                                                                 if all (subscriptionListTest name) subscribeList
--                                                                 then do hPutStrLn h "Not subscribed"
--                                                                         return $ subscribe user
--                                                                 else do
--                                                                         let (oldName, (oldStart, oldEnd)) = fromJust $ find (subscriptionListNegateTest name) subscribeList
--                                                                         doodles <- atomically $ readTVar transDoodleMap
--                                                                         let [theDoodle] = filter (doodleNameTest name) doodles
--                                                                         addedSlots <- addScores theDoodle [start, end]
--                                                                         newSlots <- minusScores (Doodle {name = name, slots = addedSlots}) [oldStart, oldEnd]
--                                                                         let restDoodles = delete theDoodle doodles
--                                                                         atomically $ writeTVar transDoodleMap $ restDoodles ++ [Doodle {name = name, slots = newSlots}]
--                                                                         return $ (filter (subscriptionListTest name) subscribeList) ++ [(name, (start, end))]

-- subscriptionListTest name (doodleName, slot) = name /= doodleName
-- subscriptionListNegateTest name (doodleName, slot) = name == doodleName
--
-- addScores theDoodle [start, end] = do
--                                     let [theSlot] = filter (slotExistenceTest [start, end]) $ slots theDoodle
--                                     let theRestSlots = delete theSlot $ slots theDoodle
--                                     let newScore = 1 + scores theSlot
--                                     return $ theRestSlots ++ [ Slot { slot = (start, end), scores = newScore }]
--
-- minusScores theDoodle [start, end] = do
--                                     let [theSlot] = filter (slotExistenceTest [start, end]) $ slots theDoodle
--                                     let theRestSlots = delete theSlot $ slots theDoodle
--                                     let newScore = scores theSlot - 1
--                                     return $ theRestSlots ++ [ Slot { slot = (start, end), scores = newScore }]
--
-- printSchedules h (doodle:doodles)
--     | length doodles /= 0 = do  printSchedule h doodle
--                                 printSchedules h doodles
--     | otherwise           = do  printSchedule h doodle
--
-- printSchedule h doodle = do    let (start, end) = slot $ maximumBy (compareSlotPreference) (slots doodle)
--                                     hPutStrLn h $ "{"++name doodle++" : "++start++"/"++end++"}"
--
-- compareSlotPreference lSlot rSlot = compare (scores lSlot) (scores rSlot)
