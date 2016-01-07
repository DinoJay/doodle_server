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
import Data.Ord
import System.Random
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Set (Set)
import Data.List
import qualified Data.Set as Set

data UserType = Teacher | Student | Administrator deriving (Show, Eq)

data User = User {uid :: String, pw :: String, category :: UserType, subs :: Set String  } deriving (Show, Eq)

data Slot = Slot {start :: String, end :: String}


instance Show Slot where
    show s = show "(" ++ start s ++ ", " ++ end s ++")"

instance Ord Slot where
    (<=) (a) (b) = start a <= start b && end a <= end b

instance Eq Slot where
    (==) (a) (b) = start a == start b && end a == end b


data Doodle = Doodle {name :: String, timeslots :: Map Slot (Set String)} deriving (Show)

instance Eq Doodle where
    (==) (a) (b) = name a == name b

-- instance Show [Doodle] where
-- myshow:: [[Slot]] -> String
-- myshow ds = concatMap ds (\d -> show d)

-- data Request = Request { action :: String, login :: String, doodle :: Doodle, slot ::  } deriving (Show, Eq)

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
        tokens <- tokenizeRule h
        if length tokens == 0
            then handleRequest h transUserMap transDoodleMap
        else do
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

respAction ["subscribe", login, doodleId] h user transUserMap userMap transDoodleMap doodleMap
    | maybeDoodle == Nothing = hPutStrLn h "doodle does not exist"
    | subscribed = hPutStrLn h "user is already subscribed"
    | maybeDoodle /= Nothing =
        let newUserMap = Map.insert (uid user) User {
                                                uid      = uid user,
                                                pw       = "aaaa",
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

respAction ["prefer", login, doodleId, slotString] h user transUserMap userMap transDoodleMap doodleMap
    | maybeDoodle == Nothing = hPutStrLn h "no-such-id"
    | not subscribed  = hPutStrLn h "not-subscribed"
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
          oldPrefs      = if maybeOldPrefs /= Nothing
                              then fromJust maybeOldPrefs
                          else Set.empty
          newPrefs      = Set.insert (uid user) oldPrefs
          newTimeslots  = Map.insert ts newPrefs (timeslots doodle)

respAction ["exam-schedule", _] h user transUserMap userMap transDoodleMap doodleMap =
        -- let sch = calcSchedule userMap doodleMap
        -- in hPutStrLn h (concatMap sch (\d -> show $ head d))
        let sch = sequence [[(name d, ts, getVotes ts d)
                               | ts <- (Map.keys $ timeslots d)]
                               | d <- (Map.elems doodleMap)]
            sumSeq = [(Map.fromList $ map (\(did, t, _) -> (did, t)) s, sumSnd s)| s <- sch]
            sortSeq = reverse $ sortBy (compare `on` snd) sumSeq
            times = filterTimes sortSeq userMap
        in do
              hPutStrLn h $ "COMBS" ++ show times
              -- hPutStrLn h $ "length" ++ show (length sumSeq)

-- noTimeClash :: Ord a => Map k (Ts a) -> (a, a) -> Bool
-- noTimeClash xs (t2, t3) = List.null $ Map.elems $ Map.filter (\y -> ( start y >= t2 && start y <= t3 || (end y >= t2 && end y <= t3) )) xs

filterTimes sch userMap =
     find (\(_, _, b)-> b==True) [(s, p, all (==False) [ allOverlap $ map fromJust $ filter (/=Nothing) userSlotList |
                              userSlotList <- [[Map.lookup did s |
                                          did <- Set.elems $ subs user]|
                                          user <- Map.elems userMap
                                          ]
                               ]
                                ) | (s, p) <- sch
                               ]

--
sumSnd = foldr (\(_, _, c) acc -> c + acc) 0
-- calcSchedule userMap doodleMap =

-- hasNoDups:: [Slot] -> Bool
-- hasNoDups l = ( length $ nubBy allOverlap l ) == length l

overlap (i,a) (j, b) = helper a b && i /= j
    where helper x y = start x >= start y && start x <= end y || end x >= start y && end x <= end y

allOverlap:: [Slot] -> Bool
allOverlap xs =
        any (==True) $ concat [[overlap x y |x <- zipped]| y <- zipped]
        where zipped = zip [0 ..] xs

-- -- error state
-- respAction _ h _ _ _ _ _=
--     do
--         hPutStrLn h "entered: error_state"

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
               return []

_tokenizeRule h (action : oldArgs)
    | length oldArgs < 2 =
        do
            inpStr <- hGetLine h
            let args = splitWs inpStr
            _tokenizeRule h ((action : oldArgs) ++ args)


_tokenizeRule _ _ =
    -- hPutStrLn h "error state X"
    return []


readDoodle :: Handle -> [Char] -> IO [Char]
readDoodle h line
    | startswith "[" line && endswith "]" stLine =
        return (stripChars "\t" line)
    | startswith "[" line =
        do
            readDoodle' h (stripChars "\t" line)
    | otherwise = error "doodle does not begin with ["
    where stLine = strip line

readDoodle' :: Handle -> [Char] -> IO [Char]
readDoodle' h acc =
    do
        line <- hGetLine h
        if (endswith "]" line)
            then return (stripChars "\t" $ acc ++ line)
            else (readDoodle' h (acc ++ line))

-- -- parseSlots :: [[Char]] -> [Slot]

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

parseSlot ts
    | length tsList == 2 =
    -- TODO:
    Just Slot {start = st, end = et}
    where tsList@(st : et : _) = splitOn "/" $ stripChars " " ts

parseSlot _ = Nothing

