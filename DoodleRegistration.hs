import Network.Socket
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Fix (fix)
-- easier access to lists
-- import Control.Lens
import System.Environment
import Data.Maybe
import Data.String.Utils
import Data.List
import Data.List.Split
import System.IO.Unsafe
import System.Random

data UserType = Teacher | Student | Administrator deriving (Show, Eq)

data User = User { uid :: String, pw :: String, category :: UserType, subscribe :: [(String, (String, String))] } deriving (Show, Eq)

data Slot = Slot { slot :: (String, String), scores :: Int } deriving (Show, Eq)
data Doodle = Doodle { name :: String, slots :: [Slot] } deriving (Show, Eq)

-- data Request = Request { action :: String, login :: String, doodle :: Doodle, slot ::  } deriving (Show, Eq)

stripChars :: String -> String -> String
stripChars = filter . flip notElem

main :: IO ()
main = withSocketsDo $ do
    -- TODO
    [login, token] <- getArgs
    userList <- newTVarIO ([User {uid = login, pw = token, category = Administrator, subscribe = []}])
    doodleList <- newTVarIO ([])
    channel <- newChan
    -- AF_INET means support Internetwork such as UDP TCP
    -- Stream means it is a Stream sockets which use TCP and SCTP
    mySocket <- socket AF_INET Stream defaultProtocol
    bind mySocket (SockAddrInet 5002 0)
    listen mySocket 5
    loop mySocket channel userList doodleList 0

--loop :: Socket -> Chan String -> TVar [User] -> Int -> IO ()
loop socket channel userList doodleList number = do
    (sock, addr) <- Network.Socket.accept socket
    h <- socketToHandle sock ReadWriteMode
    forkIO $ handleConnection h channel userList doodleList number
    loop socket channel userList doodleList $ number + 1

--handleConnection :: (Socket, SockAddr) -> Chan String -> Int -> IO ()
handleConnection h channel userList doodleList number = do
    hSetBuffering h LineBuffering
    -- loginInfo <- hGetLine h
    users <- atomically $ readTVar userList
    -- let user = getUser loginInfo users
    -- if getUser loginInfo users
    -- then do
    -- hPutStrLn h "Login succeeded!"
    handleRequest h userList doodleList
    -- else do
            -- hPutStrLn h "Login Failed."
            -- handleConnection h channel userList doodleList number
    hClose h

-- Code block to h login
getUser loginInfo users =
    do
        let [myLogin, myToken] = Data.String.Utils.split "@" loginInfo
        find (userLoginTest [myLogin, myToken]) users

userLoginTest [myLogin, myToken] user =
    if (uid user) == myLogin && (pw user) == myToken
    then True
    else False

-- hGetLines :: Handle -> IO [T.Text]
hGetLines h = do
  line <- hGetLine h
  if null line
    then do
        val <- readDoodle' h []
        return [val]
    else do
      lines <- hGetLines h
      return (line:lines)

-- Just right
hGetLines' :: Handle -> IO [String]
hGetLines' h = unsafeInterleaveIO $ do
    readable <- hIsReadable h
    if readable
        then do
            x  <- hGetLine h
            xs <- hGetLines' h
            return (x:xs)
        else return []

handleRequest h userList doodleList =
    do
        -- args <- readDoodle handle
        tokens <- tokenizeRule h
        hPutStrLn h $ "tokens\n" ++ ( show tokens )
        response tokens h userList doodleList
        -- -- test
        -- users <- atomically $ readTVar userList
        -- hPutStrLn h $ "user list" ++ ( show users )
        -- -- let updateUser = fromJust $ find (userLoginTest [login user, token user]) users
        -- doodles <- atomically $ readTVar doodleList
        -- hPutStrLn h $ "show doodles"
        -- printDoodle handl doodles
        -- -- testing block ends
        handleRequest h userList doodleList


response tokens@(_: login: _) h userList doodleList =
    do
        users <- atomically $ readTVar userList
        let user = getUser login users
        if user /= Nothing
            then respAction tokens h user userList users doodleList
        else hPutStrLn h "wrong login"

respAction [action, _, identifier] h user userList users _
    | action == "add-teacher" || action == "add-student" =
        let isAdmin    = category (fromJust user) == Administrator
            userExists = userExist identifier users
            usertype = if action == "add-teacher" then Teacher else Student
            -- TODO
            randomStr = take 4 $ randomRs ('a','z') $ unsafePerformIO newStdGen
        in
        if isAdmin
            then if not userExists
                    then do
                        atomically $ writeTVar userList ([ User {
                                                         uid = identifier,
                                                         pw = randomStr,
                                                         category = usertype,
                                                         subscribe = [] }]
                                                         ++ users)
                        newUsers <- atomically $ readTVar userList
                        hPutStrLn h $ show newUsers
                else hPutStrLn h $ "id taken"
        else hPutStrLn h $ "you are not Admin"

respAction ["set-doodle", login, identifier, doodleStr] h user userList users doodleList
    | length doodleParams == 2 = do hPutStrLn h (show doodleParams)

    where doodleParams = Data.String.Utils.split "," $ stripChars "[] " doodleStr
            -- if length doodleParams == 2 then return doodleStr
            -- else error "doodle has too many parameters!"
-- error state
respAction tokens h _ _ _ _=
    do
        hPutStrLn h "entered: error_state"

-- respAction Administrator "add-teacher" h =
--     hPutStrLn h "Login Success. You can do add-teacher, add-student or change-password command."

userExist login users = any (\u -> uid u == login) users

-- parseSlotStr h =
--     do
--         cont <- hGetContents h
--         return cont
        -- rest <- hGetLine h
        -- if (endswith "]" $ rstrip rest)
        -- then return rest
        -- else liftM (rest ++) (parseSlotStr h)

-- readArgs h = do
--     line <- hGetLine h
--     if startswith "set-doodle" $ rstrip line
--

-- getLines :: h -> IO ()
-- getLines h = getLinesAcc h ""
--
-- getLinesAcc h tmp =
--     do ineof <- hIsEOF h
--        if ineof
--            then return (tmp)
--            else do inpStr <- hGetLine h
--                    hPutStrLn h (inpStr)
--                    getLinesAcc h ( tmp ++ inpStr )

tokenizeRule :: Handle -> IO [String]
tokenizeRule h =
    do
        line <- hGetLine h
        let args = splitWs line
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


_tokenizeRule h (["set-doodle", login, token]) =
    do
        line <- hGetLine h
        doodleStr <- readDoodle h line
        return (["set-doodle", login, token, doodleStr])


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

_tokenizeRule h (["set-doodle", login, token, str])
    -- TODO: regex
    | startswith "[" str && endswith "]" str =
            return ([login, token, str])
    | startswith "[" str =
            do
               doodleStr <- readDoodle' h str
               return (["set-doodle", login, token, doodleStr])
    | otherwise = return ["error"]

_tokenizeRule h (action : oldArgs)
    | length oldArgs < 2 =
        do
            inpStr <- hGetLine h
            let args = splitWs inpStr
            _tokenizeRule h ((action : oldArgs) ++ args)

readDoodle :: Handle -> [Char] -> IO [Char]
readDoodle h line
    | startswith "[" line && endswith "]" stLine =
        return line
    | startswith "[" line =
        do
            readDoodle' h (line)
    | otherwise = error "doodle does not begin with ["
    where stLine = strip line

readDoodle' :: Handle -> [Char] -> IO [Char]
readDoodle' h acc =
    do
        line <- hGetLine h
        if (endswith "]" line)
            then return (acc ++ line)
            else (readDoodle' h (acc ++ line))


produceSlots [] = []

produceSlots (slot:slots) = [Slot {slot = (start, end), scores = 0}] ++ (produceSlots slots)
                        where
                            [start, end] = splitOn "/" slot

-- doodleNameTest doodleName doodle = doodleName == name doodle

printDoodle h (doodle:[]) = do
                               let (start, end) = slot doodle
                               hPutStrLn h $ "\t" ++ start ++ " / " ++ end
                               hPutStrLn h "]"

printDoodle h (doodle:doodles) = do
                                        let (start, end) = slot doodle
                                        hPutStrLn h "ok ["
                                        hPutStrLn h $ "\t" ++ start ++ " / " ++ end ++ ","
                                        printDoodle h doodles

slotExistenceTest [start, end] mySlot = (start, end) == slot mySlot

-- updatePreference h user doodleList name [start, end] = do  let subscribeList = subscribe user
--                                                                 if all (subscriptionListTest name) subscribeList
--                                                                 then do hPutStrLn h "Not subscribed"
--                                                                         return $ subscribe user
--                                                                 else do
--                                                                         let (oldName, (oldStart, oldEnd)) = fromJust $ find (subscriptionListNegateTest name) subscribeList
--                                                                         doodles <- atomically $ readTVar doodleList
--                                                                         let [theDoodle] = filter (doodleNameTest name) doodles
--                                                                         addedSlots <- addScores theDoodle [start, end]
--                                                                         newSlots <- minusScores (Doodle {name = name, slots = addedSlots}) [oldStart, oldEnd]
--                                                                         let restDoodles = delete theDoodle doodles
--                                                                         atomically $ writeTVar doodleList $ restDoodles ++ [Doodle {name = name, slots = newSlots}]
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
