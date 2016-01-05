module Doodle where 
import qualified System.IO

prompt :: Read a => String -> IO b -> (a -> IO b) -> IO b
prompt s m f = do putStrLn s
                  xs <- fmap (readsPrec 0) System.IO.getLine
                  if null xs
                     then System.IO.putStrLn "What???" >> m
                     else f $ fst $ head xs

class Doodle d where
  initialize :: String -> d t
  add :: Ord t => (t,t) -> d t -> d t
  remove :: Int -> d t -> d t
  toogle :: String -> Int -> d t -> d t

class Pool p where
  freshKey :: (Ord k, Enum k) => p k (d t) -> k
  get :: Ord k => k -> p k (d t) -> Maybe (d t)
  set :: Ord k => k -> (d t) -> p k (d t) -> p k (d t)


run :: (Read t, Doodle d, Show k, Ord k, Enum k, Read k, Pool p, Show (d t), Ord t) => p k (d t) -> IO ()
run p = prompt "Create a new doodle or participate to an existing one?" (return p) (turn p) >>= run 

turn :: (Read t, Doodle d, Ord k, Show k, Enum k, Read k, Pool p, Show (d t), Ord t) => p k (d t) -> Either String k -> IO (p k (d t))
turn p (Left s)  = do d <- (populate $ initialize s)
                      let k = freshKey p
                      putStrLn $ "Doodle ID: " ++ show k
                      return $ set k d p
turn p (Right k) = maybe (System.IO.putStrLn "Unknown doodle" >> return p)
                         (\d1 -> prompt "What is your name?"
                                        (turn p (Right k))
                                        (\s -> fmap (\d -> (set k d p)) (participate s d1)))
                         (get k p)

populate :: (Read t, Doodle d, Show (d t), Ord t) => d t -> IO (d t)
populate d = putStrLn (show d) >> prompt "Add/Remove a slot?" (populate d) f
  where f Nothing                = return d
        f (Just (Left i))        = populate $ remove i d
        f (Just (Right (t1,t2))) = populate $ add (t1,t2) d

participate :: (Doodle d, Show (d t)) => String -> d t -> IO (d t)
participate n d = putStrLn (show d) >> prompt "Toogle a slot?" (participate n d) f
  where f Nothing  = putStrLn "Thanks for participating!" >> return d
        f (Just i) = participate n (toogle n i d) 