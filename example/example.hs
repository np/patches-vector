{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Patch (Patch, apply, transformWith, diff, toList, fromList)
import qualified Data.Vector as V
import Data.Vector (Vector)
import Control.Concurrent
import Control.Concurrent.MVar
import System.IO
import Test.QuickCheck

newtype Doc = Doc (Vector Char) deriving (Eq, Ord, Show)

type DocPatch = Patch Char

type Version = Int

data Versioned a = Versioned
  { _v_version :: Version
  , _v_data    :: a
  }

data ClientData = ClientData
  { _c_name :: Text
  , _c_text :: Vector Char
  }

data API a = API
  { _get :: IO (Versioned (Vector a))
  , _put :: Versioned (Patch a) -> IO (Versioned (Patch a))
  }

data Repo a = Repo
  { _r_version :: Version
  , _r_vector  :: Vector a
  , _r_history :: [Patch a]
    -- ^ first patch in the list is the most recent
  }

type ConflictResolution a = a -> a -> a

-- Left biased
conflict :: ConflictResolution a
conflict x _ = x

serve :: Vector Char -> IO (API Char)
serve init = do
  v_repo <- newMVar (Repo 0 init [])
  pure $ API
    { _get = (\r -> Versioned (_r_version r) (_r_vector r)) <$> readMVar v_repo
    , _put = \(Versioned v p) -> modifyMVar v_repo $ \r -> do
       let
         q = mconcat $ take (_r_version r - v) (_r_history r)
         (p', q') = transformWith conflict p q
         r' = if p' == mempty then r
              else Repo
                { _r_version = 1  + _r_version r
                , _r_vector  = apply p' $ _r_vector r
                , _r_history = p' : _r_history r
                }
       pure (r', Versioned (_r_version r') q')
    }

arbClient :: MVar () -> API Char -> ClientData -> IO ()
arbClient logvar api (ClientData name goal) = do
  Versioned v s <- _get api
  go v s
  where
    log :: Show d => Text -> d -> IO ()
    log msg d =
      modifyMVar_ logvar $ \()-> do
        T.putStrLn $ T.unwords [name, msg, T.pack $ show d]
        hFlush stdout
    go v s0 = do
      delay <- generate $ elements [1..3]
      log "delay" delay
      threadDelay (1000000 * delay)
      log "version/old" v
      log "termlist/old" s0
      let d = diff s0 goal
      let es = toList d
      log "diff/old/goal" d
      patch <-
        if null es then do
          log "No changes needed." ()
          pure mempty
        else
          fromList . pure <$> generate (elements es)
      log "patch/cur" patch
      let s1 = apply patch s0
      log "termlist/cur" s1
      Versioned v' patch' <- _put api (Versioned v patch)
      log "version/new" v'
      log "patch/new" patch'
      let s2 = apply patch' s1
      log "termlist/new" s2
      go v' s2

main :: IO ()
main = do
  logvar <- newMVar ()
  api <- serve $ V.fromList "Here is the starting text!"
  let goal = V.fromList "Hello World!"
  void . forkIO . arbClient logvar api $ ClientData "Alice" goal
  arbClient logvar api $ ClientData "Bob" goal
