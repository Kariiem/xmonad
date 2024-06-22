-- |

module XMonad.ExtensionUtils where

import Data.Typeable
import Data.Maybe
import qualified Data.Map as M
import qualified Control.Monad.State as State

import XMonad.Core

type ExtStateRep = M.Map String (Either String StateExtension)


-- | Modify the map of state extensions by applying the given function.
modifyStateExts :: (ExtStateRep -> ExtStateRep) -> X ()
modifyStateExts f = State.modify $ \st -> st { extensibleState = f (extensibleState st) }

-- | Apply a function to a stored value of the matching type or the initial value if there
-- is none.
modify :: (ExtensionClass a) => (a -> a) -> X ()
modify = modifyM . (pure .)

-- | Apply an action to a stored value of the matching type or the initial value if there
-- is none.
modifyM :: (ExtensionClass a) => (a -> X a) -> X ()
modifyM f = put =<< f =<< get

-- | Like 'modify' but the result value is forced to WHNF before being stored.
modify' :: (ExtensionClass a) => (a -> a) -> X ()
modify' = modifyM' . (pure .)

-- | Like 'modifyM' but the result value is forced to WHNF before being stored.
modifyM' :: (ExtensionClass a) => (a -> X a) -> X ()
modifyM' f = (put $!) =<< f =<< get

-- | Add a value to the extensible state field. A previously stored value with the same
-- type will be overwritten. (More precisely: A value whose string representation of its type
-- is equal to the new one's)
put :: (ExtensionClass a) => a -> X ()
put v = modifyStateExts . M.insert (show . typeOf $ v) . Right . extensionType $ v

-- | Try to retrieve a value of the requested type, return an initial value if there is no such value.
get :: (ExtensionClass a) => X a
get = getState' undefined -- `trick' to avoid needing -XScopedTypeVariables
  where
    toValue val = fromMaybe initialValue $ cast val
    getState' :: (ExtensionClass a) => a -> X a
    getState' k = do
      v <- State.gets $ M.lookup (show . typeOf $ k) . extensibleState
      case v of
        Just (Right (StateExtension val)) -> return $ toValue val
        Just (Right (PersistentExtension val)) -> return $ toValue val
        Just (Left str) | PersistentExtension x <- extensionType k -> do
                            let val = fromMaybe initialValue
                                    $ cast =<< safeRead str `asTypeOf` Just x
                            put (val `asTypeOf` k)
                            return val
        _ -> return initialValue
    safeRead str = case reads str of
                         [(x,"")] -> Just x
                         _ -> Nothing
