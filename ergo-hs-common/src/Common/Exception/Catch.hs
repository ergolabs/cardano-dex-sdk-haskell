module Common.Exception.Catch where

import RIO
import qualified Control.Exception as CE

catch :: (Exception e, MonadUnliftIO f) => f () -> (e -> f ()) -> f ()
catch func handler = withRunInIO $ \run -> CE.catch (run func) (run . handler)