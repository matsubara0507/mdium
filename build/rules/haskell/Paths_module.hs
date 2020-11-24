module %{module_name} where

import Prelude
import Data.Version (Version (..))

version :: Version
version = Version [%{version}] []
