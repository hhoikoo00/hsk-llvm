module Compile (compile) where

import qualified MegaparsecDemo.Demo as Demo (run)
import qualified MegaparsecDemo.URIDemo as URIDemo (run)
import qualified MegaparsecDemo.ExprDemo as ExprDemo (run)

compile :: IO ()
compile = ExprDemo.run
