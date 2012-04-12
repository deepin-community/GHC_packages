

-- UUAGC 0.9.40.3 (src-ag/Expression.ag)
module Expression where
{-# LINE 2 "src-ag/Expression.ag" #-}

import UU.Scanner.Position(Pos)
import HsToken
{-# LINE 10 "dist-ghc/build/Expression.hs" #-}
-- Expression --------------------------------------------------
{-
   alternatives:
      alternative Expression:
         child pos            : {Pos}
         child tks            : {[HsToken]}
-}
data Expression = Expression (Pos) (([HsToken]))