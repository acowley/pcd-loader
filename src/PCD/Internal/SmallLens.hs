-- |Re-export the lens package without the Zipper module whose names
-- tend to clash.
module PCD.Internal.SmallLens ( module Control.Lens.Type
                              , module Control.Lens.Traversal
                              , module Control.Lens.Getter
                              , module Control.Lens.Setter
                              , module Control.Lens.Action
                              , module Control.Lens.Combinators
                              , module Control.Lens.Fold
                              , module Control.Lens.Iso
                              , module Control.Lens.Indexed
                              , module Control.Lens.IndexedFold
                              , module Control.Lens.IndexedGetter
                              , module Control.Lens.IndexedLens
                              , module Control.Lens.IndexedTraversal
                              , module Control.Lens.IndexedSetter
                              , module Control.Lens.Plated
                              , module Control.Lens.Projection
                              , module Control.Lens.Representable
                              , module Control.Lens.TH
                              , module Control.Lens.Tuple
                              , module Control.Lens.WithIndex
                              , module Control.Lens.Zoom ) where
import Control.Lens.Type
import Control.Lens.Traversal
import Control.Lens.Getter
import Control.Lens.Setter
import Control.Lens.Action
import Control.Lens.Combinators
import Control.Lens.Fold
import Control.Lens.Iso
import Control.Lens.Indexed
import Control.Lens.IndexedFold
import Control.Lens.IndexedGetter
import Control.Lens.IndexedLens
import Control.Lens.IndexedTraversal
import Control.Lens.IndexedSetter
import Control.Lens.Plated
import Control.Lens.Projection
import Control.Lens.Representable
import Control.Lens.TH
import Control.Lens.Tuple
import Control.Lens.WithIndex
import Control.Lens.Zoom

