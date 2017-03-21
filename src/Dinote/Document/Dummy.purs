module Dinote.Document.Dummy
  ( runDocumentF
  ) where

import Data.Map as Map
import Dinote.Document (Document(..), DocumentID(..))
import Dinote.Document.Algebra (DocumentF(..))
import Dinote.Prelude
import Dinote.Vertex (Vertex(..), VertexID(..))

runDocumentF :: ∀ m. (Applicative m) => DocumentF ~> m
runDocumentF (GetDocuments next) = pure <<< next $
  Map.empty
  # Map.insert (DocumentID "1") (Document "Lorem" unit)
  # Map.insert (DocumentID "2") (Document "Ipsum" unit)
runDocumentF (GetDocument id next) = pure <<< next $ case id of
  DocumentID "1" -> Just $ Document "Lorem" loremBody
  DocumentID "2" -> Just $ Document "Ipsum" ipsumBody
  _ -> Nothing
  where
  loremBody :: Map VertexID Vertex
  loremBody =
    Map.empty
    # Map.insert (VertexID "1") (Vertex (Right "Hello") (VertexID "2" : Nil))
    # Map.insert (VertexID "2") (Vertex (Right "World") Nil)

  ipsumBody :: Map VertexID Vertex
  ipsumBody = Map.singleton (VertexID "1") (Vertex (Right "Hello, world!") Nil)

