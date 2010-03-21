{-# LANGUAGE GeneralizedNewtypeDeriving,
             RecursiveDo,
             TemplateHaskell,
             FlexibleContexts
             #-} 

module Data.HalfEdge (
    HEVert(..),
    HEEdge(..),
    HEFace(..),
    HEMesh(..),
    mkHEMeshFromMesh
) where

import Prelude hiding (sequence, mapM, concat, concatMap)

import qualified Data.EnumMap as M
import qualified Data.Map as MOrd
import Data.Maybe
import Data.List hiding (concat, concatMap)
import Data.Vector hiding (fst, snd)
import Data.Lenses.Template

import qualified Data.IndexedCollection as IC
import Control.Monad.Error hiding (sequence, mapM)
import Control.Monad.State.Lazy hiding (sequence, mapM)
import Data.Foldable
import Data.Traversable

import Data.HalfEdge.MeshLoad

-- |Half edge vertex
data HEVert a p = HEVert {
    v_v :: Vector3 a,
    v_e :: IC.Key (HEEdge a p) p
} deriving (Show)

-- |Half edge edge
data HEEdge a p = HEEdge {
    e_v :: IC.Key (HEVert a p) p, -- Vertex
    e_p :: IC.Key (HEEdge a p) p, -- Paired or opposite edge
    e_f :: IC.Key (HEFace a p) p, -- Face
    e_n :: IC.Key (HEEdge a p) p  -- Next edge
} deriving (Show)

-- |Half edge face
data HEFace a p = HEFace {
    f_e :: IC.Key (HEEdge a p) p,
    f_n :: Vector3 a
} deriving (Show)

data HEMesh a p = HEMesh {
    meshEdges_ :: IC.IndexedCollection (HEEdge a p) p,
    meshVerts_ :: IC.IndexedCollection (HEVert a p) p,
    meshFaces_ :: IC.IndexedCollection (HEFace a p) p
} deriving (Show)

$( deriveLenses ''HEMesh )

data HEMeshConvertErrors = HEMeshConvertErrors [String]
  deriving (Show)

instance Error HEMeshConvertErrors where
  noMsg = HEMeshConvertErrors []
  strMsg s = HEMeshConvertErrors [s]


type VertKey a p = IC.Key (HEVert a p) p
type EdgeKey a p = IC.Key (HEEdge a p) p
type FaceKey a p = IC.Key (HEFace a p) p
type HeMeshConvertT a p b  = StateT (HEMesh a p) (StateT [Maybe String] (Either HEMeshConvertErrors)) b


mkHEMeshFromMesh :: (Num a, Ord a, Ord k, Enum k, Show k) =>  Mesh k a -> Either HEMeshConvertErrors (HEMesh a p)
mkHEMeshFromMesh mesh = flip evalStateT [] $ flip execStateT (HEMesh IC.empty IC.empty IC.empty) $ mdo
    newVertIndicies <- mapM (addVertex (getVertEdgeWith edgeKeysAndPairs)) $ verticies mesh
    edgeKeysAndPairs <- mapM (addFace (getVertKeyWith newVertIndicies) (getEdgePairWith edgeKeysAndPairs) getFaceNormal) $ faces mesh
    runChecks
    where
        getFaceNormal oldFaceIndex = checkNow ("Invalid face index " ++ show oldFaceIndex) $ M.lookup oldFaceIndex $ M.fromList $ faceNormals mesh


addVertex :: (VertKey a p -> HeMeshConvertT a p (EdgeKey a p)) -> (t, Vector3 a) -> HeMeshConvertT a p (t, VertKey a p)
addVertex getVertEdge (oldIndex, v) = mdo 
    vertexKey <- meshVerts $ IC.add $ HEVert { v_v = v, v_e = edgeKey }
    edgeKey <- getVertEdge vertexKey
    return (oldIndex, vertexKey)

addFace :: (Ord a, Num a)
        => (b -> HeMeshConvertT a p (VertKey a p))
        -> ((VertKey a p, VertKey a p) -> HeMeshConvertT a p (EdgeKey a p))
        -> (t -> HeMeshConvertT a p (Vector3 a))
        -> (t, Vector3 b) -> HeMeshConvertT a p (EdgeKey a p, [((VertKey a p, VertKey a p), EdgeKey a p)])
addFace getVertKey getEdgePair getFaceNormal (oldIndex, f) = mdo
    Vector3 a b c <- mapM getVertKey f
    Vector3 aVrt bVrt cVrt <- mapM (liftM v_v . getVert) (Vector3 a b c)
    faceNormal <- getFaceNormal oldIndex
    face <- meshFaces $ IC.add $ HEFace { f_e = edge, f_n = faceNormal }
    edges@(edge, _) <- case 0 < ((bVrt - aVrt) `cross3` (cVrt - aVrt) `dot` faceNormal) of
        True  -> addEdges getEdgePair face a b c
        False -> addEdges getEdgePair face c b a
    return edges

addEdges :: ((VertKey a p, VertKey a p)-> HeMeshConvertT a p (EdgeKey a p)) 
         -> FaceKey a p -> VertKey a p -> VertKey a p -> VertKey a p
         -> HeMeshConvertT a p (EdgeKey a p, [((VertKey a p, VertKey a p), EdgeKey a p)])
addEdges getEdgePair face a b c = mdo
    p1 <- getEdgePair (b, a)
    edgeKeyA <- meshEdges $ IC.add $ HEEdge { e_v = a, e_p = p1, e_f = face, e_n = edgeKeyB }
    p2 <- getEdgePair (b, a)
    edgeKeyB <- meshEdges $ IC.add $ HEEdge { e_v = b, e_p = p2, e_f = face, e_n = edgeKeyC }
    p3 <- getEdgePair (b, a)
    edgeKeyC <- meshEdges $ IC.add $ HEEdge { e_v = c, e_p = p3, e_f = face, e_n = edgeKeyA }
    return (edgeKeyA, [((a, b), edgeKeyA), ((b, c), edgeKeyB), ((c, a), edgeKeyC)])

getVert :: VertKey a p -> HeMeshConvertT a p (HEVert a p)
getVert newIndex = checkNow ("Invalid new vertex index " ++ show newIndex) =<< meshVerts (IC.lookup newIndex)

getEdgePairWith :: (Show a, Ord a, Foldable t) =>  t (b, [(a, c)]) -> a -> HeMeshConvertT d p c
getEdgePairWith eps vertKeyPair = fcLookup checkLater "Invalid vertKeyPair on edge pair lookup" vertKeyPair $ concatMap snd eps

getVertEdgeWith :: (Show a, Ord a, Foldable t) =>  t (d, [((a, b), c)]) -> a -> HeMeshConvertT e p c
getVertEdgeWith eps vertexKey = fcLookup checkLater "Invalid vertexKey on edge lookup" vertexKey $ fmap (mapFst fst) $ concatMap snd eps

getVertKeyWith :: (Show a, Ord a) => [(a, c)] -> a -> HeMeshConvertT b p c
getVertKeyWith newVertKeys oldIndex = fcLookup checkNow "Invalid old vertex index" oldIndex newVertKeys

fcLookup :: (Show a, Ord a) => (String -> Maybe c -> b) -> String -> a -> [(a, c)] -> b
fcLookup checker e k t = checker (e ++ " " ++ show k) $ fastLookup k t
fastLookup :: (Ord k) => k -> [(k, a)] -> Maybe a
fastLookup k t = MOrd.lookup k $ MOrd.fromList $ t

checkLater :: String -> Maybe b -> HeMeshConvertT a p b
checkLater e x = do 
  lift $ consM $ flipMaybe e x
  return $ fromJust x

checkNow :: String -> Maybe b -> HeMeshConvertT a p b
checkNow e Nothing = lift $ lift $ throwError $ HEMeshConvertErrors [e]
checkNow _ (Just x) = return x

runChecks :: HeMeshConvertT a p ()
runChecks = do
  errors <- liftM catMaybes $ lift get
  when (not $ null errors) $ lift $ lift $ throwError $ HEMeshConvertErrors errors

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

consM :: (MonadState [a] m) => a -> m ()
consM x = get >>= put . (x:)

flipMaybe :: a -> Maybe t -> Maybe a
flipMaybe e Nothing = Just e
flipMaybe _ (Just _) = Nothing

{-
testHalfEdge :: IO ()
testHalfEdge = do
    thing <- parseASE "../Data/LittleShip.ase"
    case thing of
      Nothing -> print "error parsing model"
      Just [] -> print "error parsing model"
      Just (mesh:_) ->  case mkHEMeshFromMesh mesh of
          Left err -> putStr $ "Errors: " ++ concat (intersperse "\n" err) ++ "\n"
          Right heMesh -> print $ heMesh
-}

