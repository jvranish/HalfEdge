
module Data.HalfEdge.Convert where

import Control.Monad.State hiding (mapM, mapM_)

import qualified Data.Map as Map
import Data.Foldable
import Data.Traversable
import Data.List hiding (elem)

import Data.Vector

import qualified Data.IndexedCollection as IC

import Prelude hiding (mapM, mapM_, elem, fst, snd)

-- |Half edge vertex
data HEVert vert face s = HEVert {
    v_v :: vert,
    v_e :: IC.Key (HEEdge vert face s) s
} deriving (Show)

-- |Half edge edge
data HEEdge vert face s = HEEdge {
    e_v :: IC.Key (HEVert vert face s) s, -- Vertex
    e_p :: IC.Key (HEEdge vert face s) s, -- Paired or opposite edge
    e_f :: IC.Key (HEFace vert face s) s, -- Face
    e_n :: IC.Key (HEEdge vert face s) s  -- Next edge
} deriving (Show)

-- |Half edge face
data HEFace vert face p = HEFace {
    f_e :: IC.Key (HEEdge vert face p) p,
    f_d :: face
} deriving (Show)

data HEMesh vert face s = HEMesh {
    meshEdges_ :: IC.IndexedCollection (HEEdge vert face s) s,
    meshVerts_ :: IC.IndexedCollection (HEVert vert face s) s,
    meshFaces_ :: IC.IndexedCollection (HEFace vert face s) s
} deriving (Show)

data Mesh vertIdx faceIdx vert face = Mesh {
    verticies :: Map.Map vertIdx vert,
    faces :: Map.Map faceIdx ((Vector3 vertIdx), face)
} deriving (Show)

convertMesh :: Mesh vertIdx faceIdx vert face -> Maybe (HEMesh vert face s)
convertMesh mesh = do 
  (edgeFaceMap, faceEdgesMap) <- buildEdgeFaceMaps
  



--addFace :: Mesh vertIdx faceIdx vert face -> Map.Map (vertIdx, vertIdx) (faceIdx, faceIdx) -> faceIdx -> (vertIdx, vertIdx) -> StateT (Map.Map (vertIdx, vertIdx) faceIdx, Map.Map faceIdx (Vector3 (vertIdx, vertIdx))) Maybe () 
addFace :: (Ord faceIdx, Ord vertIdx) => Mesh vertIdx faceIdx vert face -> Map.Map (vertIdx, vertIdx) (faceIdx, faceIdx) -> faceIdx -> (vertIdx, vertIdx) -> StateT (Map.Map (vertIdx, vertIdx) faceIdx, Map.Map faceIdx (Vector3 (vertIdx, vertIdx))) Maybe ()
addFace mesh biEdgeMap oldFace edge = do
  (edgeFaceMap, _) <- get
  when (not $ Map.member edge edgeFaceMap) $ do -- check if face is already added
    newFace <- lift $ (Map.lookup (sortTuple edge) biEdgeMap >>= otherElem oldFace)
    newEdges <- liftM (faceEdges . fst) $ lift $ Map.lookup newFace $ faces mesh
    let newEdges' = if elem edge newEdges then newEdges else fmap flipTuple newEdges
    -- add the edges of this face
    addFaceEdges newFace newEdges'
    mapM_ (addEdgeFace newFace) newEdges'
    --let otherEdges = delete edge newEdges
    --let otherFlipedEdges = delete edge $ fmap flipTuple newEdges
    --nextEdges <- minimumBy (compare `on` length) [otherEdges, otherFlipedEdges] -- find the set of edges with the right orientation
    mapM_ (addFace mesh biEdgeMap newFace) $ fmap flipTuple $ delete edge $ toList newEdges'
    
    
--buildEdgeFaceMaps :: Mesh vertIdx faceIdx vert face -> faceIdx -> Bool -> Maybe (Map.Map (vertIdx, vertIdx) faceIdx, Map.Map faceIdx (Vector3 (vertIdx, vertIdx)))
buildEdgeFaceMaps :: (Ord a, Ord k) => Mesh a k vert face -> k -> Bool -> Maybe (Map.Map (a, a) k, Map.Map k (Vector3 (a, a)))
buildEdgeFaceMaps mesh faceIdx orientation = flip execStateT (Map.empty, Map.empty) $ do
    biEdgeMap' <- liftM (Map.unionsWith (++)) $ mapM (lift . getFaceEdges) $ Map.assocs $ faces mesh
    biEdgeMap <- mapM (lift . listToPair) biEdgeMap'
    edges <- liftM (faceEdges . fst) $ lift $ Map.lookup faceIdx $ faces mesh
    let edges' = if orientation then edges else fmap flipTuple edges
    addFaceEdges faceIdx edges'
    mapM_ (addEdgeFace faceIdx) edges'
    mapM_ (addFace mesh biEdgeMap faceIdx) $ fmap flipTuple edges'
    --getFaceEdges 
    --let getOther
    --face = faces mesh
    --biEdgeMapIsConsistent = all ((== 2) . length) $ elems edgeMap
   






addEdgeFace face edge = do
  (edgeFaceMap, faceEdgesMap) <- get
  put $ (Map.insert edge face edgeFaceMap, faceEdgesMap)

addFaceEdges face edges = do
  (edgeFaceMap, faceEdgesMap) <- get
  put $ (edgeFaceMap, Map.insert face edges faceEdgesMap)
  

listToPair (a:b:[]) = Just (a, b)
listToPair _ = Nothing -- throwError "Inconsistent mesh, more than two faces per edge"


getFaceEdges (faceIdx, (verts, _)) | allUnique verts = Just $ Map.fromList $ fmap (\x -> (sortTuple x, [faceIdx])) $ toList $ faceEdges verts
                                   | otherwise = Nothing -- throwError "Inconsistent mesh, face with duplicate verticies"

faceEdges (Vector3 a b c) = Vector3 (a, b) (b, c) (c, a)


sortTuple :: (Ord a) => (a, a) -> (a, a)
sortTuple (a, b) | a > b = (a, b)
                 | otherwise = (b, a)


flipTuple :: (a, b) -> (b, a)           
flipTuple (a, b) = (b, a)

otherElem x (a, b) | x == a && x == b = Nothing
                   | x == a = Just b
                   | x == b = Just a
                   | otherwise = Nothing

allUnique :: (Foldable f, Ord a) => f a -> Bool
allUnique = allUnique' . sort . toList
  where
    allUnique' (a:b:xs) | a == b = False
                        | otherwise = allUnique' (b:xs)
    allUnique' (_:_) = True
    allUnique' [] = True
    
    
 
