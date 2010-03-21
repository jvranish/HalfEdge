module Data.HalfEdge.MeshLoad (
    Mesh(..),
    Face(..),
    parseASE,
    getFaces
) where

import Data.Maybe

import Control.Monad
import Control.Monad.List
import Control.Applicative ((<$), (<*))

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token

import Data.Vector
import Prelude hiding (snd)

lexerStyle :: LanguageDef st
lexerStyle = emptyDef
                { Token.identStart     = letter 
                , Token.identLetter    = alphaNum <|> oneOf "_"
                , Token.caseSensitive  = False           }
                
lexer :: Token.TokenParser st
lexer = Token.makeTokenParser lexerStyle

lexeme = Token.lexeme lexer

symbol :: String -> Parser String
symbol = Token.symbol lexer

stringLiteral :: Parser String
stringLiteral = Token.stringLiteral lexer

float :: Parser Float
float = liftM realToFrac $ Token.float lexer

index :: Parser Int
index = liftM fromInteger $ Token.integer lexer

braces :: Parser a -> Parser a
braces = Token.braces lexer

identifier :: Parser String
identifier = Token.identifier lexer

data Property = Block [(String, [Property])]
              | Name String
              | String String
              | Number Float
              | Index Int
              | Pair Property Property
          deriving (Show)

number :: Parser Float
number =  try (symbol "-" >> float >>= return . negate) <|>  float

parseItemName :: Parser String
parseItemName = symbol "*" >> lexeme ( many ( alphaNum <|> oneOf "_"))

parseItem :: Parser (String, [Property])
parseItem = liftM2 (,) parseItemName (many parseProperty) <?> "item"

parseProperty :: Parser Property
parseProperty =  try (parsePropertyAtom <* (notFollowedBy $ char ':'))
             <|> liftM2 Pair parsePropertyAtom (symbol ":" >> parseProperty)

parsePropertyAtom :: Parser Property
parsePropertyAtom =  liftM Block (braces (many parseItem))
                 <|> liftM Name identifier
                 <|> liftM String (symbol "\"" >> manyTill (noneOf "\"") (symbol "\""))
                 <|> try (liftM Number number)
                 <|> liftM Index index
                 <?> "property"

parseASE :: String -> IO (Maybe [Mesh Int Float])
parseASE s = parseFromFile (many parseItem >>= (<$ eof) >>= return . getMesh) s >>= packageResult
 
-- verticies, faces, facenNormals, vertexNormals
data Mesh a b = Mesh {
    verticies :: [(a, Vector3 b)],
    faces :: [(a, Vector3 a)],
    faceNormals :: [(a, Vector3 b)],
    vertexNormals :: [(a, Vector3 b)]
} deriving (Show)

data Face a = Face (Vector3 (Vector3 a)) (Vector3 (Vector3 a))
  deriving (Show)
  
getFaces :: Mesh Int Float -> [Face Float]  
getFaces (Mesh verticies faces faceNormals vertexNormals) = fmap getFace faces 
  where
    getFace (i, face) = Face (fmap (unsafeLookup verticies) face) $ Vector3 (getNormal (i*3)) (getNormal (i*3+1)) (getNormal (i*3+2))
    getNormal i = snd $ vertexNormals !! i
    unsafeLookup xs a = fromMaybe (error "no mesh item found") $ lookup a xs   


packageResult (Left a) = return Nothing
packageResult (Right a) = return a


getMesh :: [(String, [Property])] -> Maybe [Mesh Int Float]
getMesh items = runListT $ do
  ("GEOMOBJECT", Block object:_) <- ListT $ Just items
  (Block mesh:_) <- lift $ lookup "MESH" object
  (Block vertices:_) <- lift $ lookup "MESH_VERTEX_LIST" mesh
  (Block faces:_) <- lift $ lookup "MESH_FACE_LIST" mesh
  (Block normals:_) <- lift $ lookup "MESH_NORMALS" mesh
  return $ Mesh [(i, Vector3 x y z) | ("MESH_VERTEX", [Index i, Number x, Number y, Number z]) <- vertices]
    [(i, Vector3 a b c) | ("MESH_FACE", Pair (Index i) (Pair (Name "A") (Index a)):Pair (Name "B") (Index b):Pair (Name "C") (Index c):_) <- faces] 
    [(i, Vector3 x y z) | ("MESH_FACENORMAL", [Index i, Number x, Number y, Number z]) <- normals]
    [(i, Vector3 x y z) | ("MESH_VERTEXNORMAL", [Index i, Number x, Number y, Number z]) <- normals]
    

