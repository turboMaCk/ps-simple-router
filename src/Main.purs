module Main
    ( Parser
    , State
    , s
    , root
    , string
    , int
    , number
    , custom
    , parse
    , oneOf
    , slash
    , from
    , branch
    , (//)
    , (/:)
    , (</)
    ) where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.String as String
import Data.Maybe (Maybe(..), maybe)
import Data.Array as Arr
import Data.Int as Int
import Data.Number as Num


toParts :: String -> Array String
toParts =
    Arr.filter (not <<< String.null) <<<
        String.split (String.Pattern "/")


type State a =
    { value :: a
    , path :: Array String
    }


data Parser a b = Parser
    (State a -> Array (State b))


s :: forall a. String -> Parser a a
s str =
    Parser $ \ { path, value } ->
        maybe [] (\x -> [x]) $
              Arr.uncons path >>= \ { head, tail } ->
                  if str == head then
                    Just { path: tail, value }
                  else
                      Nothing


custom :: forall a b. (String -> Maybe a) -> Parser (a -> b) b
custom fromString =
    Parser $ \ { path, value } ->
        maybe [] pure $
              Arr.uncons path >>= \ { head, tail } ->
                  (\nextVal ->
                           { value: value nextVal
                           , path: tail
                           }
                  ) <$> fromString head


string :: forall a. Parser (String -> a) a
string = custom Just


int :: forall a. Parser (Int -> a) a
int = custom Int.fromString


number :: forall a. Parser (Number -> a) a
number = custom Num.fromString


slash :: forall a b c. Parser a b -> Parser b c -> Parser a c
slash (Parser before) (Parser after) =
    Parser $ \rec ->
      before rec >>= after
infixr 8 slash as //


instance semigroupParser :: Semigroup (Parser a a) where
    append = slash

instance monoidParser :: Monoid (Parser a a) where
    mempty = root


getFirst :: forall a. Array (State a) -> Maybe a
getFirst states =
    Arr.uncons states >>= \ { head, tail } ->
          if Arr.null head.path then
              Just head.value
          else
              getFirst tail


mapState :: forall a b. (a -> b) -> State a -> State b
mapState f { value, path } =
    { value: f value, path: path }


parse :: forall a. Parser (a -> a) a -> String -> Maybe a
parse (Parser parser) url =
    getFirst $ parser { value: identity
                      , path: toParts url
                      }

from :: forall a b c. a -> Parser a b -> Parser (b -> c) c
from subVal (Parser parseArg) =
    Parser $ \ { value, path } ->
        map (mapState value) $ parseArg { value: subVal, path }
infixr 8 from as </


root :: forall a. Parser a a
root =
    Parser pure


oneOf :: forall a b. Array (Parser a b) -> Parser a b
oneOf xs =
    Parser $ \state ->
        xs >>= \(Parser parser) -> parser state


branch :: forall a b c. Parser a b -> Array (Parser b c) -> Parser a c
branch parser xs = parser // oneOf xs
infixr 7 branch as /:
