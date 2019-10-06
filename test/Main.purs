module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.Spec
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Spec.Assertions (shouldEqual)
import Effect.Aff (launchAff_)
import Main
import Data.Maybe (Maybe(..))


data Route = Home
           | GetPost Int
           | GetPostComment Int String

derive instance eqRoute :: Eq Route

instance showRoute :: Show Route where
    show Home = "Home"
    show (GetPost i) = "GetPost " <> show i
    show (GetPostComment i s) = "GetPostComment " <> show i <> " " <> s


main :: Effect Unit
main =
    let
        router :: forall a. Parser (Route -> a) a
        router =
            oneOf
            [ Home </ root
            , s "post" /: [ GetPost </ int
                          , GetPostComment </ int // s "comment" // string
                          ]
            ]
    in
    launchAff_ $ runSpec [consoleReporter] do
      describe "Url string parsing" do
        it "parse /" do
            parse router "/"
                `shouldEqual` (Just Home)

        it "parse /post/5" do
            parse router "/post/4"
                `shouldEqual` (Just $ GetPost 4)

        it "parse /post/5/comment/foo" do
            parse router "/post/5/comment/foo"
                `shouldEqual` (Just $ GetPostComment 5 "foo")

        it "parse /non-exist as Nothing" do
            parse router "/non-exist"
                `shouldEqual` Nothing
