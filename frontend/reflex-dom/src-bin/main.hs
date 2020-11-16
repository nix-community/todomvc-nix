import Reflex.Dom
import Data.Monoid

main :: IO ()
main = mainWidgetWithHead headWidget bodyWidget

headWidget = do
  elAttr "link" ("href" =: "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css" <> "rel" =: "stylesheet" <> "type" =: "text/css") $ return ()

bodyWidget = do
  divClass "container" $ divClass "jumbotron" $ do
    el "h1" $ text "Hello, world!"
    el "p" $ text "This was made with Reflex and Bootstrap. This is a simple hero unit, a simple jumbotron-style component for calling extra attention to featured content or information."
    el "p" $ elAttr "a" ("class" =: "btn btn-primary btn-lg" <> "href" =: "https://github.com/ryantrinkle/try-reflex" <> "role" =: "button") $ text "Learn more"
