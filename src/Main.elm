module Main exposing ( main )

import Html
import Html.Styled as HS
import Html.Styled.Attributes as HSA
import Css
import Browser as B
import Browser.Navigation as BN
import Url
import Url.Parser as UP exposing ((</>))
import Url.Parser.Query as UQ

import Command as SC
import Tutorial

main : Program () Tutorial.WrapModel (Maybe Tutorial.WrapMessage)
main = B.application {
        init = \_ url key -> (initModel url key, Cmd.none)
      , view = documentWrapper << HS.toUnstyled << HS.map (Just) << viewWithHeaderFooter
      , update = \msg model -> (update msg model, Cmd.none)
      , subscriptions = \_ -> Sub.map (Just << Tutorial.SpaceMessage) SC.subscriptions
      , onUrlChange = urlToMessage
      , onUrlRequest = \urlRequest -> case urlRequest of
            B.Internal url -> urlToMessage url
            B.External s -> Debug.log s Nothing
    }

{-| Initialize the model from a URL and save the navigation key for later use. -}
initModel : Url.Url -> BN.Key -> Tutorial.WrapModel
initModel url key =
    Tutorial.wrapLiftSC
        (Maybe.withDefault identity
            <| Maybe.map SC.applyBookmark
                <| tryUrlByteString url
        )
        (Tutorial.wrapInit key (topUrl url))

topUrl : Url.Url -> String
topUrl url
    = let
        protocol = case url.protocol of
            Url.Http -> "http"
            Url.Https -> "https"
        hostString = url.host ++ Maybe.withDefault "" (Maybe.map (\p -> ":" ++ String.fromInt p) url.port_)
    in protocol ++ "://" ++ hostString ++ url.path

{-! Wrap the HTML view in a document object, specifying the page title. -}
documentWrapper
    : Html.Html msg
    -> B.Document msg
documentWrapper contents
    = {
        title = "Fractal Play"
      , body = [contents]
    }

{-| Main view, including header and tutorial.
    This is defined here because things like the header should be owned by Main.
 -}
viewWithHeaderFooter : Tutorial.WrapModel -> HS.Html Tutorial.WrapMessage
viewWithHeaderFooter model =
    HS.div [ HSA.id "inner-root" ]
        [ HS.header
            [ HSA.class "header" ]
            [ HS.h1
                [ HSA.css [Css.margin2 (Css.rem 1) (Css.rem 0) ] ]
                [ HS.text "Fractal Play" ]
            , HS.map Tutorial.TutorialMessage <| Tutorial.view model.tutorial
            ]
        , HS.map Tutorial.SpaceMessage <| SC.view model.sc
        , HS.footer []
            [ HS.p []
                [ HS.a [ HSA.href "LICENSE.txt" ] [ HS.text "© 2024 Nathan Morrison" ]
                , HS.text "·"
                , HS.a [ HSA.href "https://github.com/morrna/fractal-play" ] [ HS.text "Source" ]
                , HS.text "·"
                , HS.a [ HSA.href "https://github.com/morrna/fractal-play/issues/new" ] [ HS.text "Feedback or ideas?" ]
                ]
            ]
        ]


{-| Update that redirects messages to the right component. -}
update : Maybe Tutorial.WrapMessage -> Tutorial.WrapModel -> Tutorial.WrapModel
update
  = Maybe.withDefault identity
    << Maybe.map ( \message ->
            case message of
                Tutorial.SpaceMessage msg -> Tutorial.wrapLiftSC <| SC.update msg
                Tutorial.TutorialMessage msg -> Tutorial.update msg
        )

{-| Get a message for a given internal URL event. -}
urlToMessage
    : Url.Url
   -> Maybe Tutorial.WrapMessage
urlToMessage url
    = Maybe.map (Tutorial.SpaceMessage << SC.commandMessageApplyBookmark)
        <| tryUrlByteString url

{-| Get a candidate bytestring from the URL if there is one. -}
tryUrlByteString
    : Url.Url
   -> Maybe String
tryUrlByteString url
    = Maybe.withDefault Nothing
        (UP.parse urlParser url)

urlParser : UP.Parser (Maybe String -> a) a
urlParser = UP.oneOf [
        {- index.html needs to be explicitly matched for testing with elm reactor -}
        UP.s "index.html" </> UP.query (UQ.string "")
      , UP.s "dist" </> UP.s "index.html" </> UP.query (UQ.string "")
      , UP.top </> UP.query (UQ.string "")
      , UP.query (UQ.string "")
    ]
