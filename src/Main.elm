module Main exposing (Model, Msg(..), Position(..), Spieler, Symbol(..), init, main, update, view)

import Html exposing (Html, div, h1, h2, img, text)
import Html.Events exposing (onClick)



-- TODO
-- Die verschiedenen TODO Stellen umsetzen
-- Feld-Rendering
-- Spielzustand: Im Spiel | Spieler hat gewonnen (Restart)
-- Mouseover Effekte bei Feldern
---- MODEL ----


type Symbol
    = X
    | O


type alias Spieler =
    Symbol


type Position
    = LinksOben
    | MitteOben
    | RechtsOben
    | LinksMittig
    | MitteMittig
    | RechtsMittig
    | LinksUnten
    | MitteUnten
    | RechtsUnten


type alias Feld =
    ( Position, Maybe Symbol )


type alias Model =
    { gewinner : Maybe Spieler
    , spielerAmZug : Spieler
    , feldLinksOben : Feld
    , feldMitteOben : Feld
    , feldRechtsOben : Feld
    , feldLinksMittig : Feld
    , feldMitteMittig : Feld
    , feldRechtsMittig : Feld
    , feldLinksUnten : Feld
    , feldMitteUnten : Feld
    , feldRechtsUnten : Feld
    }


init : ( Model, Cmd Msg )
init =
    ( { gewinner = Nothing
      , spielerAmZug = X
      , feldLinksOben = ( LinksOben, Nothing )
      , feldMitteOben = ( MitteOben, Nothing )
      , feldRechtsOben = ( RechtsOben, Nothing )
      , feldLinksMittig = ( LinksMittig, Nothing )
      , feldMitteMittig = ( MitteMittig, Nothing )
      , feldRechtsMittig = ( RechtsMittig, Nothing )
      , feldLinksUnten = ( LinksUnten, Nothing )
      , feldMitteUnten = ( MitteUnten, Nothing )
      , feldRechtsUnten = ( RechtsUnten, Nothing )
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | FeldGeklickt Position (Maybe Symbol)


hatGewonnen : Model -> Bool
hatGewonnen model =
    -- TODO
    False


flipSpielerAmZug : { a | spielerAmZug : Spieler } -> { a | spielerAmZug : Spieler }
flipSpielerAmZug ms =
    let
        neuerSpielerAmZug =
            case ms.spielerAmZug of
                X ->
                    O

                O ->
                    X
    in
    { ms | spielerAmZug = neuerSpielerAmZug }


spielerHatGewonnen : { a | gewinner : Maybe Spieler } -> Maybe Spieler -> { a | gewinner : Maybe Spieler }
spielerHatGewonnen ms spieler =
    { ms | gewinner = spieler }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        FeldGeklickt pos symbol ->
            -- TODO: Auch wenn ein schon geklicktes Feld geklickt wird, schaltet der nächste Spieler um - das darf nicht sein.
            let
                newModel =
                    case symbol of
                        Just _ ->
                            model

                        Nothing ->
                            case pos of
                                LinksOben ->
                                    { model | feldLinksOben = ( LinksOben, Just model.spielerAmZug ) }

                                MitteOben ->
                                    { model | feldMitteOben = ( MitteOben, Just model.spielerAmZug ) }

                                RechtsOben ->
                                    { model | feldRechtsOben = ( RechtsOben, Just model.spielerAmZug ) }

                                LinksMittig ->
                                    { model | feldLinksMittig = ( LinksMittig, Just model.spielerAmZug ) }

                                MitteMittig ->
                                    { model | feldMitteMittig = ( MitteMittig, Just model.spielerAmZug ) }

                                RechtsMittig ->
                                    { model | feldRechtsMittig = ( RechtsMittig, Just model.spielerAmZug ) }

                                LinksUnten ->
                                    { model | feldLinksUnten = ( LinksUnten, Just model.spielerAmZug ) }

                                MitteUnten ->
                                    { model | feldMitteUnten = ( MitteUnten, Just model.spielerAmZug ) }

                                RechtsUnten ->
                                    { model | feldRechtsUnten = ( RechtsUnten, Just model.spielerAmZug ) }

                nachZug =
                    case hatGewonnen newModel of
                        False ->
                            flipSpielerAmZug newModel

                        True ->
                            spielerHatGewonnen newModel symbol
            in
            ( nachZug, Cmd.none )



---- VIEW ----


symbolToString : Symbol -> String
symbolToString symbol =
    case symbol of
        X ->
            "X"

        O ->
            "O"


getFelder : Model -> List Feld
getFelder model =
    [ model.feldLinksOben
    , model.feldMitteOben
    , model.feldRechtsOben
    , model.feldLinksMittig
    , model.feldMitteMittig
    , model.feldRechtsMittig
    , model.feldLinksUnten
    , model.feldMitteUnten
    , model.feldRechtsUnten
    ]


zeichneSpielfeld : Model -> Html Msg
zeichneSpielfeld model =
    let
        einDiv feld txt =
            let
                ( pos, symbol ) =
                    feld
            in
            div [ onClick (FeldGeklickt pos symbol) ] [ text txt ]

        einFeld feld =
            let
                position =
                    Tuple.first feld

                txt =
                    case position of
                        LinksOben ->
                            "LinksOben"

                        MitteOben ->
                            "MitteOben"

                        RechtsOben ->
                            "RechtsOben"

                        LinksMittig ->
                            "LinksMittig"

                        MitteMittig ->
                            "MitteMittig"

                        RechtsMittig ->
                            "RechtsMittig"

                        LinksUnten ->
                            "LinksUnten"

                        MitteUnten ->
                            "MitteUnten"

                        RechtsUnten ->
                            "RechtsUnten"
            in
            einDiv feld txt
    in
    div [] (getFelder model |> List.map einFeld)


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Tic Tac Toe" ]
        , h2 [] [ text ("Spieler am Zug: " ++ symbolToString model.spielerAmZug) ]
        , zeichneSpielfeld model
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }