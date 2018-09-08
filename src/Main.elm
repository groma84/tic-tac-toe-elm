module Main exposing (Model, Msg(..), Position(..), Spieler, Symbol(..), init, main, update, view)

import Html exposing (Html, div, h1, h2, text, button)
import Html.Attributes exposing (class, classList, type_)
import Html.Events exposing (onClick)


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
    | NeuesSpiel


symbolToString : Maybe Symbol -> String
symbolToString symbol =
    case symbol of
        Nothing ->
            ""

        Just s ->
            case s of
                X ->
                    "X"

                O ->
                    "O"


hatGewonnen : Model -> Bool
hatGewonnen model =
    let
        pruefeSieg gesuchtesSymbol felder =
            let
                feldIstSymbol symbol feld =
                    case Tuple.second feld of
                        Nothing ->
                            False

                        Just s ->
                            s == symbol
            in
                List.all (feldIstSymbol gesuchtesSymbol) felder

        moeglicheSiegReihen =
            [ [ model.feldLinksOben, model.feldMitteOben, model.feldRechtsOben ]
            , [ model.feldLinksMittig, model.feldMitteMittig, model.feldRechtsMittig ]
            , [ model.feldLinksUnten, model.feldMitteUnten, model.feldRechtsUnten ]
            , [ model.feldLinksOben, model.feldLinksMittig, model.feldLinksUnten ]
            , [ model.feldMitteOben, model.feldMitteMittig, model.feldMitteUnten ]
            , [ model.feldRechtsOben, model.feldRechtsMittig, model.feldRechtsUnten ]
            , [ model.feldLinksOben, model.feldMitteMittig, model.feldRechtsUnten ]
            , [ model.feldRechtsOben, model.feldMitteMittig, model.feldLinksUnten ]
            ]
    in
        List.map (pruefeSieg model.spielerAmZug) moeglicheSiegReihen
            |> List.any (\gewonnen -> gewonnen == True)


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


spielerHatGewonnen : { a | gewinner : Maybe Spieler } -> Spieler -> { a | gewinner : Maybe Spieler }
spielerHatGewonnen model spieler =
    { model | gewinner = Just spieler }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        FeldGeklickt pos symbol ->
            let
                newModel =
                    case symbol of
                        Just _ ->
                            model

                        Nothing ->
                            let
                                gezogen =
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
                            in
                                if hatGewonnen gezogen then
                                    spielerHatGewonnen gezogen model.spielerAmZug
                                else
                                    flipSpielerAmZug gezogen
            in
                ( newModel, Cmd.none )

        NeuesSpiel ->
            init



---- VIEW ----


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

                cssClass =
                    case pos of
                        LinksOben ->
                            "feld_links_oben"

                        MitteOben ->
                            "feld_mitte_oben"

                        RechtsOben ->
                            "feld_rechts_oben"

                        LinksMittig ->
                            "feld_links_mittig"

                        MitteMittig ->
                            "feld_mitte_mittig"

                        RechtsMittig ->
                            "feld_rechts_mittig"

                        LinksUnten ->
                            "feld_links_unten"

                        MitteUnten ->
                            "feld_mitte_unten"

                        RechtsUnten ->
                            "feld_rechts_unten"
            in
                div [ onClick (FeldGeklickt pos symbol) ]
                    [ div
                        [ classList
                            [ ( cssClass, True )
                            , ( "feld", True )
                            , ( "x", symbol == Just X )
                            , ( "o", symbol == Just O )
                            ]
                        ]
                        [ text txt
                        ]
                    ]

        einFeld feld =
            let
                symbol =
                    Tuple.second feld
            in
                einDiv feld (symbolToString symbol)
    in
        case model.gewinner of
            Nothing ->
                div []
                    [ div [ class "spielfeld" ] (getFelder model |> List.map einFeld)
                    , button [ class "spielende_button", type_ "button", onClick NeuesSpiel ] [ text "Neues Spiel" ]
                    ]

            Just sieger ->
                div []
                    [ div [ class "spielende_text" ] [ text ("Spieler " ++ (sieger |> Just |> symbolToString) ++ " hat gewonnen") ]
                    , button [ class "spielende_button", type_ "button", onClick NeuesSpiel ] [ text "Neues Spiel" ]
                    ]


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Tic Tac Toe" ]
        , h2 [] [ text ("Spieler am Zug: " ++ (model.spielerAmZug |> Just |> symbolToString)) ]
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
