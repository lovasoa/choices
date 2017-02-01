module Example exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Choices


type alias Model =
    { choicesModel : Choices.Model Int }


main =
    Html.beginnerProgram
        { model = init
        , view = view
        , update = update
        }


init : Model
init =
    { choicesModel =
        Choices.makeModelFromTuples
            [ ( 1, "The first choice" )
            , ( 2, "The second choice" )
            , ( 3, "Last but not least: choice number 3" )
            ]
    }


type Msg
    = ChoicesMsg (Choices.Msg Int)


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChoicesMsg choicesMsg ->
            { model | choicesModel = Choices.update choicesMsg model.choicesModel }


view : Model -> Html Msg
view model =
    article
        []
        [ Html.map ChoicesMsg (Choices.view model.choicesModel)
        , text
            (case Choices.getSelected model.choicesModel of
                Just value ->
                    "You chose number " ++ toString value

                Nothing ->
                    "You didn't make a choice yet..."
            )
        ]
