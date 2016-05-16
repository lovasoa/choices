module Choices exposing (
    Model, Answer, makeModel, getAllSelected, getSelected,
    Msg, update,
    ViewType, view, genericView
  )


{-| This library allows you to draw an HTML GUI for choices  between different values

# Model
@docs Model, Answer, makeModel, getAllSelected, getSelected

# Update
@docs Msg, update

# View
@docs ViewType, view, genericView
-}

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String

main =
  Html.beginnerProgram { model = exampleModel, view = view, update = update }


-- MODEL
{-| A choice-}
type alias Answer valueType = {
  value: valueType,
  selected:Bool,
  description:String
}

{-| A list of answsers (or choices) -}
type alias Model valueType = List (Answer valueType)


{-| Make a model from a list of values-}
makeModel : (a -> String) -> List a -> Model a
makeModel stringify = List.map
                        (\v -> {
                          value = v,
                          selected = False,
                          description = stringify v
                        })

exampleModel : Model Int
exampleModel = makeModel (((++)"Answer N°") << toString) [1..10]

{-| get a list of all answers that have been selected -}
getAllSelected : Model valueType -> List valueType
getAllSelected = List.filter (.selected) >> List.map (.value)

{-| get the first selected answer-}
getSelected : Model valueType -> Maybe valueType
getSelected = getAllSelected >> List.head

-- UPDATE

{-| Indicate how to update the model-}
type Msg valueType
    = Check valueType Bool

{-| update the model -}
update : Msg valueType -> Model valueType -> Model valueType
update action model =
  case action of
    Check value selected ->
      List.map
        (\elem -> if elem.value == value then {elem | selected = selected} else elem)
        model


-- VIEW
type alias ModelName = String
{-| What Html element to use in order to draw the GUI-}
type ViewType = InputCheckbox | InputRadio | HtmlSelect Bool

giveName : Model valueType -> ModelName
giveName model = List.map (toString << .value) model
                  |> String.join ","
                  |> ((++)"choices:")

{-| = genericView (InputRadio) -}
view :  Model valueType -> Html (Msg valueType)
view = genericView (InputRadio)

{-| view
-}
genericView : ViewType -> Model valueType -> Html (Msg valueType)
genericView viewType model =
  let name = giveName model
  in (answerContainer viewType) (List.map (viewAnswer viewType name) model)

answerContainer viewType =
  case viewType of
      HtmlSelect isMultiple -> select [multiple isMultiple]
      _ -> div []

viewAnswer : ViewType -> ModelName -> Answer valueType -> Html (Msg valueType)
viewAnswer viewType name =
  case viewType of
    InputCheckbox   -> viewAnswerInput "checkbox" name
    InputRadio      -> viewAnswerInput "radio" name
    HtmlSelect mult -> viewAnswerSelect mult

viewAnswerInput : String -> ModelName -> Answer valueType -> Html (Msg valueType)
viewAnswerInput inputType modelName answer =
  label [] [
    input [
            type' inputType,
            checked answer.selected,
            onCheck (Check answer.value),
            value (toString answer.value),
            name modelName
          ] []
    , text answer.description
  ]

viewAnswerSelect : Bool -> Answer valueType -> Html (Msg valueType)
viewAnswerSelect isMultiple answer =
  option [
          selected answer.selected,
          onCheck (Check answer.value),
          value (toString answer.value)
         ] [text answer.description]
