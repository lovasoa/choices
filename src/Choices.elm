module Choices
    exposing
        ( Model
        , Answer
        , makeModel
        , makeModelFromTuples
        , getAllSelected
        , getSelected
        , setSelected
        , Msg
        , update
        , ViewParams
        , ViewType(..)
        , view
        , genericView
        )

{-| This library allows you to draw an HTML GUI for choices  between different values.

  This allows to easily represent and switch between
    * `<input type='radio'>`,
    * `<input type='checkbox'>`
    * `<select>`, (and `<select multiple>`)

  You can easily allow the user to choose between values of any Elm type,
  and represent each value as a string with a custom function.

  You can see an simple example of this library in use here:
  [`example.elm`](https://github.com/lovasoa/choices/blob/master/example.elm)

# Model
@docs Model, Answer, makeModel, makeModelFromTuples, getAllSelected, getSelected, setSelected

# View
@docs ViewParams, ViewType, view, genericView

# Update
@docs Msg, update
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String


-- MODEL


{-| Represents a choice (one of the possible answers) and its human description
-}
type alias Answer valueType =
    { value : valueType
    , selected : Bool
    , description : String
    }


{-| A list of answsers (or choices)
-}
type alias Model valueType =
    List (Answer valueType)


{-| Make a model from a list of values a function associating a value to its description

    >>> makeModel (\n -> "This is answer number " ++ toString n) [1,2]
    [
      {value= 1, selected=False, description="This is answer number 1"},
      {value= 2, selected=False, description="This is answer number 2"}
    ]

    >>> makeModel Tuple.second [(True, "Yes"), (False, "No")]
    [
      {value= (True, "Yes"), selected=False, description="Yes"},
      {value= (False, "No"), selected=False, description="No"}
    ]
-}
makeModel : (a -> String) -> List a -> Model a
makeModel stringify =
    List.map
        (\v ->
            { value = v
            , selected = False
            , description = stringify v
            }
        )


{-| Make a model from a list of (value, description) tuples

    >>> makeModelFromTuples  [(True, "Yes"), (False, "No")]
    [
      {value= True, selected=False, description="Yes"},
      {value= False, selected=False, description="No"}
    ]
-}
makeModelFromTuples : List ( a, String ) -> Model a
makeModelFromTuples =
    List.map
        (\( value, description ) ->
            { value = value
            , selected = False
            , description = description
            }
        )


{-| get a list of all answers that have been selected

    >>> getAllSelected
    ...  [
    ...    {value= 1, selected= True, description= "This is answer number 1"},
    ...    {value= 2, selected= False, description= "This is answer number 2"},
    ...    {value= 3, selected= True, description= "This is answer number 2"}
    ...  ]
    [1,3]
-}
getAllSelected : Model valueType -> List valueType
getAllSelected =
    List.filter (.selected) >> List.map (.value)


{-| get the first selected answer

    >>> getSelected
    ...  [
    ...    {value =  1, selected = False, description = "This is answer number 1"},
    ...    {value =  2, selected = False, description = "This is answer number 2"}
    ...  ]
    Nothing

    >>> getSelected
    ...  [
    ...    {value =  1, selected = False, description = "This is answer number 1"},
    ...    {value =  2, selected = True, description = "This is answer number 2"}
    ...  ]
    Just 2
-}
getSelected : Model valueType -> Maybe valueType
getSelected =
    getAllSelected >> List.head


{-| Set the set of selected elements

    >>> setSelected [1]
    ...  [
    ...    {value =  1, selected = False, description = "One"},
    ...    {value =  2, selected = True, description = "Two"}
    ...  ]
    [
      {value= 1, selected= True, description="One"},
      {value= 2, selected= False, description="Two"}
    ]
-}
setSelected : List valueType -> Model valueType -> Model valueType
setSelected selection =
    List.map
        (\answer ->
            { answer | selected = List.member answer.value selection }
        )



-- UPDATE


{-| Indicate how to update the model
-}
type Msg valueType
    = Check valueType Bool Bool


{-| update the model
-}
update : Msg valueType -> Model valueType -> Model valueType
update action model =
    case action of
        Check value deselectOthers selected ->
            List.map
                (\elem ->
                    if elem.value == value then
                        { elem | selected = selected }
                    else
                        { elem
                            | selected =
                                if deselectOthers then
                                    False
                                else
                                    elem.selected
                        }
                )
                model



-- VIEW


type alias ModelName =
    String


{-| What Html element to use in order to draw the GUI.
  * **InputCheckbox** : Use `<input type='checkbox'>`
  * **InputRadio** : Use `<input type='radio'>`
  * **HtmlSelect** *allowMultiple*: Use `<select [multiple]>`
-}
type ViewType
    = InputCheckbox
    | InputRadio
    | HtmlSelect Bool


{-| Parameters of a view
  * viewType : html element to use
  * name : HTML `name` attribute
-}
type alias ViewParams =
    { viewType : ViewType
    , name : String
    }


{-| A view that uses the InputRadio display, and that has its input name set to "ElmChoices"
-}
view : Model valueType -> Html (Msg valueType)
view =
    genericView (ViewParams InputRadio "ElmChoices")


{-| Given a ViewParams and a Model, create the corresponding Html elements
-}
genericView : ViewParams -> Model valueType -> Html (Msg valueType)
genericView params model =
    (answerContainer params.viewType) (List.map (viewAnswer params.viewType params.name) model)


answerContainer viewType =
    case viewType of
        HtmlSelect isMultiple ->
            select [ multiple isMultiple ]

        _ ->
            div []


viewAnswer : ViewType -> ModelName -> Answer valueType -> Html (Msg valueType)
viewAnswer viewType name =
    case viewType of
        InputCheckbox ->
            viewAnswerInput "checkbox" name

        InputRadio ->
            viewAnswerInput "radio" name

        HtmlSelect mult ->
            viewAnswerSelect mult


viewAnswerInput : String -> ModelName -> Answer valueType -> Html (Msg valueType)
viewAnswerInput inputType modelName answer =
    label []
        [ input
            ([ type_ inputType
             , checked answer.selected
             , onCheck (Check answer.value (inputType == "radio"))
             , value (toString answer.value)
             ]
                ++ (if modelName == "" then
                        []
                    else
                        [ name modelName ]
                   )
            )
            []
        , text answer.description
        ]


viewAnswerSelect : Bool -> Answer valueType -> Html (Msg valueType)
viewAnswerSelect isMultiple answer =
    option
        [ selected answer.selected
        , onCheck (Check answer.value (not isMultiple))
        , value (toString answer.value)
        ]
        [ text answer.description ]
