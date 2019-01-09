module Main exposing (main)

import Dict exposing (Dict)
import OrderedDict exposing (OrderedDict, insert)

import Browser
-- import Html exposing (Html, button, div, text, select, option, input, br)
import Html exposing (..)
import Html.Events exposing (onClick, onInput, onBlur)
import Html.Attributes exposing (..)

import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)


-- MODEL

type Conversion = InchToMet | MetToInch | CelsToFahr | FahrToCels | NothingSelected


type alias Model =
    { variant : Conversion
    , textInput : String
    , outputValue : Maybe Float
    , error : Maybe String
    }

type Msg
    = ChangeMode String | Calculate Float | ProcessInputString String | Skip String

initialModel : Model
initialModel =
    { variant = NothingSelected
    , textInput = ""
    , outputValue = Nothing
    , error = Nothing }


conversionWithDescription : OrderedDict String String
conversionWithDescription =
    OrderedDict.empty
      |> insert "InchToMet" "Inches to Meters"
      |> insert "MetToInch" "Meters to Inches"
      |> insert "CelsToFahr" "Celsius to Fahrenheit"
      |> insert "FahrToCels" "Fahrenheit to Celsius"

-- UPDATE

stringToConversion : String -> Maybe Conversion
stringToConversion string =
    case string of
      "InchToMet" -> Just InchToMet
      "MetToInch" -> Just MetToInch
      "CelsToFahr" -> Just CelsToFahr
      "FahrToCels" -> Just FahrToCels
      _ -> Nothing

update : Msg -> Model -> Model
update msg model =
  case msg of
    ChangeMode targetMode ->
      let
          no_error_model = { model | error = Nothing }
          updateFunction variant = update ( ProcessInputString model.textInput ) { no_error_model | variant = variant}
      in
      case ( stringToConversion targetMode ) of
        Just constructor -> updateFunction constructor
        Nothing -> { model | error = Just ("Wrong selection: " ++ targetMode)}

    Calculate inputValue ->
      let
          updateOutput value = { model | outputValue = Just ( value ) }
      in
      case model.variant of
        InchToMet -> updateOutput ( inputValue * 25.4 / 1000.0 )
        MetToInch -> updateOutput ( inputValue / 25.4 * 1000.0 )
        CelsToFahr -> updateOutput ( inputValue * 9.0 / 5.0 + 32.0 )
        FahrToCels -> updateOutput ( (inputValue - 32.0) * 5.0 / 9.0 )
        NothingSelected -> model

    ProcessInputString str ->
      let
        modelInput = { model | textInput = str }
      in
      case String.toFloat str of
        Just floatValue ->
          update ( Calculate floatValue ) { modelInput | error = Nothing }
        Nothing ->
          { modelInput | outputValue = Nothing, error = Just "Please enter valid float number" }

    Skip _ ->
      model

-- VIEW

toPrecision : Float -> Int -> String
toPrecision float precision =
  let
    precisionBase = toFloat ( 10 ^ precision )
    upToPrecision = float * precisionBase
      |> round
      |> toFloat
  in
    upToPrecision / precisionBase
      |> String.fromFloat

renderOptions : OrderedDict String String -> List ( Html Msg )
renderOptions dict =
  let
      dictWithHtml = dict.dict
        |> Dict.map ( \k a -> option [ value k ] [ text a ] )
      newOrderedDict = OrderedDict dict.order dictWithHtml
  in
    OrderedDict.orderedValues newOrderedDict

viewInput : String -> Model -> String -> ( String -> Msg ) -> Bool -> Html Msg
viewInput ph model vl msg showError =
  p []
    ([ label [] [ text ph ]
    , input [ disabled ( if model.variant == NothingSelected then True else False )
            , value vl
            , onInput msg
            , class "w3-input"
            ] []
    ] ++ if showError == True then
      [viewErrorMessageLabel model]
      else [] )

view : Model -> Html Msg
view model =
    div [class "w3-container"]
        [ h1 [class "w3-center w3-teal w3-wide w3-padding-large w3-card"] [text "Amazing converter"]

        , div [ class "w3-container w3-card w3-margin-bottom" ]

          [ p []
            [ select [ onInput ChangeMode
                     , class "w3-select"]
              ([ option [ disabled True, selected True ] [ text "-- select conversion --" ]]
              ++ renderOptions conversionWithDescription )
            ]

          , viewInput "Value to convert" model model.textInput ProcessInputString True

          , viewInput "Answer" model
              ( case model.outputValue of
                  Just float -> toPrecision float 3
                  Nothing -> ""
              )
              Skip
              False
        ]

        -- , br [] []

        -- , viewErrorMessage model
        , div [ class "w3-card w3-container"]
            [ p [ class "w3-opacity"
                , align "right"]
                [ text "Amazing convert brought to you by"
                , br [] []
                , text "Artem Yastrebov in 2019"
                ]

            ]

        , Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "https://www.w3schools.com/w3css/4/w3.css" ] []

        ]

viewErrorMessageLabel : Model -> Html Msg
viewErrorMessageLabel model =
  case model.error of
    Nothing ->
      label [ ] [text "" ]
    Just msg ->
      label [ class "w3-text-red" ] [ text msg ]

viewErrorMessage : Model -> Html Msg
viewErrorMessage model =
  case model.error of
    Nothing ->
      div [ ] [text "" ]
    Just msg ->
      div [ class "w3-margin-top w3-card w3-panel w3-red w3-padding w3-container" ] [ b [] [text msg] ]

main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
