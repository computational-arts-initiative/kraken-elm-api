module Main exposing (..)

import Kraken
import Kraken.Product exposing (Product)
import Kraken.Product as Product

import Browser
import Html exposing (Html, text, pre)
import Http



-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL


type Model
  = Failure
  | Loading
  | Success (List Product)


init : () -> (Model, Cmd Msg)
init _ =
  ( Loading
  , Kraken.requestProducts
    |> Cmd.map GotProducts
  )



-- UPDATE


type Msg
  = GotProducts (Result Http.Error (List Product))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotProducts result ->
      case result of
        Ok fullText ->
          (Success fullText, Cmd.none)

        Err _ ->
          (Failure, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW

infoLine : Product -> String
infoLine product =
  Product.codeOf product
    ++ " : " ++ Product.nameOf product
    ++ " : " ++ Product.key product
    ++ " : " ++ (Product.shortKey product |> Maybe.withDefault "--")
    ++ " : " ++ (Product.twoLetter product |> Maybe.withDefault "--")
    ++ " : " ++ (Product.tagline product |> Maybe.withDefault "[no tagline]")
    ++ " : " ++ (Product.ico16 product |> Maybe.map .vector |> Maybe.withDefault "[no icon]")


view : Model -> Html Msg
view model =
  case model of
    Failure ->
      text "I was unable to load products list."

    Loading ->
      text "Loading..."

    Success products ->
      pre [] [ text
        <| String.join "\n"
        <| List.map infoLine products
      ]