module Main exposing (main)

import List
import Browser
import Html exposing (Html)
import Html.Events as Evt
import Html.Attributes as Attr

type alias Model =
    { build_file   : String
    , fields       : List Field
    }

type Msg
    = InputField Field String
    | BuildFile

type FieldType = Text | Select

type alias Field =
    { name   : String
    , ftype  : FieldType
    , ph     : String
    , val    : String
    , label  : String
    , req    : Bool
    , opts   : List String
    }

init : () -> ( Model, Cmd Msg )
init _ =
    { build_file   = ""
    , fields       =
        [ { name  = "Type"
          , val   = "Application"
          , ph    = ""
          , ftype = Select
          , label = "Type"
          , req   = True
          , opts  = [ "Application", "Link", "Directory" ] }

        , { name  = "Name"
          , val   = ""
          , ph    = "Mozilla"
          , ftype = Text
          , label = "Name"
          , req   = True
          , opts  = [] }

        , { name  = "GenericName"
          , val   = ""
          , ph    = "Web Browser"
          , ftype = Text
          , label = "GenericName"
          , req   = False
          , opts  = [] }
        
        , { name  = "Comment"
          , val   = ""
          , ph    = "View sites on the Internet"
          , ftype = Text
          , label = "Comment"
          , req   = False
          , opts  = [] }

        , { name  = "Exec"
          , val   = ""
          , ph    = "/path/to/application"
          , ftype = Text
          , label = "Exec"
          , req   = False
          , opts  = [] }

        , { name  = "URL"
          , val   = ""
          , ph    = "https://.... (Only for Type is Link)"
          , ftype = Text
          , label = "URL"
          , req   = False
          , opts  = [] }

        , { name  = "Icon"
          , val   = ""
          , ph    = "/path/to/icon"
          , ftype = Text
          , label = "Icon"
          , req   = False
          , opts  = [] }

        , { name  = "Categories"
          , val   = "Development"
          , ph    = ""
          , ftype = Select
          , label = "Categories"
          , req   = False
          , opts  = [ "AudioVideo"
                    , "Audio"
                    , "Video"
                    , "Development"
                    , "Education"
                    , "Game"
                    , "Graphics"
                    , "Graphics"
                    , "Network"
                    , "Office"
                    , "Settings"
                    , "System"
                    , "Utility"] }

        , { name  = "Terminal"
          , val   = "False"
          , ph    = ""
          , ftype = Select
          , label = "Terminal"
          , req   = False
          , opts  = [ "False", "True" ] } ]
    } |> update BuildFile

buildLine : Field -> String
buildLine field =
    if field.req == True then
        field.name ++ "=" ++ field.val ++ "\n"
    else
        if String.length field.val > 0 then
            field.name ++ "=" ++ field.val ++ "\n"
        else
            ""

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputField field v ->
            { model | 
                fields = model.fields |> List.map (\f ->
                    if f == field then
                        {f | val = String.trimLeft v}
                    else
                        f) } |> update BuildFile
   
        BuildFile ->
            ({model |
                build_file = "[Desktop Entry]\n" ++ String.concat (List.map buildLine model.fields) }, Cmd.none)

optView : String -> String -> Html Msg
optView fval val =
    Html.option [ Attr.value val, Attr.selected (fval == val) ] [ Html.text val ]

viewTypeField : Field -> Html Msg
viewTypeField field =
    case field.ftype of
        Text ->
            Html.input
                [ Attr.class "form-control"
                , Attr.id field.name
                , Attr.name field.name
                , Attr.value field.val
                , Attr.placeholder field.ph
                , Evt.onInput (InputField field) ] []
        Select ->
            Html.select
                [ Attr.class "form-control"
                , Attr.id field.name
                , Attr.name field.name
                , Evt.onInput (InputField field) ] (List.map (optView field.val) field.opts)

viewField : Field -> Html Msg
viewField field =
    Html.div [ Attr.class "col-md-6 col-12"] [
        Html.div [ Attr.class "form-group" ]
            [ Html.label [ Attr.for field.name ] [ Html.text field.label ]
            , viewTypeField field ]
    ]

view : Model -> Html Msg
view model =
    Html.div [ Attr.class "app-desktop-builder" ]
        [ Html.div [ Attr.class "row" ] (List.map viewField model.fields) 
        , Html.div [] [ Html.h4 [] [ Html.text "Result"] ]
        , Html.div [ Attr.class "form-group" ]
              [ Html.textarea [ Attr.class "form-control form-control-lg"]
                    [ Html.text model.build_file ] 
              ]
        ]

main =
    Browser.element
        { init          = init
        , view          = view
        , update        = update
        , subscriptions = \_ -> Sub.none
        }
