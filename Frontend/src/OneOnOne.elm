module OneOnOne exposing (..)

import Browser
import Debug exposing (toString)
import Enums
import Html exposing (Attribute, Html, button, div, input, option, select, span, text, textarea)
import Html.Attributes exposing (class, classList, placeholder, selected, type_, value)
import Html.Events
import Http
import Json.Decode exposing (succeed)
import List exposing (sortBy)
import String exposing (toInt)
import Time exposing (Month(..))



-- CONSTANTS


baseUrl =
    "http://localhost:8080"



-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



-- MODEL


type alias Employee =
    { name : String, id : Int }


type alias Employees =
    List Employee


type TodoState
    = Open
    | Discarded
    | Done


type alias Todo =
    { text : String, state : TodoState, note : Maybe String, toBeImplementedBy : Enums.Target }


type alias Todos =
    List Todo


type alias DateTime =
    ( Time.Posix, Time.Zone )


type alias Topic =
    { question : String, answer : String, putOnAgendaBy : Enums.Source }


type alias Topics =
    List Topic


type alias OneOnOne =
    { employeeId : Int, date : DateTime, topics : Topics }


type alias OneOnOnes =
    List OneOnOne


type alias SelectedEmployee =
    Maybe Employee


type alias TopicSuggestion =
    { suggestedQuestion : String, lastAsked : Maybe DateTime }


type alias TopicSuggestions =
    List TopicSuggestion


type Model
    = Loading
    | Failure
    | Success { employees : Employees, selectedEmployee : SelectedEmployee, todos : Todos, oneOnOnes : OneOnOnes, topicSuggestions : TopicSuggestions }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getEmployees )



-- UPDATE


type Msg
    = GotEmployees (Result Http.Error Employees)
    | GotOneOnOnes (Result Http.Error OneOnOnes)
    | GotTodos (Result Http.Error Todos)
    | GotTopicSuggestions (Result Http.Error TopicSuggestions)
    | EmployeeSelectionChanged SelectedEmployee


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotEmployees result ->
            case result of
                Ok loadedEmployees ->
                    ( Success { employees = sortBy .name loadedEmployees, selectedEmployee = Nothing, todos = [], oneOnOnes = [], topicSuggestions = [] }, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )

        GotOneOnOnes result ->
            case result of
                Ok loadedOneOnOnes ->
                    case model of
                        Success oldModelContent ->
                            ( Success { oldModelContent | oneOnOnes = loadedOneOnOnes }, Cmd.none )

                        other ->
                            ( other, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )

        GotTodos result ->
            case result of
                Ok loadedTodos ->
                    case model of
                        Success oldModelContent ->
                            ( Success { oldModelContent | todos = loadedTodos }, Cmd.none )

                        other ->
                            ( other, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )

        GotTopicSuggestions result ->
            case result of
                Ok loadedTopicSuggestions ->
                    case model of
                        Success oldModelContent ->
                            ( Success { oldModelContent | topicSuggestions = loadedTopicSuggestions }, Cmd.none )

                        other ->
                            ( other, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )

        EmployeeSelectionChanged selectedEmployee ->
            case model of
                Success oldModelContent ->
                    ( Success { oldModelContent | selectedEmployee = selectedEmployee, todos = [], oneOnOnes = [] }, getContentsForSelectedEmployee selectedEmployee )

                other ->
                    ( other, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "style.css" ] []
        , Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "https://fonts.googleapis.com/css2?family=Material+Symbols+Outlined" ] []
        , case model of
            Loading ->
                text "Lade.."

            Failure ->
                text "Etwas ist schiefgelaufen :("

            Success modelContent ->
                if modelContent.employees == [] then
                    text "Keine Mitarbeiter zur Auswahl vorhanden"

                else
                    div [ class "main" ]
                        [ div [ class "left-sidebar" ] (getLeftSidebarContent modelContent.employees modelContent.todos modelContent.selectedEmployee)
                        , div [ class "content-area" ] (getContentAreaContent modelContent.oneOnOnes modelContent.topicSuggestions modelContent.selectedEmployee)
                        ]
        ]


getLeftSidebarContent : Employees -> Todos -> SelectedEmployee -> List (Html Msg)
getLeftSidebarContent employees todos selectedEmployee =
    [ getEmployeeSelectionDropDown employees selectedEmployee, div [ class "todos-area" ] (getTodoAreaContent todos selectedEmployee) ]


getEmployeeSelectionDropDown : Employees -> SelectedEmployee -> Html Msg
getEmployeeSelectionDropDown employees selectedEmployee =
    select [ onSelectedEmployeeChange (changeSelectedEmployee employees) ] (option [] [] :: getEmployeeOptions employees selectedEmployee)


changeSelectedEmployee : Employees -> String -> Msg
changeSelectedEmployee employees idString =
    case toInt idString of
        Nothing ->
            EmployeeSelectionChanged Nothing

        Just id ->
            let
                employee =
                    List.head (List.filter (\emp -> emp.id == id) employees)
            in
            EmployeeSelectionChanged employee


getEmployeeOptions : Employees -> SelectedEmployee -> List (Html Msg)
getEmployeeOptions employees selectedEmployee =
    List.map (getEmployeeOption selectedEmployee) employees


getEmployeeOption : SelectedEmployee -> Employee -> Html Msg
getEmployeeOption selectedEmployee employee =
    option (getEmployeeOptionAttributes employee selectedEmployee) [ text employee.name ]


getEmployeeOptionAttributes : Employee -> SelectedEmployee -> List (Attribute msg)
getEmployeeOptionAttributes employee selectedEmployee =
    let
        isEmployeeSelected =
            case selectedEmployee of
                Just semployee ->
                    semployee == employee

                Nothing ->
                    False
    in
    [ value (toString employee.id), selected isEmployeeSelected ]


getTodoAreaContent : Todos -> SelectedEmployee -> List (Html Msg)
getTodoAreaContent todos selectedEmployee =
    case selectedEmployee of
        Just _ ->
            let
                todosContent =
                    case todos of
                        [] ->
                            [ text "Keine offenen Todos :)" ]

                        _ ->
                            getTodosPresentation todos
            in
            button [ class "outlined" ] [ text "Neues Todo hinzufügen" ] :: todosContent

        Nothing ->
            [ emptyHtml ]


getTodosPresentation : Todos -> List (Html Msg)
getTodosPresentation todos =
    List.map getTodoPresentation todos


getTodoPresentation : Todo -> Html Msg
getTodoPresentation todo =
    let
        maybeNote =
            case todo.note of
                Nothing ->
                    ""

                Just noteText ->
                    noteText

        todoStateClass =
            case todo.state of
                Open ->
                    ( "open", True )

                Discarded ->
                    ( "discarded", True )

                Done ->
                    ( "done", True )

        todoTargetClass =
            case todo.toBeImplementedBy of
                Enums.Manager ->
                    ( "manager-todo", True )

                Enums.Employee ->
                    ( "employee-todo", True )
    in
    div [ classList [ ( "todo", True ), todoStateClass, todoTargetClass ] ] [ input [ value todo.text ] [], input [ placeholder "Notiz", value maybeNote ] [] ]


getContentAreaContent : OneOnOnes -> TopicSuggestions -> SelectedEmployee -> List (Html Msg)
getContentAreaContent oneOnOnes topicSuggestions selectedEmployee =
    case selectedEmployee of
        Nothing ->
            [ text "Wähle einen Mitarbeiter, um eure bisherigen One-on-Ones anzuzeigen" ]

        Just _ ->
            let
                addOneOnOneButton =
                    button [ class "outlined" ] [ text "Neues One-on-One hinzufügen" ]

                addTopicSuggestionsButton =
                    button [ class "outlined" ] [ text "Neuen Vorschlag hinzufügen" ]

                oneOnOnesContent =
                    case oneOnOnes of
                        [] ->
                            [ text "Keine One-on-Ones vorhanden" ]

                        _ ->
                            getOneOnOnesPresentations oneOnOnes

                topicSuggestionsContent =
                    case topicSuggestions of
                        [] ->
                            [ text "Keine Vorschläge :(" ]

                        _ ->
                            getTopicSuggestionsPresentation topicSuggestions
            in
            [ div [ class "one-on-ones" ] (addOneOnOneButton :: oneOnOnesContent)
            , div [ class "topic-suggestions" ] (addTopicSuggestionsButton :: topicSuggestionsContent)
            ]


getOneOnOnesPresentations : OneOnOnes -> List (Html Msg)
getOneOnOnesPresentations oneOnOnes =
    List.map getOneOnOnePresentation oneOnOnes


getOneOnOnePresentation : OneOnOne -> Html Msg
getOneOnOnePresentation oneOnOne =
    div [ class "one-on-one" ] [ div [] [ text (toDateString oneOnOne.date) ], div [ class "topics" ] (getTopicsPresentation oneOnOne.topics) ]


getTopicsPresentation : Topics -> List (Html Msg)
getTopicsPresentation topics =
    List.map getTopicPresentation topics


getTopicPresentation : Topic -> Html Msg
getTopicPresentation topic =
    let
        topicClass =
            case topic.putOnAgendaBy of
                Enums.Manager ->
                    "manager-topic"

                Enums.Employee ->
                    "employee-topic"
    in
    div [ class "topic", class topicClass ] [ input [ class "question", value topic.question ] [], textarea [ class "answer", value topic.answer ] [] ]


getTopicSuggestionsPresentation : TopicSuggestions -> List (Html Msg)
getTopicSuggestionsPresentation topicSuggestions =
    List.map getTopicSuggestionPresentation topicSuggestions


getTopicSuggestionPresentation : TopicSuggestion -> Html Msg
getTopicSuggestionPresentation topicSuggestion =
    let
        lastAskedString =
            case topicSuggestion.lastAsked of
                Nothing ->
                    "-"

                Just date ->
                    toDateString date

        addButton =
            button [ class "material-symbols-outlined" ] [ text "add_circle" ]
    in
    div [ class "topic-suggestion" ] [ addButton, text (topicSuggestion.suggestedQuestion ++ " (Zuletzt: " ++ lastAskedString ++ ")") ]


toDateString : DateTime -> String
toDateString ( time, zone ) =
    String.fromInt (Time.toDay zone time)
        ++ "."
        ++ String.padLeft 2 '0' (String.fromInt (toMonthNumber (Time.toMonth zone time)))
        ++ "."
        ++ String.padLeft 2 '0' (String.fromInt (Time.toYear zone time))


toMonthNumber : Time.Month -> Int
toMonthNumber month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


onSelectedEmployeeChange : (String -> msg) -> Html.Attribute msg
onSelectedEmployeeChange handler =
    Html.Events.on "change" <| Json.Decode.map handler <| Json.Decode.at [ "target", "value" ] Json.Decode.string



-- HTML


getEmployees : Cmd Msg
getEmployees =
    Http.get
        { url = baseUrl ++ "/employees"
        , expect = Http.expectJson GotEmployees employeesDecoder
        }


employeesDecoder : Json.Decode.Decoder Employees
employeesDecoder =
    Json.Decode.list employeeDecoder


employeeDecoder : Json.Decode.Decoder Employee
employeeDecoder =
    Json.Decode.map2 Employee (Json.Decode.field "name" Json.Decode.string) (Json.Decode.field "id" Json.Decode.int)


getContentsForSelectedEmployee : SelectedEmployee -> Cmd Msg
getContentsForSelectedEmployee selectedEmployee =
    case selectedEmployee of
        Just employee ->
            Cmd.batch
                [ Http.get { url = baseUrl ++ "/employee/" ++ String.fromInt employee.id ++ "/oneOnOnes", expect = Http.expectJson GotOneOnOnes oneOnOnesDecoder }
                , Http.get { url = baseUrl ++ "/employee/" ++ String.fromInt employee.id ++ "/todos", expect = Http.expectJson GotTodos todosDecoder }
                , Http.get { url = baseUrl ++ "/employee/" ++ String.fromInt employee.id ++ "/topicSuggestions", expect = Http.expectJson GotTopicSuggestions topicSuggestionsDecoder }
                ]

        Nothing ->
            Cmd.none


topicSuggestionsDecoder : Json.Decode.Decoder TopicSuggestions
topicSuggestionsDecoder =
    Json.Decode.list topicSuggestionDecoder


topicSuggestionDecoder : Json.Decode.Decoder TopicSuggestion
topicSuggestionDecoder =
    Json.Decode.map2 TopicSuggestion (Json.Decode.field "suggestedQuestion" Json.Decode.string) (Json.Decode.maybe (dateTimeDecoder "lastAsked"))


todosDecoder : Json.Decode.Decoder Todos
todosDecoder =
    Json.Decode.list todoDecoder


todoDecoder : Json.Decode.Decoder Todo
todoDecoder =
    Json.Decode.map4 Todo (Json.Decode.field "text" Json.Decode.string) (Json.Decode.field "state" stateDecoder) (Json.Decode.maybe (Json.Decode.field "note" Json.Decode.string)) (Json.Decode.field "toBeImplementedBy" sourceDecoder)


stateDecoder : Json.Decode.Decoder TodoState
stateDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\value ->
                case value of
                    "Open" ->
                        Json.Decode.succeed Open

                    "Discarded" ->
                        Json.Decode.succeed Discarded

                    "Done" ->
                        Json.Decode.succeed Done

                    somethingElse ->
                        Json.Decode.fail <| "Unknown TodoState: " ++ somethingElse
            )


oneOnOnesDecoder : Json.Decode.Decoder OneOnOnes
oneOnOnesDecoder =
    Json.Decode.list oneOnOneDecoder


oneOnOneDecoder : Json.Decode.Decoder OneOnOne
oneOnOneDecoder =
    Json.Decode.map3 OneOnOne (Json.Decode.field "employeeId" Json.Decode.int) (dateTimeDecoder "date") (Json.Decode.field "topics" topicsDecoder)


dateTimeDecoder : String -> Json.Decode.Decoder DateTime
dateTimeDecoder fieldname =
    Json.Decode.field fieldname Json.Decode.int
        |> Json.Decode.andThen
            (\value ->
                succeed ( Time.millisToPosix value, Time.utc )
            )


topicsDecoder : Json.Decode.Decoder Topics
topicsDecoder =
    Json.Decode.list topicDecoder


topicDecoder : Json.Decode.Decoder Topic
topicDecoder =
    Json.Decode.map3 Topic (Json.Decode.field "question" Json.Decode.string) (Json.Decode.field "answer" Json.Decode.string) (Json.Decode.field "putOnAgendaBy" sourceDecoder)


sourceDecoder : Json.Decode.Decoder Enums.Source
sourceDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\value ->
                case value of
                    "Manager" ->
                        Json.Decode.succeed Enums.Manager

                    "Employee" ->
                        Json.Decode.succeed Enums.Employee

                    somethingElse ->
                        Json.Decode.fail <| "Unknown Source: " ++ somethingElse
            )


emptyHtml =
    text ""


loggingDecoder : Json.Decode.Decoder a -> Json.Decode.Decoder a
loggingDecoder realDecoder =
    Json.Decode.value
        |> Json.Decode.andThen
            (\value ->
                case Json.Decode.decodeValue realDecoder value of
                    Ok decoded ->
                        Json.Decode.succeed decoded

                    Err error ->
                        Json.Decode.fail <| Debug.log "decode error" (Json.Decode.errorToString error)
            )
