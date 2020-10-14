port module Main exposing
    ( RawTab
    , TabId
    , TabInfo
    , TabStatus
    , WindowId
    , WindowInfo
    , WindowMap
    , WindowStatus
    , applySearch
    , checkTab
    , convertFromJson
    , convertToMap
    , matchTab
    , migrateStatus
    , splitQuery
    , updateWindowInfo
    )

import Browser
import Debounce exposing (Debounce)
import Dict
import Html
import Html.Attributes as A
import Html.Events
import Html.Lazy
import Json.Decode as D
import Regex
import Set
import Task



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- DATA


type alias RawTab =
    { id : TabId
    , title : String
    , url : String
    , favicon : String
    , windowId : Int
    }


type alias WindowMap =
    Dict.Dict WindowId WindowInfo


type alias WindowId =
    Int


type alias WindowInfo =
    { id : WindowId
    , name : String
    , tabInfos : List TabInfo
    , status : WindowStatus
    }


type alias WindowStatus =
    { show : Bool
    , expand : Bool
    }


type alias TabId =
    Int


type alias TabInfo =
    { id : TabId
    , title : String
    , url : String
    , favicon : String
    , windowId : WindowId
    , status : TabStatus
    }


type alias TabStatus =
    { show : Bool
    , checked : Bool
    }


type alias MoveTabsArgs =
    { tabIds : List TabId
    , windowId : Maybe WindowId
    }



-- MODEL


type alias Model =
    { windows : WindowMap
    , updatedDebouncer : Debounce ()
    , query : String
    , queryDebouncer : Debounce String
    , searchMode : Bool
    , showWindowSelector : Bool
    , openerWindowId : WindowId
    , destinationWindowId : Maybe WindowId
    , isProcessing : Bool
    }


initialModel : Model
initialModel =
    { windows = Dict.empty
    , updatedDebouncer = Debounce.init
    , query = ""
    , queryDebouncer = Debounce.init
    , searchMode = False
    , showWindowSelector = False
    , openerWindowId = -1
    , destinationWindowId = Nothing
    , isProcessing = True
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, initialize )


initialize : Cmd Msg
initialize =
    getTabs ()



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ gotTabs GotTabs
        , updatedBrowser UpdatedBrowser
        ]



-- DEBOUNCE CONFIG


updatedConfig : (Debounce.Msg -> Msg) -> Debounce.Config Msg
updatedConfig debounceMsg =
    { strategy = Debounce.later 100
    , transform = debounceMsg
    }


queryConfig : (Debounce.Msg -> Msg) -> Debounce.Config Msg
queryConfig debounceMsg =
    { strategy = Debounce.later 500
    , transform = debounceMsg
    }



-- UPDATE


type Msg
    = UpdatedBrowser String
    | DebounceUpdated Debounce.Msg
    | GotTabs String
    | SetQuery String
    | DebounceQuery Debounce.Msg
    | Search String
    | ClearQuery
    | ShowWindow WindowId
    | ExpandWindow WindowId Bool
    | CheckAll WindowId Bool
    | DeleteTabs WindowId
    | MoveTabs
    | OpenWindowSelector WindowId
    | CloseWindowSelector
    | SelectedDestination (Maybe WindowId)
    | CheckTab WindowId TabId Bool
    | OpenTab WindowId TabId


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTabs json ->
            let
                newWindows =
                    convertFromJson json
                        |> Result.withDefault []
                        |> convertToMap

                oldWindows : Dict.Dict TabId TabInfo
                oldWindows =
                    Dict.values model.windows
                        |> List.map (\x -> x.tabInfos)
                        |> List.concat
                        |> List.map (\x -> ( x.id, x ))
                        |> Dict.fromList

                setupStatus : TabInfo -> TabInfo
                setupStatus ti =
                    Dict.get ti.id oldWindows
                        |> migrateStatus ti
                        |> applySearch model.searchMode (splitQuery model.query)

                wiUpdater wi =
                    let
                        status =
                            case Dict.get wi.id model.windows of
                                Just value ->
                                    value.status

                                Nothing ->
                                    wi.status

                        tabInfos =
                            List.map setupStatus wi.tabInfos
                    in
                    { wi | tabInfos = tabInfos, status = status }

                windows =
                    List.foldr
                        (\wi acc -> updateWindowInfo wi wiUpdater acc)
                        newWindows
                        (Dict.keys newWindows)
            in
            ( { model | windows = windows, isProcessing = False }
            , Cmd.none
            )

        UpdatedBrowser message ->
            let
                ( debounce, cmd ) =
                    Debounce.push
                        (updatedConfig DebounceUpdated)
                        ()
                        model.updatedDebouncer
            in
            ( { model | updatedDebouncer = debounce }
            , cmd
            )

        DebounceUpdated msg_ ->
            let
                ( debounce, cmd ) =
                    Debounce.update
                        (updatedConfig DebounceUpdated)
                        (Debounce.takeLast getTabs)
                        msg_
                        model.updatedDebouncer
            in
            ( { model | updatedDebouncer = debounce }
            , cmd
            )

        SetQuery query ->
            let
                ( debounce, cmd ) =
                    Debounce.push
                        (queryConfig DebounceQuery)
                        query
                        model.queryDebouncer
            in
            ( { model | query = query, queryDebouncer = debounce }
            , cmd
            )

        DebounceQuery msg_ ->
            let
                ( debounce, cmd ) =
                    Debounce.update
                        (queryConfig DebounceQuery)
                        (Debounce.takeLast search)
                        msg_
                        model.queryDebouncer
            in
            ( { model | queryDebouncer = debounce }
            , cmd
            )

        CheckTab windowId tabId checked ->
            let
                cond ti =
                    ti.id == tabId
            in
            ( { model | windows = checkTab windowId cond checked model.windows }
            , Cmd.none
            )

        OpenTab windowId tabId ->
            ( model, openTab tabId )

        DeleteTabs windowId ->
            let
                getIds tis =
                    List.filter (\x -> x.status.checked) tis
                        |> List.map (\x -> x.id)

                ids =
                    case Dict.get windowId model.windows of
                        Just value ->
                            getIds value.tabInfos

                        Nothing ->
                            []
            in
            ( { model | isProcessing = True }
            , deleteTabs ids
            )

        ExpandWindow windowId expand ->
            let
                windows =
                    updateWindowStatus
                        windowId
                        (\ws -> { ws | expand = expand })
                        model.windows
            in
            ( { model | windows = windows }, Cmd.none )

        ShowWindow windowId ->
            let
                windows =
                    updateWindowStatus
                        windowId
                        (\ws -> { ws | show = not ws.show })
                        model.windows
            in
            ( { model | windows = windows }, Cmd.none )

        OpenWindowSelector windowId ->
            let
                destinationWindowId =
                    Dict.values model.windows
                        |> List.filter (\x -> x.id /= windowId)
                        |> List.sortBy .id
                        |> List.head
                        |> Maybe.andThen (\x -> Just x.id)
            in
            ( { model
                | showWindowSelector = True
                , openerWindowId = windowId
                , destinationWindowId = destinationWindowId
              }
            , Cmd.none
            )

        CloseWindowSelector ->
            ( { model | showWindowSelector = False }, Cmd.none )

        SelectedDestination destinationWindowId ->
            ( { model | destinationWindowId = destinationWindowId }, Cmd.none )

        MoveTabs ->
            let
                ids =
                    Dict.get model.openerWindowId model.windows
                        |> Maybe.map (\x -> x.tabInfos)
                        |> Maybe.withDefault []
                        |> List.filter (\x -> x.status.checked)
                        |> List.map (\x -> x.id)
            in
            ( { model
                | showWindowSelector = False
                , isProcessing = True
              }
            , moveTabs (MoveTabsArgs ids model.destinationWindowId)
            )

        Search query ->
            let
                queries =
                    splitQuery query

                searchMode =
                    List.length queries /= 0

                tiUpdater ti =
                    let
                        uncheck ts =
                            { ts | checked = False }
                    in
                    applySearch True queries ti
                        |> (\x -> { x | status = uncheck x.status })

                wiUpdater wi =
                    { wi | tabInfos = List.map tiUpdater wi.tabInfos }

                windows =
                    List.foldr
                        (\wi acc -> updateWindowInfo wi wiUpdater acc)
                        model.windows
                        (Dict.keys model.windows)
            in
            ( { model | windows = windows, searchMode = searchMode }, Cmd.none )

        ClearQuery ->
            ( { model | query = "" }, search "" )

        CheckAll windowId checked ->
            let
                cond ti =
                    ti.status.show

                windows =
                    checkTab windowId cond checked model.windows
            in
            ( { model | windows = windows }, Cmd.none )



-- VIEW


view : Model -> Html.Html Msg
view model =
    let
        callSelector =
            Html.Lazy.lazy3
                windowSelectorView
                model.windows
                model.destinationWindowId
                model.openerWindowId

        selector =
            if model.showWindowSelector then
                Html.div [ A.class "ws-wrapper" ]
                    [ callSelector ]

            else
                Html.text ""

        callNav =
            Html.Lazy.lazy4
                navView
                model.windows
                model.query
                model.searchMode
                model.isProcessing

        callMain =
            Html.Lazy.lazy2
                mainView
                model.windows
                model.searchMode
    in
    Html.div [ A.class "app-wrapper" ]
        [ Html.div [ A.class "app" ]
            [ callNav
            , callMain
            ]
        , selector
        ]


navView : WindowMap -> String -> Bool -> Bool -> Html.Html Msg
navView windows query searchMode isProcessing =
    let
        clearClass =
            mkc [ mkt (String.isEmpty query) "hidden" ]

        statusClasses =
            mkc [ "st", mkt (not isProcessing) "hidden_" ]

        callWindow =
            Html.Lazy.lazy2
                navWindowInfoView
                searchMode
    in
    Html.div [ A.class "nav" ]
        [ Html.div [ A.class "br" ]
            [ Html.img [ A.src "/img/logo.svg" ] []
            , Html.p [] [ Html.text "grasper" ]
            ]
        , Html.div [ A.class "sb" ]
            [ Html.div [ A.class "sb-title" ]
                [ Html.img [ A.src "/img/search.svg" ] []
                , Html.p [] [ Html.text "Search" ]
                ]
            , Html.div [ A.class "sb-box" ]
                [ Html.input
                    [ A.type_ "text"
                    , A.value query
                    , Html.Events.onInput SetQuery
                    ]
                    []
                , Html.button
                    [ A.class clearClass
                    , A.tabindex -1
                    ]
                    [ Html.img
                        [ A.src "/img/delete.svg"
                        , A.title "Clear"
                        , Html.Events.onClick ClearQuery
                        ]
                        []
                    ]
                ]
            ]
        , Html.div [ A.class "wl" ]
            [ Html.div [ A.class "wl-title" ]
                [ Html.img [ A.src "/img/window.svg" ] []
                , Html.p [] [ Html.text "Windows" ]
                ]
            , Html.div [ A.class "wl-container" ]
                (windows
                    |> Dict.values
                    |> List.map callWindow
                )
            ]
        , Html.div [ A.class statusClasses ]
            [ Html.img [ A.src "/img/spinner.svg" ] []
            , Html.span [] [ Html.text "Processing..." ]
            ]
        ]


navWindowInfoView : Bool -> WindowInfo -> Html.Html Msg
navWindowInfoView searchMode windowInfo =
    let
        callCount =
            Html.Lazy.lazy3
                tabCountView
                "wl-info"
                searchMode
                windowInfo.tabInfos
    in
    Html.div [ A.class "wl-window" ]
        [ Html.label []
            [ Html.input
                [ A.type_ "checkbox"
                , A.checked windowInfo.status.show
                , Html.Events.onClick (ShowWindow windowInfo.id)
                ]
                []
            , Html.text windowInfo.name
            ]
        , callCount
        ]


mainView : WindowMap -> Bool -> Html.Html Msg
mainView windows searchMode =
    windows
        |> Dict.values
        |> List.map (Html.Lazy.lazy2 windowInfoView searchMode)
        |> Html.div [ A.class "main" ]


windowInfoView : Bool -> WindowInfo -> Html.Html Msg
windowInfoView searchMode windowInfo =
    let
        windowClasses =
            mkc
                [ "wd"
                , mkt windowInfo.status.expand "expand"
                , mkt (not windowInfo.status.show) "hidden"
                ]

        callHeader =
            Html.Lazy.lazy3
                windowHeaderView
                windowInfo.id
                windowInfo.name
                windowInfo.status.expand

        callAction =
            Html.Lazy.lazy3
                windowActionView
                searchMode
                windowInfo.id
                windowInfo.tabInfos

        callTab ti =
            Html.Lazy.lazy7
                tabInfoView
                ti.id
                ti.title
                ti.url
                ti.favicon
                ti.windowId
                ti.status.show
                ti.status.checked
    in
    Html.div [ A.class windowClasses ]
        [ callHeader
        , callAction
        , Html.div [ A.class "wd-container" ]
            (List.map callTab windowInfo.tabInfos)
        ]


windowHeaderView : WindowId -> String -> Bool -> Html.Html Msg
windowHeaderView windowId name expand =
    let
        expandClass =
            mkt expand "hidden"

        contractClass =
            mkt (not expand) "hidden"
    in
    Html.div [ A.class "wh" ]
        [ Html.div [ A.class "wh-title" ]
            [ Html.text name ]
        , Html.div [ A.class "wh-action" ]
            [ Html.button
                [ A.class expandClass
                , A.tabindex -1
                , Html.Events.onClick (ExpandWindow windowId True)
                ]
                [ Html.img [ A.src "/img/expand.svg", A.title "Expand" ] [] ]
            , Html.button
                [ A.class contractClass
                , A.tabindex -1
                , Html.Events.onClick (ExpandWindow windowId False)
                ]
                [ Html.img [ A.src "/img/contract.svg", A.title "Normal" ] [] ]
            ]
        ]


windowActionView : Bool -> WindowId -> List TabInfo -> Html.Html Msg
windowActionView searchMode windowId tabInfos =
    let
        checkedTabExists =
            List.any (\x -> x.status.checked) tabInfos

        tabActionClasses =
            mkc
                [ "wm-action"
                , mkt (not checkedTabExists) "invisible"
                ]

        checkedCount =
            List.filter (\x -> x.status.checked) tabInfos
                |> List.length
                |> String.fromInt

        callButton =
            Html.Lazy.lazy2
                checkButtonView
                windowId
                tabInfos

        callCount =
            Html.Lazy.lazy3
                tabCountView
                "wm-info"
                searchMode
                tabInfos
    in
    Html.div [ A.class "wm" ]
        [ callButton
        , Html.div [ A.class tabActionClasses ]
            [ Html.button
                [ Html.Events.onClick (DeleteTabs windowId) ]
                [ Html.img
                    [ A.src "/img/trash.svg"
                    , A.title "Delete Tabs"
                    ]
                    []
                ]
            , Html.button
                [ Html.Events.onClick (OpenWindowSelector windowId) ]
                [ Html.img
                    [ A.src "/img/move.svg"
                    , A.title "Move Tabs"
                    ]
                    []
                ]
            , Html.div []
                [ Html.img
                    [ A.src "/img/check.svg"
                    , A.title "Checked Tabs"
                    ]
                    []
                , Html.text checkedCount
                ]
            ]
        , callCount
        ]


tabInfoView : TabId -> String -> String -> String -> WindowId -> Bool -> Bool -> Html.Html Msg
tabInfoView tabId title url favicon windowId show checked =
    let
        isEmptyFavicon =
            String.isEmpty favicon

        titleClasses =
            mkc [ "tb-title", mkt isEmptyFavicon "no-favicon" ]

        tabClasses =
            mkc [ "tb", mkt (not show) "hidden_" ]

        faviconElement : Html.Html Msg
        faviconElement =
            if isEmptyFavicon then
                Html.text ""

            else
                Html.img [ A.src favicon ] []
    in
    Html.div [ A.class tabClasses ]
        [ Html.input
            [ A.type_ "checkbox"
            , A.checked checked
            , Html.Events.onCheck (CheckTab windowId tabId)
            ]
            []
        , Html.button
            [ A.class titleClasses
            , Html.Events.onClick (OpenTab windowId tabId)
            , A.tabindex -1
            ]
            [ faviconElement
            , Html.div [] [ Html.text title ]
            ]
        , Html.div [ A.class "tb-url" ]
            [ Html.text url ]
        ]


windowSelectorView : WindowMap -> Maybe WindowId -> WindowId -> Html.Html Msg
windowSelectorView windows destinationWindowId openerWindowId =
    let
        newWindowValue =
            "new"

        selectedValue =
            case destinationWindowId of
                Just value ->
                    String.fromInt value

                Nothing ->
                    newWindowValue

        makeOption wi =
            let
                val =
                    String.fromInt wi.id
            in
            Html.option
                [ A.value val
                , A.selected (val == selectedValue)
                ]
                [ Html.text wi.name ]

        newWindow =
            Html.option
                [ A.value newWindowValue
                , A.selected (newWindowValue == selectedValue)
                ]
                [ Html.text "[ New Window ]" ]

        makeDestinations windowInfos =
            List.sortBy .id windowInfos
                |> List.filter (\x -> x.id /= openerWindowId)
                |> List.map makeOption
                |> (\x -> x ++ [ newWindow ])

        selected x =
            SelectedDestination (String.toInt x)
    in
    Html.div [ A.class "ws" ]
        [ Html.div [ A.class "ws-header" ]
            [ Html.div []
                [ Html.text "Moving selected tabs to" ]
            , Html.button
                [ Html.Events.onClick CloseWindowSelector ]
                [ Html.img [ A.src "/img/closer.svg", A.title "Close" ] [] ]
            ]
        , Html.div [ A.class "ws-body" ]
            [ Dict.values windows
                |> makeDestinations
                |> Html.select
                    [ A.class "ws-selector"
                    , Html.Events.onInput selected
                    ]
            , Html.div [ A.class "ws-selector-aux" ]
                [ Html.img [ A.src "/img/down.svg" ] [] ]
            ]
        , Html.div [ A.class "ws-footer" ]
            [ Html.button
                [ Html.Events.onClick CloseWindowSelector ]
                [ Html.text "Cancel" ]
            , Html.button
                [ A.class "ok"
                , Html.Events.onClick MoveTabs
                ]
                [ Html.text "Move" ]
            ]
        ]


tabCountView : String -> Bool -> List TabInfo -> Html.Html Msg
tabCountView classes searchMode tabInfos =
    let
        hitsLabel =
            mkt searchMode "hits:"

        hitsContent =
            mkt searchMode (showCount tabInfos)
    in
    Html.div [ A.class classes ]
        [ Html.div [ A.class "matched" ] [ Html.text hitsLabel ]
        , Html.div [ A.class "matched" ] [ Html.text hitsContent ]
        , Html.div [] [ Html.text "tabs:" ]
        , Html.div [] [ Html.text (tabCount tabInfos) ]
        ]


checkButtonView : WindowId -> List TabInfo -> Html.Html Msg
checkButtonView windowId tabInfos =
    let
        showTabInfos =
            List.filter (\x -> x.status.show) tabInfos

        checkedCount =
            List.filter (\x -> x.status.checked) showTabInfos
                |> List.length

        ( img, checked ) =
            if checkedCount == 0 then
                ( "/img/cb_none.svg", True )

            else if checkedCount == List.length showTabInfos then
                ( "/img/cb_checked.svg", False )

            else
                ( "/img/cb_indeterminate.svg", False )
    in
    Html.button
        [ A.class "wm-checker"
        , Html.Events.onClick (CheckAll windowId checked)
        ]
        [ Html.img [ A.src img ] [] ]



-- HELPERS


convertFromJson : String -> Result D.Error (List RawTab)
convertFromJson json =
    let
        decoder =
            D.map5 RawTab
                (D.field "id" D.int)
                (D.field "title" D.string)
                (D.field "url" D.string)
                (D.field "favicon" D.string)
                (D.field "windowId" D.int)
    in
    D.decodeString (D.list decoder) json


convertToMap : List RawTab -> WindowMap
convertToMap rawTabs =
    let
        gatherByWindowId : WindowId -> List RawTab
        gatherByWindowId wid =
            List.foldr
                (\x acc ->
                    if x.windowId == wid then
                        x :: acc

                    else
                        acc
                )
                []
                rawTabs

        convertToTabInfo : RawTab -> TabInfo
        convertToTabInfo x =
            TabInfo x.id x.title x.url x.favicon x.windowId (TabStatus True False)

        makeWindowInfo : WindowId -> ( WindowId, WindowInfo )
        makeWindowInfo wid =
            Tuple.pair wid <|
                WindowInfo
                    wid
                    ("window - " ++ String.fromInt wid)
                    (gatherByWindowId wid
                        |> List.map convertToTabInfo
                    )
                    (WindowStatus True False)
    in
    List.map (\x -> x.windowId) rawTabs
        |> Set.fromList
        |> Set.toList
        |> List.map makeWindowInfo
        |> Dict.fromList


updateWindowInfo : WindowId -> (WindowInfo -> WindowInfo) -> WindowMap -> WindowMap
updateWindowInfo windowId updater windows =
    let
        cb : Maybe WindowInfo -> Maybe WindowInfo
        cb =
            Maybe.andThen <| \x -> Just (updater x)
    in
    Dict.update windowId cb windows


updateWindowStatus : WindowId -> (WindowStatus -> WindowStatus) -> WindowMap -> WindowMap
updateWindowStatus windowId wsUpdater windows =
    updateWindowInfo
        windowId
        (\wi -> { wi | status = wsUpdater wi.status })
        windows


checkTab : WindowId -> (TabInfo -> Bool) -> Bool -> WindowMap -> WindowMap
checkTab windowId cond checked windows =
    let
        tsUpdater ts =
            { ts | checked = checked }

        tiUpdater ti =
            if cond ti then
                { ti | status = tsUpdater ti.status }

            else
                ti

        wiUpdater wi =
            { wi | tabInfos = List.map tiUpdater wi.tabInfos }
    in
    updateWindowInfo windowId wiUpdater windows


search : String -> Cmd Msg
search query =
    Task.perform Search (Task.succeed query)


matchTab : List String -> TabInfo -> Bool
matchTab queries tabInfo =
    let
        title =
            String.toLower tabInfo.title

        url =
            String.toLower tabInfo.url

        isMatch q =
            String.contains q title || String.contains q url
    in
    List.all isMatch queries


splitQuery : String -> List String
splitQuery query =
    let
        space =
            Maybe.withDefault Regex.never <|
                Regex.fromString "( |\u{3000})+"
    in
    Regex.replace space (\_ -> " ") query
        |> String.split " "
        |> List.filter (\x -> not (String.isEmpty x))
        |> List.map String.toLower


migrateStatus : TabInfo -> Maybe TabInfo -> TabInfo
migrateStatus new current =
    let
        status =
            case current of
                Just value ->
                    if value.windowId == new.windowId then
                        value.status

                    else
                        TabStatus value.status.show False

                Nothing ->
                    new.status
    in
    { new | status = status }


applySearch : Bool -> List String -> TabInfo -> TabInfo
applySearch searchMode queries ti =
    let
        status =
            if searchMode then
                ti.status
                    |> (\ts -> { ts | show = matchTab queries ti })

            else
                ti.status
    in
    { ti | status = status }


tabCount : List TabInfo -> String
tabCount tabInfos =
    List.length tabInfos |> String.fromInt


showCount : List TabInfo -> String
showCount tabInfos =
    List.filter (\x -> x.status.show) tabInfos
        |> List.length
        |> String.fromInt


mkc : List String -> String
mkc classes =
    List.filter (String.isEmpty >> not) classes
        |> String.join " "


mkt : Bool -> String -> String
mkt cond match =
    if cond then
        match

    else
        ""



-- PORT


port getTabs : () -> Cmd msg


port openTab : TabId -> Cmd msg


port deleteTabs : List TabId -> Cmd msg


port moveTabs : MoveTabsArgs -> Cmd msg


port gotTabs : (String -> msg) -> Sub msg


port updatedBrowser : (String -> msg) -> Sub msg
