module Tests exposing (..)

import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (..)
import Test exposing (..)



-- HELPER


genTabInfo : WindowId -> TabId -> TabInfo
genTabInfo wid tid =
    let
        stid =
            String.fromInt tid

        title =
            "tab" ++ stid

        url =
            "https://host" ++ stid

        favicon =
            "https://favicon" ++ stid
    in
    TabInfo tid title url favicon wid (TabStatus True False)


genTabInfoList : WindowId -> Int -> Int -> List TabInfo
genTabInfoList wid tid count =
    let
        from =
            tid

        to =
            tid + count - 1
    in
    List.range from to
        |> List.foldr (\x acc -> genTabInfo wid x :: acc) []


genWindowInfo : WindowId -> List TabInfo -> WindowInfo
genWindowInfo windowId tabInfos =
    let
        name =
            "window - " ++ String.fromInt windowId

        status =
            WindowStatus True False
    in
    WindowInfo windowId name tabInfos status


getWindowInfo windowId windows =
    Dict.get windowId windows


getTabInfo : WindowId -> TabId -> WindowMap -> Maybe TabInfo
getTabInfo windowId tabId windows =
    let
        get : WindowInfo -> Maybe TabInfo
        get x =
            let
                tabInfo =
                    List.filter (\y -> y.id == tabId) x.tabInfos
            in
            case tabInfo of
                [] ->
                    Nothing

                first :: rest ->
                    Just first
    in
    Dict.get windowId windows
        |> Maybe.andThen get



-- SUITE


mainSuite : Test
mainSuite =
    describe "Main"
        [ test "convertFromJson" <|
            \_ ->
                let
                    data =
                        """
[{ "id": 1, "title": "tab1", "url": "http://host1", "favicon": "http://favicon1", "windowId": 10 }]
"""

                    expected =
                        Ok [ RawTab 1 "tab1" "http://host1" "http://favicon1" 10 ]
                in
                Expect.equal expected (convertFromJson data)
        , test "convertToMap" <|
            \_ ->
                let
                    data =
                        [ RawTab 1 "tab1" "https://host1" "https://favicon1" 10, RawTab 2 "tab2" "https://host2" "https://favicon2" 10 ]

                    tabInfos =
                        genTabInfoList 10 1 2

                    expected : WindowMap
                    expected =
                        Dict.empty
                            |> Dict.insert 10 (genWindowInfo 10 tabInfos)
                in
                Expect.equal expected (convertToMap data)
        , test "updateWindowInfo" <|
            \_ ->
                let
                    data : WindowMap
                    data =
                        Dict.empty
                            |> Dict.insert 10 (genWindowInfo 10 (genTabInfoList 10 1 3))

                    setter : WindowInfo -> WindowInfo
                    setter x =
                        { x | name = "NewName" }

                    actual =
                        updateWindowInfo 10 setter data
                            |> getWindowInfo 10
                            |> Maybe.map (\x -> x.name)

                    expected =
                        Just "NewName"
                in
                Expect.equal expected actual
        , test "checkTab - On" <|
            \_ ->
                let
                    data : WindowMap
                    data =
                        Dict.empty
                            |> Dict.insert 10 (genWindowInfo 10 (genTabInfoList 10 1 5))

                    actual =
                        checkTab 10 (\ti -> ti.id == 2) True data
                            |> checkTab 10 (\ti -> ti.id == 4) True
                            |> getWindowInfo 10
                            |> Maybe.map (\x -> x.tabInfos)
                            |> Maybe.withDefault []
                            |> List.filter (\x -> x.status.checked)
                            |> List.length

                    expected =
                        2
                in
                Expect.equal expected actual
        , test "checkTab - On -> Off" <|
            \_ ->
                let
                    data : WindowMap
                    data =
                        Dict.empty
                            |> Dict.insert 10 (genWindowInfo 10 (genTabInfoList 10 1 5))

                    cond ti =
                        ti.id == 2

                    actual =
                        checkTab 10 cond True data
                            |> checkTab 10 cond False
                            |> getWindowInfo 10
                            |> Maybe.map (\x -> x.tabInfos)
                            |> Maybe.withDefault []
                            |> List.filter (\x -> x.status.checked)
                            |> List.length

                    expected =
                        0
                in
                Expect.equal expected actual
        , test "splitQuery" <|
            \_ ->
                let
                    query =
                        "  q1 Q2  q3\u{3000}Q4    "

                    actual =
                        splitQuery query

                    expected =
                        [ "q1", "q2", "q3", "q4" ]
                in
                Expect.equal expected actual
        , test "matchTab" <|
            \_ ->
                let
                    q =
                        [ "title" ]

                    ti =
                        TabInfo 1 "Tab Title" "xxx" "xxx" 1 (TabStatus False False)

                    actual =
                        matchTab q ti
                in
                Expect.equal True actual
        ]


migrateStatusSuite : Test
migrateStatusSuite =
    let
        caller : String -> TabInfo -> Maybe TabInfo -> TabInfo -> Test
        caller desc new current expected =
            test desc <|
                \_ ->
                    migrateStatus new current
                        |> Expect.equal expected
    in
    describe "migrateStatus"
        [ caller
            "exists"
            (TabInfo 1 "Tab Title" "xxx" "xxx" 1 (TabStatus False False))
            (Just (TabInfo 1 "Tab Title" "xxx" "xxx" 1 (TabStatus True True)))
            (TabInfo 1 "Tab Title" "xxx" "xxx" 1 (TabStatus True True))
        , caller
            "different windowId"
            (TabInfo 1 "Tab Title" "xxx" "xxx" 2 (TabStatus False False))
            (Just (TabInfo 1 "Tab Title" "xxx" "xxx" 1 (TabStatus True True)))
            (TabInfo 1 "Tab Title" "xxx" "xxx" 2 (TabStatus True False))
        , caller
            "not exists 1"
            (TabInfo 1 "Tab Title" "xxx" "xxx" 1 (TabStatus False False))
            Nothing
            (TabInfo 1 "Tab Title" "xxx" "xxx" 1 (TabStatus False False))
        , caller
            "not exists 2"
            (TabInfo 1 "Tab Title" "xxx" "xxx" 1 (TabStatus True True))
            Nothing
            (TabInfo 1 "Tab Title" "xxx" "xxx" 1 (TabStatus True True))
        ]


applySearchSuite : Test
applySearchSuite =
    let
        caller : String -> Bool -> List String -> TabInfo -> TabStatus -> Test
        caller desc searchMode queries ti expected =
            test desc <|
                \_ ->
                    applySearch searchMode queries ti
                        |> Expect.equal { ti | status = expected }
    in
    describe "applySearch"
        [ caller
            "searchMode:on  => match"
            True
            [ "title" ]
            (TabInfo 1 "Tab Title" "xxx" "xxx" 1 (TabStatus False False))
            (TabStatus True False)
        , caller
            "searchMode:on  => unmatch"
            True
            [ "not-match" ]
            (TabInfo 1 "Tab Title" "xxx" "xxx" 1 (TabStatus True True))
            (TabStatus False True)
        , caller
            "searchMode:off => match"
            False
            [ "title" ]
            (TabInfo 1 "Tab Title" "xxx" "xxx" 1 (TabStatus False True))
            (TabStatus False True)
        , caller
            "searchMode:off => unmatch"
            False
            [ "not-match" ]
            (TabInfo 1 "Tab Title" "xxx" "xxx" 1 (TabStatus True False))
            (TabStatus True False)
        ]
