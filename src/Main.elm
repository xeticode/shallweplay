module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Colors
    exposing
        ( c_base
        , c_black
        , c_border
        , c_dark
        , c_darker
        , c_darkest
        , c_good
        , c_info
        , c_light
        , c_lighter
        , c_lightest
        , c_random
        , c_urgent
        , c_warn
        )
import DataModel
    exposing
        ( Event
        , EventId
        , EventStatus(..)
        , Events
        , InviteStatus(..)
        , Invites
        , LoginField(..)
        , Model
        , Password
        , Player
        , PlayerPosition
        , PossibleEvent
        , PossibleEventSetter(..)
        , UserName
        , amITheOrganizer
        , clearPossibleEventInfo
        , defaultMillisForPossibleEvent
        , defaultModel
        , emptyPlayerPosition
        , eventsByPlayer
        , eventsForPlayer
        , isMaybeVec2WithinMaybeDomElement
        , newInvite
        , newPlayer
        , newPossibleEvent
        , nextInviteStatusForStatus
        , playerEvents
        , primePossibleEventWithMillis
        , saveAndClearPossibleEvent
        , toggleEventStatusById
        , toggleInviteStatusByEventId
        )
import Dict
import Draggable as D
import Draggable.Events as DE
import Element as E
import Element.Background as EBA
import Element.Border as EBO
import Element.Font as EF
import Element.Input as EI
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as Decode
import Math.Vector2 as M
import Task
import Time as T



--
-- Main and Init
--


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( defaultModel
    , focusUserNameField
    )


playerDragConfig : D.Config UserName Msg
playerDragConfig =
    D.customConfig
        [ DE.onDragStart StartedDraggingPlayer
        , DE.onDragBy
            (\( dx, dy ) ->
                M.vec2 dx dy |> DraggingPlayer
            )
        , DE.onDragEnd EndedDraggingPlayer
        ]



--
-- Subscriptions and Commands
--


subscriptions : Model -> Sub Msg
subscriptions { drag } =
    D.subscriptions PlayerDragMsg drag


blurUserNameField : Cmd Msg
blurUserNameField =
    Task.attempt (\_ -> NoOp "blurUserNameField") (Dom.blur (fieldIdForLoginField UserNameField))


focusFieldWithId : String -> Cmd Msg
focusFieldWithId id =
    Task.attempt (\_ -> NoOp <| "focus field with id " ++ id) (Dom.focus id)


focusUserNameField : Cmd Msg
focusUserNameField =
    focusFieldWithId (fieldIdForLoginField UserNameField)



--
-- Msg and Update
--


type Msg
    = NoOp String
    | ChangedUserName UserName
    | ChangedPassword Password
    | EnteredUserName
    | EnteredPassword
    | LoggedOut
    | PrimingPossibleEvent T.Posix
    | ChangedPossibleEventProp PossibleEventSetter String
    | ToggledEventStatus EventId
    | ToggledInviteStatus String EventId
    | PlayerDragMsg (D.Msg UserName)
    | StartedDraggingPlayer UserName
    | ReceivedPlayerElementInfo (Result Dom.Error ( Dom.Element, Dom.Element ))
    | DraggingPlayer M.Vec2
    | EndedDraggingPlayer
    | RemovePossiblyInvited UserName
    | DismissPossibleEventWithSave Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp _ ->
            ( model, Cmd.none )

        ChangedUserName user_name ->
            ( updateModelFromChangedUserName model user_name
            , Cmd.none
            )

        ChangedPassword password ->
            ( updateModelFromChangedPassword model password
            , Cmd.none
            )

        EnteredUserName ->
            ( updateModelFromEnteredUserName model
            , commandFromEnteredUserName model.input_user_name
            )

        EnteredPassword ->
            ( updateModelFromEnteredPassword model
            , Cmd.none
            )

        LoggedOut ->
            ( updateModelFromLoggingOut model
            , focusUserNameField
            )

        PrimingPossibleEvent posix ->
            ( updateModelFromPrimingPossibleEvent model posix
            , Cmd.none
            )

        ChangedPossibleEventProp setter val ->
            ( updateModelFromChangedPossibleEventProp model setter val
            , Cmd.none
            )

        ToggledEventStatus event_id ->
            ( updateModelFromToggledEventStatus model event_id
            , Cmd.none
            )

        ToggledInviteStatus user_name event_id ->
            ( updateModelFromToggledInviteStatus model user_name event_id
            , Cmd.none
            )

        PlayerDragMsg drag_msg ->
            D.update playerDragConfig drag_msg model

        StartedDraggingPlayer user_name ->
            let
                drag_commands =
                    Task.attempt ReceivedPlayerElementInfo <|
                        Task.map2 (\pe_info p_info -> ( pe_info, p_info )) (Dom.getElement possibleEventCardId) (Dom.getElement user_name)
            in
            ( updateModelFromStartedDraggingPlayer model user_name
            , Cmd.batch [ Task.perform PrimingPossibleEvent T.now, drag_commands ]
            )

        ReceivedPlayerElementInfo result ->
            ( updateModelFromReceivedPlayerElementInfo model result
            , Cmd.none
            )

        DraggingPlayer vec2 ->
            ( updateModelFromDraggingPlayer model vec2
            , Cmd.none
            )

        EndedDraggingPlayer ->
            ( updateModelFromEndedDraggingPlayer model, Cmd.none )

        RemovePossiblyInvited user_name ->
            ( updateModelFromRemovePossiblyInvited model user_name, Cmd.none )

        DismissPossibleEventWithSave save_or_no ->
            ( updateModelFromDismissPossibleEventWithSave model save_or_no, Cmd.none )


{-| One advantage of breaking out these functions from the
Update function proper is it makes Update behavior easy to test
-}
updateModelFromChangedUserName : Model -> UserName -> Model
updateModelFromChangedUserName model user_name =
    { model
        | input_user_name = user_name
    }


updateModelFromChangedPassword : Model -> Password -> Model
updateModelFromChangedPassword model password =
    { model
        | input_password = password
    }


updateModelFromEnteredUserName : Model -> Model
updateModelFromEnteredUserName model =
    let
        cleaned_user_name =
            cleanedLoginString model.input_user_name

        m_player =
            Dict.get cleaned_user_name model.players
    in
    if isUserNameValid cleaned_user_name then
        let
            offset_and_cleared_model =
                { model
                    | login_offset = 804.0
                    , login_field_with_tab_index = NoField
                }
                    |> clearPossibleEventInfo
        in
        case m_player of
            Nothing ->
                let
                    new_player =
                        newPlayer cleaned_user_name
                in
                { offset_and_cleared_model
                    | m_player = Just new_player
                    , players = Dict.insert cleaned_user_name new_player model.players
                }

            _ ->
                { offset_and_cleared_model
                    | m_player = m_player
                }

    else
        model


commandFromEnteredUserName : UserName -> Cmd Msg
commandFromEnteredUserName user_name =
    let
        cleaned_user_name =
            cleanedLoginString user_name
    in
    if isUserNameValid cleaned_user_name then
        blurUserNameField

    else
        Cmd.none


updateModelFromEnteredPassword : Model -> Model
updateModelFromEnteredPassword model =
    let
        cleaned_password =
            cleanedLoginString model.input_password
    in
    if isPasswordValid cleaned_password then
        { model
            | login_offset = 804.0
            , login_field_with_tab_index = NoField
        }

    else
        model


updateModelFromLoggingOut : Model -> Model
updateModelFromLoggingOut model =
    { model
        | login_offset = 0.0
        , m_player = Nothing
        , input_user_name = ""
        , login_field_with_tab_index = UserNameField
    }


updateModelFromPrimingPossibleEvent : Model -> T.Posix -> Model
updateModelFromPrimingPossibleEvent ({ m_possible_event } as model) posix =
    let
        millis =
            T.posixToMillis posix
    in
    { model
        | m_possible_event =
            Just
                (case m_possible_event of
                    Nothing ->
                        newPossibleEvent millis

                    Just poss_event ->
                        primePossibleEventWithMillis millis poss_event
                )
    }


updateModelFromChangedPossibleEventProp : Model -> PossibleEventSetter -> String -> Model
updateModelFromChangedPossibleEventProp model setter val =
    let
        toMillis : String -> Int
        toMillis int_string =
            String.toInt int_string
                |> Maybe.withDefault 0

        m_poss_event =
            Maybe.map
                (\pe ->
                    case setter of
                        MillisUp ->
                            updatePossibleEventMillis Increment (toMillis val) pe

                        MillisDown ->
                            updatePossibleEventMillis Decrement (toMillis val) pe

                        Title ->
                            updatePossibleEventTitle val pe

                        Venue ->
                            updatePossibleEventVenue val pe

                        Description ->
                            updatePossibleEventDescription val pe
                )
                model.m_possible_event
    in
    { model | m_possible_event = m_poss_event }


updateModelFromToggledEventStatus : Model -> EventId -> Model
updateModelFromToggledEventStatus model event_id =
    { model | events = toggleEventStatusById event_id model.events }


updateModelFromToggledInviteStatus : Model -> UserName -> EventId -> Model
updateModelFromToggledInviteStatus model user_name event_id =
    { model | events = toggleInviteStatusByEventId user_name event_id model.events }


updateModelFromStartedDraggingPlayer : Model -> UserName -> Model
updateModelFromStartedDraggingPlayer model user_name =
    let
        dragged_model =
            { model | dragged_player_position = ( Just user_name, Nothing ) }
    in
    if Nothing == model.m_possible_event then
        { dragged_model
            | m_possible_event = Just (newPossibleEvent defaultMillisForPossibleEvent)
        }

    else
        dragged_model


updateModelFromReceivedPlayerElementInfo : Model -> Result Dom.Error ( Dom.Element, Dom.Element ) -> Model
updateModelFromReceivedPlayerElementInfo model result =
    case result of
        Ok elems_info ->
            let
                ( m_user_name, _ ) =
                    model.dragged_player_position

                ( poss_event_info, player_token_info ) =
                    elems_info

                new_vec =
                    M.vec2
                        (player_token_info.element.x - poss_event_info.element.x)
                        (player_token_info.element.y - poss_event_info.element.y)
            in
            { model
                | dragged_player_position = ( m_user_name, Just new_vec )
                , m_poss_event_elem_info = Just poss_event_info
            }

        Err _ ->
            model


updateModelFromDraggingPlayer : Model -> M.Vec2 -> Model
updateModelFromDraggingPlayer model delta_vec2 =
    let
        ( m_user_name, m_vec2 ) =
            model.dragged_player_position

        m_new_vec2 =
            Maybe.map (\vec2 -> M.add delta_vec2 vec2) m_vec2
    in
    { model | dragged_player_position = ( m_user_name, m_new_vec2 ) }


updateModelFromEndedDraggingPlayer : Model -> Model
updateModelFromEndedDraggingPlayer ({ dragged_player_position, m_poss_event_elem_info, m_possible_event } as model) =
    let
        nm_possible_event =
            if isMaybeVec2WithinMaybeDomElement (Tuple.second dragged_player_position) m_poss_event_elem_info then
                dragged_player_position
                    |> Tuple.first
                    |> Maybe.andThen (\un -> Just (newInvite un))
                    |> Maybe.map2 (\pe ni -> { pe | invites = Dict.insert ni.for_user_name ni pe.invites }) m_possible_event

            else
                m_possible_event
    in
    { model
        | dragged_player_position = emptyPlayerPosition
        , m_possible_event = nm_possible_event
    }


updateModelFromRemovePossiblyInvited : Model -> UserName -> Model
updateModelFromRemovePossiblyInvited ({ m_possible_event } as model) user_name =
    { model | m_possible_event = Maybe.andThen (\pe -> Just { pe | invites = Dict.remove user_name pe.invites }) m_possible_event }


updateModelFromDismissPossibleEventWithSave : Model -> Bool -> Model
updateModelFromDismissPossibleEventWithSave model save_or_no =
    if save_or_no then
        saveAndClearPossibleEvent model

    else
        { model | m_possible_event = Nothing }



--
-- Views
--


mainViewWidth : E.Length
mainViewWidth =
    E.px 900


view : Model -> Browser.Document Msg
view model =
    { title = "Shall We Play"
    , body =
        [ viewBody model ]
    }


viewBody : Model -> Html Msg
viewBody model =
    E.layout
        [ EBA.color c_border
        , EF.size 16
        , EF.family
            [ EF.typeface "Ubuntu Mono"
            , EF.monospace
            ]
        , EF.color c_darkest
        ]
        (E.column
            [ E.width E.fill
            , E.padding 20
            ]
            [ topView model
            , mainView model
            ]
        )



-- Top


topView : Model -> E.Element Msg
topView model =
    E.row
        [ EBO.roundEach { topLeft = 15, topRight = 0, bottomLeft = 0, bottomRight = 0 }
        , EBO.widthEach { bottom = 0, left = 3, right = 0, top = 3 }
        , EBO.color c_light
        , EBA.color c_darker
        , E.width mainViewWidth
        , E.clip
        , E.centerX
        , E.spacing 20
        ]
        ([]
            |> (::) shallWePlayView
            |> (::) (loginLogoutView model)
            |> maybeUpdateViewList (youHaveView model.events model.m_player)
            |> List.reverse
        )


shallWePlayView : E.Element Msg
shallWePlayView =
    E.textColumn
        [ EBA.color c_lightest
        , EF.size 20
        , E.width (E.px 80)
        , E.padding 10
        , E.spacing 5
        ]
        [ E.paragraph [] [ E.text "Shall" ]
        , E.paragraph []
            [ E.image [ E.alignLeft, E.width (E.px 24) ]
                { src = "assets/images/noun_Dice_1145792.svg", description = "Die" }
            , E.text "We"
            ]
        , E.paragraph [] [ E.text "Play?" ]
        ]


youHaveView : Events -> Maybe Player -> Maybe (E.Element Msg)
youHaveView events m_player =
    Maybe.map
        (\player ->
            E.column
                [ E.width E.fill
                , E.spacing 8
                ]
                [ youHaveEventsView events player
                , youHaveInvitesView events player
                ]
        )
        m_player


youHaveEventsView : Events -> Player -> E.Element Msg
youHaveEventsView events player =
    let
        player_events =
            eventsByPlayer (Dict.toList events) player
    in
    E.el
        [ E.width E.fill
        , EF.center
        , EF.bold
        , EF.color c_lighter
        ]
        (E.text <| "You have organized " ++ String.fromInt (List.length player_events) ++ " event(s)")


youHaveInvitesView : Events -> Player -> E.Element Msg
youHaveInvitesView events player =
    let
        player_events =
            eventsForPlayer (Dict.toList events) player
    in
    E.el
        [ E.width E.fill
        , EF.center
        , EF.bold
        , EF.color c_lighter
        ]
        (E.text <| "You are invited to " ++ String.fromInt (List.length player_events) ++ " event(s)")



-- Login and Logout


loginLogoutRowViewWidth : E.Length
loginLogoutRowViewWidth =
    E.px 382


loginLogoutView : Model -> E.Element Msg
loginLogoutView model =
    E.el
        [ EBO.rounded 7
        , EBA.color c_lightest
        , E.clip
        , E.width (E.px 400)
        , E.paddingXY 10 5
        ]
        (loginLogoutRowView model)


loginLogoutRowView : Model -> E.Element Msg
loginLogoutRowView model =
    E.row
        [ E.width E.fill
        , E.spacing 20
        , E.moveLeft model.login_offset
        ]
        [ loginView model.input_user_name model.login_field_with_tab_index
        , passwordView model.input_password model.login_field_with_tab_index
        , logoutView model.m_player
        ]


loginView : UserName -> LoginField -> E.Element Msg
loginView input_user_name field =
    E.row
        [ E.width loginLogoutRowViewWidth ]
        [ EI.username
            [ E.focused [ EBA.color c_black ]
            , EBO.width 0
            , EF.color c_lightest
            , EBA.color c_border
            , onEnter EnteredUserName
            , E.htmlAttribute <| HA.id (fieldIdForLoginField UserNameField)
            , E.htmlAttribute <| HA.tabindex (tabIndexFromLoginField field UserNameField)
            ]
            { onChange = ChangedUserName
            , text = input_user_name
            , placeholder = Nothing
            , label = EI.labelLeft [ EF.bold, E.centerY ] (E.text "User Name")
            }
        , EI.button
            [ E.paddingEach { top = 0, right = 0, bottom = 0, left = 5 }
            , E.focused []
            , E.htmlAttribute <| HA.tabindex -1
            ]
            { onPress = Just EnteredUserName
            , label =
                E.image
                    [ E.width (E.px 26)
                    ]
                    { src = "assets/images/noun_Login_2185437.svg", description = "To Password" }
            }
        ]


passwordView : Password -> LoginField -> E.Element Msg
passwordView password field =
    E.row
        [ E.width loginLogoutRowViewWidth ]
        [ EI.currentPassword
            [ E.focused [ EBA.color c_black ]
            , EBO.width 0
            , EF.color c_lightest
            , EBA.color c_border
            , onEnter EnteredPassword
            , E.htmlAttribute <| HA.id (fieldIdForLoginField PasswordField)
            , E.htmlAttribute <| HA.tabindex (tabIndexFromLoginField field PasswordField)
            ]
            { onChange = ChangedPassword
            , text = password
            , placeholder = Nothing
            , show = False
            , label = EI.labelLeft [ EF.bold, E.centerY ] (E.text "Password")
            }
        , EI.button
            [ E.paddingEach { top = 0, right = 0, bottom = 0, left = 5 }
            , E.focused []
            , E.htmlAttribute <| HA.tabindex -1
            ]
            { onPress = Just EnteredPassword
            , label =
                E.image
                    [ E.width (E.px 26)
                    ]
                    { src = "assets/images/noun_Login_2185437.svg", description = "Login" }
            }
        ]


logoutView : Maybe Player -> E.Element Msg
logoutView m_player =
    let
        user_name =
            .user_name (Maybe.withDefault (newPlayer "") m_player)
    in
    E.row
        [ E.width loginLogoutRowViewWidth, E.spacing 5 ]
        [ E.el [ EF.bold ] (E.text "User Name")
        , E.el
            [ EF.color c_lighter
            , EBA.color c_dark
            , E.width E.fill
            , E.paddingXY 10 9
            , EBO.rounded 3
            ]
            (playerTokenView <| LoggedIn user_name)
        , EI.button
            [ E.paddingEach { top = 0, right = 0, bottom = 0, left = 0 }
            , E.focused []
            , E.htmlAttribute <| HA.tabindex -1
            ]
            { onPress = Just LoggedOut
            , label =
                E.image
                    [ E.width (E.px 26)
                    ]
                    { src = "assets/images/noun_Login_2185481.svg", description = "Logout" }
            }
        ]



-- Main


mainView : Model -> E.Element Msg
mainView model =
    case model.m_player of
        Nothing ->
            plainMainView

        Just player ->
            eventsMainView model player


plainMainView : E.Element Msg
plainMainView =
    E.textColumn
        [ EBO.widthEach { bottom = 0, left = 3, right = 0, top = 0 }
        , EBO.color c_light
        , EBA.color c_base
        , EF.color c_lightest
        , E.width mainViewWidth
        , E.centerX
        , E.padding 10
        , E.spacing 10
        ]
        [ disclaimerParagraph
        , E.paragraph
            []
            [ E.text "To use this site, please specify a user name and log in. If that account does not exist, it will be created."
            ]
        ]


eventsMainView : Model -> Player -> E.Element Msg
eventsMainView model me =
    E.column
        [ EBO.widthEach { bottom = 0, left = 3, right = 0, top = 0 }
        , EBO.color c_light
        , EBA.color c_base
        , EF.color c_lightest
        , E.width mainViewWidth
        , E.centerX
        ]
        [ playersView model me
        , eventCardsView model.events me
        , footerView
        ]



-- Players


maybeDraggedPlayerView : PlayerPosition -> Maybe (E.Attribute Msg)
maybeDraggedPlayerView ( m_user_name, m_vec2 ) =
    Maybe.map2
        (\user_name vec2 ->
            E.inFront <|
                E.el
                    [ E.moveRight (M.getX vec2)
                    , E.moveDown (M.getY vec2)
                    ]
                    (playerTokenView (Dragged user_name))
        )
        m_user_name
        m_vec2


playersView : Model -> Player -> E.Element Msg
playersView model me =
    E.column
        ([]
            |> (::) (E.padding 10)
            |> (::) (E.width E.fill)
            |> maybePossibleEventCardView model
        )
        [ playersViewHeader
        , E.wrappedRow
            [ E.spacing 10
            , E.paddingXY 5 5
            , E.width E.fill
            , EBA.color c_dark
            ]
            (model.players
                |> Dict.toList
                |> List.filter (\( u, _ ) -> u /= me.user_name)
                |> List.map
                    (\( _, p ) -> playerTokenView (Listed p.user_name))
            )
        ]


playersViewHeader : E.Element Msg
playersViewHeader =
    E.row
        [ E.width E.fill
        , E.paddingEach { top = 0, right = 0, bottom = 10, left = 0 }
        ]
        [ E.el
            [ EF.extraBold
            ]
            (E.text "All Other Players")
        , E.el
            [ EF.color c_lighter
            , E.alignRight
            ]
            (E.text "Drag a player to start an event")
        ]


type PlayerToken
    = LoggedIn UserName
    | Listed UserName
    | Invited UserName InviteStatus
    | Dragged UserName
    | PossiblyInvited UserName


playerTokenView : PlayerToken -> E.Element Msg
playerTokenView state =
    case state of
        LoggedIn user_name ->
            E.el (playerTokenElementAttributes loggedInPlayerTokenAttributes) (E.text user_name)

        Listed user_name ->
            E.el (playerTokenElementAttributes (listedPlayerTokenAttributes user_name)) (E.text user_name)

        Invited user_name status ->
            E.el (playerTokenElementAttributes (invitedPlayerTokenAttributes status)) (E.text user_name)

        Dragged user_name ->
            E.el (playerTokenElementAttributes draggedPlayerTokenAttributes) (E.text user_name)

        PossiblyInvited user_name ->
            E.el
                (playerTokenElementAttributes possiblyInvitedTokenAttributes)
                (E.row
                    [ E.spacing 5 ]
                    [ E.text user_name
                    , EI.button [ E.focused [] ]
                        { onPress = Just (RemovePossiblyInvited user_name)
                        , label =
                            E.image [ E.width (E.px 9) ]
                                { src = "assets/images/noun_x_2147847.svg"
                                , description = "remove"
                                }
                        }
                    ]
                )


type alias PlayerTokenAttributes =
    { background_color : E.Color
    , text_color : E.Color
    , m_id : Maybe String
    , show_pointer : Bool
    }


playerTokenAttributesDefaults : PlayerTokenAttributes
playerTokenAttributesDefaults =
    { background_color = c_darkest
    , text_color = c_lightest
    , m_id = Nothing
    , show_pointer = False
    }


withBackgroundColor : E.Color -> PlayerTokenAttributes -> PlayerTokenAttributes
withBackgroundColor color attributes =
    { attributes | background_color = color }


withTextColor : E.Color -> PlayerTokenAttributes -> PlayerTokenAttributes
withTextColor color attributes =
    { attributes | text_color = color }


withMaybeId : Maybe String -> PlayerTokenAttributes -> PlayerTokenAttributes
withMaybeId m_id attributes =
    { attributes | m_id = m_id }


withShowPointer : Bool -> PlayerTokenAttributes -> PlayerTokenAttributes
withShowPointer show_pointer attributes =
    { attributes | show_pointer = show_pointer }


loggedInPlayerTokenAttributes : PlayerTokenAttributes
loggedInPlayerTokenAttributes =
    playerTokenAttributesDefaults


listedPlayerTokenAttributes : UserName -> PlayerTokenAttributes
listedPlayerTokenAttributes user_name =
    playerTokenAttributesDefaults
        |> withMaybeId (Just user_name)
        |> withShowPointer True


invitedPlayerTokenAttributes : InviteStatus -> PlayerTokenAttributes
invitedPlayerTokenAttributes status =
    case status of
        Pending ->
            playerTokenAttributesDefaults
                |> withBackgroundColor c_warn
                |> withTextColor c_darkest

        Accepted ->
            playerTokenAttributesDefaults
                |> withBackgroundColor c_good
                |> withTextColor c_darkest

        Declined ->
            playerTokenAttributesDefaults
                |> withBackgroundColor c_urgent
                |> withTextColor c_darkest


draggedPlayerTokenAttributes : PlayerTokenAttributes
draggedPlayerTokenAttributes =
    playerTokenAttributesDefaults
        |> withShowPointer True


possiblyInvitedTokenAttributes : PlayerTokenAttributes
possiblyInvitedTokenAttributes =
    playerTokenAttributesDefaults


playerTokenElementAttributes : PlayerTokenAttributes -> List (E.Attribute Msg)
playerTokenElementAttributes attributes =
    [ EBO.rounded 50
    , E.paddingXY 10 3
    ]
        |> (::) (EBA.color attributes.background_color)
        |> (::) (EF.color attributes.text_color)
        |> (\attrs ->
                case attributes.m_id of
                    Just user_name ->
                        attrs
                            |> (::) (E.htmlAttribute (HA.id user_name))
                            |> (::) E.pointer
                            |> (++)
                                (D.mouseTrigger user_name PlayerDragMsg
                                    :: D.touchTriggers user_name PlayerDragMsg
                                    |> List.map E.htmlAttribute
                                )

                    Nothing ->
                        attrs
           )
        |> (\attrs ->
                if attributes.show_pointer && Nothing == attributes.m_id then
                    E.pointer :: attrs

                else
                    attrs
           )



-- Event Cards


eventCardsView : Events -> Player -> E.Element Msg
eventCardsView events me =
    E.column
        [ E.padding 10
        , E.width E.fill
        ]
        [ E.el
            [ EF.extraBold
            , E.paddingEach { top = 0, right = 0, bottom = 15, left = 0 }
            ]
            (E.text "Events")
        , E.wrappedRow [ E.spacing 20 ]
            (List.map (\e -> eventCardView e me) (playerEvents events me))
        ]


eventCardView : Event -> Player -> E.Element Msg
eventCardView event me =
    E.el
        [ E.padding 5
        , E.width E.fill
        , E.alignTop
        , EBO.widthEach { top = 2, right = 0, bottom = 0, left = 2 }
        , EBO.roundEach { topLeft = 7, bottomLeft = 0, topRight = 0, bottomRight = 0 }
        , EBO.color c_light
        , EBA.color c_dark
        ]
        (E.column
            [ E.spacing 10, E.width E.fill ]
            ([]
                |> (::) (cardHeader event me)
                |> (::) (cardSection "Date" (E.el [] (E.text (utcDateString (T.millisToPosix event.millis)))))
                |> (::) (cardSection "Description" (E.paragraph [] [ E.text event.description ]))
                |> (::) (cardSection "Venue" (E.paragraph [] [ E.text event.venue ]))
                |> (::) (inviteesSection event me)
                |> maybeUpdateViewList (maybeGoingSection event me)
                |> maybeUpdateViewList (maybeActionButton event me)
                |> List.reverse
            )
        )


cardHeader : Event -> Player -> E.Element Msg
cardHeader ({ title, status, organizer_user_name } as event) me =
    let
        status_elem =
            case status of
                Scheduled ->
                    E.el [ E.alignRight, EF.color c_info ] (E.text (eventStatusToHeaderString status))

                Canceled ->
                    E.el [ E.alignRight, EF.color c_random ] (E.text (eventStatusToHeaderString status))

        organizer_text =
            if amITheOrganizer event me then
                "You"

            else
                organizer_user_name
    in
    E.column
        [ E.width E.fill
        , EBA.color c_darker
        ]
        [ E.row
            [ E.width E.fill
            , E.spacing 20
            , E.paddingEach { top = 5, right = 5, bottom = 2, left = 5 }
            ]
            [ E.el [ EF.bold ] (E.text title)
            , status_elem
            ]
        , E.el
            [ E.paddingEach
                { top = 0
                , right = 5
                , bottom = 1
                , left = 3
                }
            , EF.italic
            , EF.color c_lighter
            ]
            (E.text ("Organized by " ++ organizer_text))
        ]


cardSection : String -> E.Element Msg -> E.Element Msg
cardSection label elem =
    E.column []
        [ E.el [ EF.color c_lighter, E.paddingXY 0 2 ] (E.text label)
        , elem
        ]


inviteesSection : Event -> Player -> E.Element Msg
inviteesSection event me =
    cardSection
        (if amITheOrganizer event me then
            "Invitees"

         else
            "Other Invitees"
        )
        (E.wrappedRow
            [ E.spacing 10
            , E.paddingXY 0 5
            ]
            (List.filterMap
                (\( _, invite ) ->
                    if me.user_name /= invite.for_user_name then
                        Just (playerTokenView <| Invited invite.for_user_name invite.status)

                    else
                        Nothing
                )
                (Dict.toList event.invites)
            )
        )


maybeGoingSection : Event -> Player -> Maybe (E.Element Msg)
maybeGoingSection event me =
    if amITheOrganizer event me then
        Nothing

    else
        let
            going_descriptor =
                if Canceled == event.status then
                    goingAnswerDescriptorForInviteStatus Declined

                else
                    case Dict.get me.user_name event.invites of
                        Just invite ->
                            goingAnswerDescriptorForInviteStatus invite.status

                        Nothing ->
                            goingAnswerDescriptorForInviteStatus Declined
        in
        Just
            (cardSection "Going?"
                (E.el
                    [ E.paddingXY 2 0
                    , EBA.color going_descriptor.background_color
                    , EF.color going_descriptor.text_color
                    ]
                    (E.text going_descriptor.text)
                )
            )


{-| We want the going answer to be styled the same way as the token
-}
type alias GoingAnswerDescriptor =
    { text : String
    , text_color : E.Color
    , background_color : E.Color
    }


goingAnswerDescriptorForInviteStatus : InviteStatus -> GoingAnswerDescriptor
goingAnswerDescriptorForInviteStatus status =
    let
        base_descriptor =
            invitedPlayerTokenAttributes status
    in
    { text = inviteStatusToGoingString status
    , text_color = base_descriptor.text_color
    , background_color = base_descriptor.background_color
    }


maybeActionButton : Event -> Player -> Maybe (E.Element Msg)
maybeActionButton event me =
    if amITheOrganizer event me then
        let
            label =
                E.text (eventStatusToButtonString event.status)
        in
        Just (cardButton label (ToggledEventStatus event.id))

    else if Canceled == event.status then
        Nothing

    else
        let
            label_text =
                case Dict.get me.user_name event.invites of
                    Just invite ->
                        nextInviteStatusForStatus invite.status
                            |> inviteStatusToButtonString

                    Nothing ->
                        inviteStatusToButtonString Declined

            label =
                E.el [ E.width E.fill, EF.center ] (E.text <| "Update to " ++ label_text)
        in
        Just (cardButton label (ToggledInviteStatus me.user_name event.id))


cardButton : E.Element Msg -> Msg -> E.Element Msg
cardButton label msg =
    EI.button
        [ EBA.color c_darker
        , EBO.width 1
        , EBO.color c_lightest
        , E.focused []
        , E.paddingEach { top = 2, right = 2, bottom = 3, left = 2 }
        ]
        { onPress = Just msg
        , label = label
        }


eventStatusToHeaderString : EventStatus -> String
eventStatusToHeaderString status =
    case status of
        Canceled ->
            "Canceled"

        Scheduled ->
            "Scheduled"


eventStatusToButtonString : EventStatus -> String
eventStatusToButtonString status =
    case status of
        Canceled ->
            "Reschedule This Event"

        Scheduled ->
            "Cancel This Event"


inviteStatusToGoingString : InviteStatus -> String
inviteStatusToGoingString status =
    case status of
        Pending ->
            "Maybe"

        Accepted ->
            "Yes"

        Declined ->
            "No"


inviteStatusToButtonString : InviteStatus -> String
inviteStatusToButtonString status =
    case status of
        Pending ->
            "I'm Not Sure"

        Accepted ->
            "I Am Going"

        Declined ->
            "I Am Not Going"



-- Possible Event Card


{-| We put the player position relative to this element so that we
are sure the dragged player will display in front of it
-}
possibleEventCardId : String
possibleEventCardId =
    "possible_event"


possibleEventCardView : PossibleEvent -> PlayerPosition -> Maybe Dom.Element -> E.Element Msg
possibleEventCardView possible_event (( _, m_vec2 ) as player_position) m_poss_elem_info =
    E.el
        ([ E.padding 5
         , E.width (E.px 400)
         , E.centerX
         , EBO.widthEach { top = 2, right = 0, bottom = 0, left = 2 }
         , EBO.roundEach { topLeft = 7, bottomLeft = 0, topRight = 0, bottomRight = 0 }
         , EBO.color c_light
         , EBA.color c_darker
         , E.htmlAttribute (HA.id possibleEventCardId)
         ]
            |> (\m_elem attrs ->
                    case m_elem of
                        Just elem ->
                            elem :: attrs

                        Nothing ->
                            attrs
               )
                (maybeDraggedPlayerView player_position)
            |> (\is_within attrs ->
                    if is_within then
                        EBO.glow c_lighter 17.0 :: attrs

                    else
                        EBO.shadow
                            { offset = ( 6, 6 )
                            , size = 0.0
                            , blur = 2.0
                            , color = c_lighter
                            }
                            :: attrs
               )
                (isMaybeVec2WithinMaybeDomElement m_vec2 m_poss_elem_info)
        )
        (E.column
            [ E.spacing 10, E.width E.fill ]
            [ possibleTitleView possible_event.title
            , possibleDateView possible_event.millis
            , possibleDescriptionView possible_event.description
            , possibleVenueView possible_event.venue
            , cardSection "Invitees" (possibleInviteesView possible_event.invites)
            , possibleSaveButtonsView
            ]
        )


possibleViewAttributes : List (E.Attribute Msg)
possibleViewAttributes =
    [ E.focused []
    , E.padding 5
    , EBO.width 0
    , EBA.color c_dark
    ]


possibleTitleView : String -> E.Element Msg
possibleTitleView title =
    EI.spellChecked
        possibleViewAttributes
        { onChange = ChangedPossibleEventProp Title
        , text = title
        , placeholder = Nothing
        , label = EI.labelAbove [ EF.color c_lighter ] (E.text "Title")
        }


possibleDateView : Int -> E.Element Msg
possibleDateView millis =
    possibleDateInputView millis


possibleDateInputView : Int -> E.Element Msg
possibleDateInputView millis =
    let
        pos =
            T.millisToPosix millis
    in
    E.row [ E.spacing 15 ]
        [ monthInputView (T.toYear T.utc pos) (T.toMonth T.utc pos)
        , dayInputView <| String.fromInt (T.toDay T.utc pos)
        , yearInputView <| String.fromInt (T.toYear T.utc pos)
        , E.el [ E.alignBottom, E.paddingXY 0 6 ] (E.text "@")
        , hourInputView <| String.fromInt (T.toHour T.utc pos)
        , minuteInputView <| String.fromInt (T.toMinute T.utc pos)
        , E.el [ E.alignBottom, E.paddingXY 0 6 ] (E.text "UTC")
        ]


monthInputView : Int -> T.Month -> E.Element Msg
monthInputView year month =
    cardSection "Month"
        (E.row
            []
            [ dateDisplayView (toMonthString month) 35
            , upDownButtonSet (1000 * 60 * 60 * 24 * daysForMonth year month)
                (Just (1000 * 60 * 60 * 24 * daysForPreviousMonth year month))
            ]
        )


dayInputView : String -> E.Element Msg
dayInputView value =
    cardSection "Day"
        (E.row
            []
            [ dateDisplayView value 25
            , upDownButtonSet (1000 * 60 * 60 * 24) Nothing
            ]
        )


yearInputView : String -> E.Element Msg
yearInputView value =
    cardSection "Year"
        (E.row
            []
            [ dateDisplayView value 40
            , upDownButtonSet (1000 * 60 * 60 * 24 * 365) Nothing
            ]
        )


hourInputView : String -> E.Element Msg
hourInputView value =
    cardSection "Hour"
        (E.row
            []
            [ dateDisplayView (String.padLeft 2 '0' value) 25
            , upDownButtonSet (1000 * 60 * 60) Nothing
            ]
        )


minuteInputView : String -> E.Element Msg
minuteInputView value =
    cardSection "Min"
        (E.row
            []
            [ dateDisplayView (String.padLeft 2 '0' value) 25
            , upDownButtonSet (1000 * 60) Nothing
            ]
        )


dateDisplayView : String -> Int -> E.Element Msg
dateDisplayView value width =
    E.el
        [ E.width (E.px width)
        , EBA.color c_dark
        , E.paddingEach { top = 5, right = 0, bottom = 5, left = 5 }
        , EBO.roundEach
            { topLeft = 3
            , topRight = 0
            , bottomRight = 0
            , bottomLeft = 3
            }
        ]
        (E.text value)


upDownButtonSet : Int -> Maybe Int -> E.Element Msg
upDownButtonSet change_up m_change_down =
    E.column
        [ EBA.color c_dark
        , EBO.roundEach
            { topLeft = 0
            , topRight = 3
            , bottomRight = 3
            , bottomLeft = 0
            }
        , E.alignBottom
        , E.spacing 1
        , E.paddingEach { top = 0, right = 3, bottom = 0, left = 0 }
        ]
        [ EI.button
            [ E.focused [], E.htmlAttribute <| HA.tabindex -1 ]
            { onPress =
                Just
                    (ChangedPossibleEventProp
                        MillisUp
                        (String.fromInt change_up)
                    )
            , label =
                E.image
                    [ E.width (E.px 12)
                    , E.height (E.px 13)
                    ]
                    { src = "assets/images/noun_arrow_1921080.svg", description = "up" }
            }
        , EI.button
            [ E.focused [], E.htmlAttribute <| HA.tabindex -1 ]
            { onPress =
                Just
                    (ChangedPossibleEventProp
                        MillisDown
                        (String.fromInt (Maybe.withDefault change_up m_change_down))
                    )
            , label =
                E.image
                    [ E.width (E.px 12)
                    , E.height (E.px 12)
                    , E.rotate (degrees 180)
                    ]
                    { src = "assets/images/noun_arrow_1921080.svg", description = "down" }
            }
        ]


possibleMultiLineView : String -> String -> PossibleEventSetter -> E.Element Msg
possibleMultiLineView label value setter =
    EI.multiline
        possibleViewAttributes
        { onChange = ChangedPossibleEventProp setter
        , text = value
        , placeholder = Nothing
        , label = EI.labelAbove [ EF.color c_lighter ] (E.text label)
        , spellcheck = True
        }


possibleDescriptionView : String -> E.Element Msg
possibleDescriptionView description =
    possibleMultiLineView "Description" description Description


possibleVenueView : String -> E.Element Msg
possibleVenueView venue =
    possibleMultiLineView "Venue" venue Venue


possibleInviteesView : Invites -> E.Element Msg
possibleInviteesView invites =
    E.wrappedRow
        [ E.spacing 10
        , E.paddingXY 0 5
        ]
        (List.map
            (\( _, invite ) ->
                playerTokenView <| PossiblyInvited invite.for_user_name
            )
            (Dict.toList invites)
        )


possibleSaveButtonsView : E.Element Msg
possibleSaveButtonsView =
    E.row [ E.spacing 5, E.alignRight ]
        [ cardButton (E.text "Cancel")
            (DismissPossibleEventWithSave False)
        , cardButton
            (E.text "Save")
            (DismissPossibleEventWithSave True)
        ]


maybePossibleEventCardView : Model -> List (E.Attribute Msg) -> List (E.Attribute Msg)
maybePossibleEventCardView { m_possible_event, dragged_player_position, m_poss_event_elem_info } attrs =
    case m_possible_event |> Maybe.andThen (\pe -> Just (possibleEventCardView pe dragged_player_position m_poss_event_elem_info)) of
        Just poss_card_view ->
            E.below poss_card_view :: attrs

        Nothing ->
            attrs



-- Footer


disclaimerParagraph : E.Element Msg
disclaimerParagraph =
    E.paragraph
        []
        [ E.text "This site is for demonstration purposes only. "
        , E.el [ EF.bold ] (E.text "ALL")
        , E.text " data on this site is public and freely accessible, "
        , E.el [ EF.bold ] (E.text "editable")
        , E.text ", and may disappear at any time."
        ]


footerView : E.Element Msg
footerView =
    E.textColumn
        [ E.width E.fill
        , E.padding 10
        , EBA.color c_base
        ]
        [ disclaimerParagraph ]



--
-- Helpers
--


type AdjustMillis
    = Increment
    | Decrement


updatePossibleEventMillis : AdjustMillis -> Int -> PossibleEvent -> PossibleEvent
updatePossibleEventMillis change_direction millis possible_event =
    let
        f_millis =
            case change_direction of
                Increment ->
                    possible_event.millis + millis

                Decrement ->
                    possible_event.millis - millis
    in
    { possible_event | millis = f_millis }


updatePossibleEventTitle : String -> PossibleEvent -> PossibleEvent
updatePossibleEventTitle title possible_event =
    { possible_event | title = title }


updatePossibleEventVenue : String -> PossibleEvent -> PossibleEvent
updatePossibleEventVenue venue possible_event =
    { possible_event | venue = venue }


updatePossibleEventDescription : String -> PossibleEvent -> PossibleEvent
updatePossibleEventDescription description possible_event =
    { possible_event | description = description }


cleanedLoginString : String -> String
cleanedLoginString login_string =
    String.trim login_string


isUserNameValid : UserName -> Bool
isUserNameValid user_name =
    0 < String.length user_name


isPasswordValid : Password -> Bool
isPasswordValid password =
    0 < String.length password


fieldIdForLoginField : LoginField -> String
fieldIdForLoginField field =
    case field of
        UserNameField ->
            "user_name"

        PasswordField ->
            "password"

        NoField ->
            "none"


tabIndexFromLoginField : LoginField -> LoginField -> Int
tabIndexFromLoginField tabbable_field current_field =
    if tabbable_field == current_field then
        0

    else
        -1


maybeUpdateViewList : Maybe (E.Element Msg) -> List (E.Element Msg) -> List (E.Element Msg)
maybeUpdateViewList m_element view_list =
    case m_element of
        Just element ->
            element :: view_list

        Nothing ->
            view_list


utcDateString : T.Posix -> String
utcDateString time =
    toWeekdayString (T.toWeekday T.utc time)
        ++ ", "
        ++ toMonthString
            (T.toMonth T.utc time)
        ++ " "
        ++ String.fromInt (T.toDay T.utc time)
        ++ " "
        ++ String.fromInt (T.toYear T.utc time)
        ++ " @ "
        ++ String.padLeft 2 '0' (String.fromInt (T.toHour T.utc time))
        ++ ":"
        ++ String.padLeft 2 '0' (String.fromInt (T.toMinute T.utc time))
        ++ " UTC"


toWeekdayString : T.Weekday -> String
toWeekdayString weekday =
    case weekday of
        T.Mon ->
            "Mon"

        T.Tue ->
            "Tue"

        T.Wed ->
            "Wed"

        T.Thu ->
            "Thu"

        T.Fri ->
            "Fri"

        T.Sat ->
            "Sat"

        T.Sun ->
            "Sun"


toMonthString : T.Month -> String
toMonthString month =
    case month of
        T.Jan ->
            "Jan"

        T.Feb ->
            "Feb"

        T.Mar ->
            "Mar"

        T.Apr ->
            "Apr"

        T.May ->
            "May"

        T.Jun ->
            "Jun"

        T.Jul ->
            "Jul"

        T.Aug ->
            "Aug"

        T.Sep ->
            "Sep"

        T.Oct ->
            "Oct"

        T.Nov ->
            "Nov"

        T.Dec ->
            "Dec"


previousMonth : T.Month -> T.Month
previousMonth month =
    case month of
        T.Jan ->
            T.Dec

        T.Feb ->
            T.Jan

        T.Mar ->
            T.Feb

        T.Apr ->
            T.Mar

        T.May ->
            T.Apr

        T.Jun ->
            T.May

        T.Jul ->
            T.Jun

        T.Aug ->
            T.Jul

        T.Sep ->
            T.Aug

        T.Oct ->
            T.Sep

        T.Nov ->
            T.Oct

        T.Dec ->
            T.Nov


daysForMonth : Int -> T.Month -> Int
daysForMonth year month =
    let
        -- This is **not** a perfect test for leap year, but for our purposes, its OK
        -- If people want to schedule events in the past or way in the future, say,
        -- in the year 2400, then we have a problem
        is_leapyear =
            0 == modBy 4 year
    in
    case month of
        T.Feb ->
            if is_leapyear then
                29

            else
                28

        T.Apr ->
            30

        T.Jun ->
            30

        T.Sep ->
            30

        T.Nov ->
            30

        _ ->
            31


daysForPreviousMonth : Int -> T.Month -> Int
daysForPreviousMonth year month =
    daysForMonth year <| previousMonth month


onEnter : Msg -> E.Attribute Msg
onEnter msg =
    E.htmlAttribute
        (HE.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if "Enter" == key then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the Enter key"
                    )
            )
        )
