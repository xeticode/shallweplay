module DataModel exposing (Event, EventId, EventStatus(..), Events, Invite, InviteStatus(..), Invites, LoginField(..), Model, Password, Player, PlayerPosition, Players, PossibleEvent, PossibleEventSetter(..), UserName, amITheOrganizer, clearPossibleEventInfo, defaultMillisForPossibleEvent, defaultModel, emptyPlayerPosition, eventsByPlayer, eventsForPlayer, isMaybeVec2WithinMaybeDomElement, isThisMe, maybeEventFromPossibleEvent, newInvite, newPlayer, newPossibleEvent, nextEventId, nextInviteStatusForInvite, nextInviteStatusForStatus, playerEvents, primePossibleEventWithMillis, saveAndClearPossibleEvent, startingEvents, startingPlayers, toggleEventStatus, toggleEventStatusById, toggleInviteStatusByEventId)

import Browser.Dom as Dom
import Dict exposing (Dict)
import Draggable as D
import List.Extra as LE
import Math.Vector2 as M



--
-- Model
--


type alias Model =
    { input_user_name : UserName
    , input_password : Password
    , m_player : Maybe Player
    , players : Players
    , events : Events
    , m_possible_event : Maybe PossibleEvent
    , m_poss_event_elem_info : Maybe Dom.Element
    , login_offset : Float
    , login_field_with_tab_index : LoginField
    , dragged_player_position : PlayerPosition
    , drag : D.State UserName
    }



-- Player


type alias UserName =
    String


type alias Password =
    String


type alias PlayerPosition =
    ( Maybe UserName, Maybe M.Vec2 )


type alias Player =
    { user_name : UserName
    , member_since : Int
    }


type alias Players =
    Dict UserName Player



-- Event


type alias EventId =
    Int


type EventStatus
    = Canceled
    | Scheduled


type alias Event =
    { id : EventId
    , organizer_user_name : UserName
    , millis : Int
    , title : String
    , description : String
    , venue : String
    , status : EventStatus
    , invites : Invites
    }


type alias Events =
    Dict Int Event



-- Invite


type InviteStatus
    = Pending
    | Accepted
    | Declined


type alias Invite =
    { for_user_name : UserName
    , status : InviteStatus
    }


type alias Invites =
    Dict UserName Invite



-- Possible Event


type alias PossibleEvent =
    { millis : Int
    , title : String
    , description : String
    , venue : String
    , invites : Invites
    }


{-|

    How do you update a single record property via Msg?  We could have a separate
    Msg variant for each property, but that would not be a good separation of
    concerns between the Msg type and specific record details. We could have single Msg
    where we pass a setter function along with the value, but that makes the Msg and update
    handling more opaque during debugging.

    I believe the most idiomatic approach is to create a type with variants for each
    set operation driven by Msg, then have a single Msg that takes this type and a value
    and then pattern match in a case statement on the setter type.

    It's not much more verbose than passing a setter function, and keeps the Msg and
    update handling completely transparent.

-}
type PossibleEventSetter
    = MillisUp
    | MillisDown
    | Title
    | Description
    | Venue



-- Login Field


type LoginField
    = UserNameField
    | PasswordField
    | NoField



--
-- Default Data - should go away once the DB is hooked up
--


defaultModel : Model
defaultModel =
    { input_user_name = ""
    , input_password = ""
    , m_player = Nothing
    , players = startingPlayers
    , events = startingEvents
    , m_possible_event = Nothing
    , m_poss_event_elem_info = Nothing
    , login_offset = 0.0
    , login_field_with_tab_index = UserNameField
    , dragged_player_position = emptyPlayerPosition
    , drag = D.init
    }


emptyPlayerPosition : PlayerPosition
emptyPlayerPosition =
    ( Nothing, Nothing )


newPlayer : UserName -> Player
newPlayer user_name =
    Player user_name 0


newInvite : UserName -> Invite
newInvite user_name =
    Invite user_name Pending


defaultMillisForPossibleEvent : Int
defaultMillisForPossibleEvent =
    0


newPossibleEvent : Int -> PossibleEvent
newPossibleEvent millis =
    { millis = millis
    , title = ""
    , description = ""
    , venue = ""
    , invites = Dict.empty
    }


primePossibleEventWithMillis : Int -> PossibleEvent -> PossibleEvent
primePossibleEventWithMillis millis poss_event =
    if defaultMillisForPossibleEvent == poss_event.millis then
        { poss_event | millis = millis }

    else
        poss_event


clearPossibleEventInfo : Model -> Model
clearPossibleEventInfo model =
    { model
        | m_possible_event = Nothing
        , m_poss_event_elem_info = Nothing
        , dragged_player_position = emptyPlayerPosition
        , drag = D.init
    }


maybeEventFromPossibleEvent : Maybe Player -> Events -> Maybe PossibleEvent -> Maybe Event
maybeEventFromPossibleEvent m_player events m_possible_event =
    Maybe.map2
        (\p pe ->
            { id = nextEventId events
            , organizer_user_name = p.user_name
            , millis = pe.millis
            , title = pe.title
            , description = pe.description
            , status = Scheduled
            , venue = pe.venue
            , invites = pe.invites
            }
        )
        m_player
        m_possible_event


startingPlayers : Players
startingPlayers =
    Dict.fromList
        [ ( "Charlie", newPlayer "Charlie" )
        , ( "Jim", newPlayer "Jim" )
        , ( "Bones", newPlayer "Bones" )
        , ( "Solo", newPlayer "Solo" )
        , ( "Badger", newPlayer "Badger" )
        ]


startingEvents : Events
startingEvents =
    Dict.fromList
        [ ( 1
          , { id = 1
            , organizer_user_name = "Charlie"
            , millis = 1595999011722
            , title = "Sierra Game Club Meeting #220"
            , description = "On the schedule for this evening is Axis and Allies. If you bring a hat, you get free food."
            , venue = "Charlie's House"
            , status = Scheduled
            , invites =
                Dict.fromList
                    [ ( "Jim"
                      , { for_user_name = "Jim"
                        , status = Pending
                        }
                      )
                    , ( "Bones"
                      , { for_user_name = "Bones"
                        , status = Accepted
                        }
                      )
                    ]
            }
          )
        , ( 2
          , { id = 2
            , organizer_user_name = "Solo"
            , millis = 1595999011722
            , title = "Kepler Family Game Night"
            , description = "Lets play 'Draw for your Life' over Zoom!"
            , venue = "Online. Here are the details: <link goes here>"
            , status = Scheduled
            , invites =
                Dict.fromList
                    [ ( "Jade"
                      , { for_user_name = "Jade"
                        , status = Declined
                        }
                      )
                    , ( "Badger"
                      , { for_user_name = "Badger"
                        , status = Pending
                        }
                      )
                    , ( "Jim"
                      , { for_user_name = "Jim"
                        , status = Accepted
                        }
                      )
                    ]
            }
          )
        , ( 3
          , { id = 3
            , organizer_user_name = "Jim"
            , millis = 1599999011722
            , title = "South Pacific Game Convention"
            , description = "Come join me at the convention so we can meet new people and play some new games!"
            , venue = "Anaheim Convention Center"
            , status = Canceled
            , invites =
                Dict.fromList
                    [ ( "Solo"
                      , { for_user_name = "Solo"
                        , status = Pending
                        }
                      )
                    , ( "Bones"
                      , { for_user_name = "Bones"
                        , status = Accepted
                        }
                      )
                    , ( "Badger"
                      , { for_user_name = "Badger"
                        , status = Pending
                        }
                      )
                    , ( "Jade"
                      , { for_user_name = "Jade"
                        , status = Declined
                        }
                      )
                    ]
            }
          )
        ]



--
-- Model Logic
--


nextEventId : Events -> EventId
nextEventId events =
    Dict.keys events
        |> LE.last
        |> Maybe.withDefault 0
        |> (+) 1


isMaybeVec2WithinMaybeDomElement : Maybe M.Vec2 -> Maybe Dom.Element -> Bool
isMaybeVec2WithinMaybeDomElement m_vec2 m_elem_info =
    case Maybe.map2 (\vec2 elem_info -> ( vec2, elem_info )) m_vec2 m_elem_info of
        Just ( v2, ei ) ->
            let
                threshold =
                    20

                x =
                    M.getX v2

                y =
                    M.getY v2

                el =
                    ei.element
            in
            x > 0 - threshold && x < el.width - threshold && y > 0 - threshold && y < el.height - threshold

        Nothing ->
            False


isThisMe : Player -> Player -> Bool
isThisMe player me =
    player.user_name == me.user_name


amITheOrganizer : Event -> Player -> Bool
amITheOrganizer event me =
    event.organizer_user_name == me.user_name


saveAndClearPossibleEvent : Model -> Model
saveAndClearPossibleEvent ({ m_player, events, m_possible_event } as model) =
    case maybeEventFromPossibleEvent m_player events m_possible_event of
        Just event ->
            { model
                | events = Dict.insert event.id event model.events
                , m_possible_event = Nothing
            }

        Nothing ->
            { model | m_possible_event = Nothing }


playerEvents : Events -> Player -> List Event
playerEvents events player =
    let
        event_list =
            Dict.toList events
    in
    eventsByPlayer event_list player ++ eventsForPlayer event_list player


eventsByPlayer : List ( Int, Event ) -> Player -> List Event
eventsByPlayer event_list player =
    List.filterMap
        (\( _, e ) ->
            if amITheOrganizer e player then
                Just e

            else
                Nothing
        )
        event_list


eventsForPlayer : List ( Int, Event ) -> Player -> List Event
eventsForPlayer event_list player =
    List.filterMap
        (\( _, e ) ->
            if List.any (\( _, i ) -> i.for_user_name == player.user_name) (Dict.toList e.invites) then
                Just e

            else
                Nothing
        )
        event_list


toggleEventStatusById : EventId -> Events -> Events
toggleEventStatusById event_id events =
    let
        m_event =
            Dict.get event_id events
    in
    case m_event of
        Just event ->
            Dict.insert event_id (toggleEventStatus event) events

        Nothing ->
            events


toggleEventStatus : Event -> Event
toggleEventStatus event =
    case event.status of
        Canceled ->
            { event | status = Scheduled }

        Scheduled ->
            { event | status = Canceled }


toggleInviteStatusByEventId : UserName -> EventId -> Events -> Events
toggleInviteStatusByEventId for_user_name event_id events =
    case Dict.get event_id events of
        Just event ->
            case Dict.get for_user_name event.invites of
                Just invite ->
                    let
                        new_invite =
                            { invite | status = nextInviteStatusForInvite invite }

                        new_event =
                            { event | invites = Dict.insert for_user_name new_invite event.invites }
                    in
                    Dict.insert event_id new_event events

                Nothing ->
                    events

        Nothing ->
            events


nextInviteStatusForInvite : Invite -> InviteStatus
nextInviteStatusForInvite invite =
    nextInviteStatusForStatus invite.status


nextInviteStatusForStatus : InviteStatus -> InviteStatus
nextInviteStatusForStatus status =
    case status of
        Pending ->
            Accepted

        Accepted ->
            Declined

        Declined ->
            Pending
