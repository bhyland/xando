module Backend exposing (..)

import Lamdera exposing (ClientId, SessionId, broadcast, sendToFrontend)
import Dict exposing (Dict)
import Types exposing (..)


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { clientList = Dict.empty -- currently connected
      , sessions = Dict.empty -- Persists across sessions instead of local storage
      }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        ClientConnected sessionId clientId ->
            let
                name =
                    Dict.get sessionId model.sessions
                        |> Maybe.withDefault backupName
                
                backupName =
                    List.drop 
                    (modBy (List.length names) <| Dict.size model.clientList)
                    names
                    |> List.head
                    |> Maybe.withDefault "Falafel"
                        
                clientList = 
                    Dict.insert clientId name model.clientList

                sessions =
                    Dict.insert sessionId name model.sessions
                        
            in
                ( { model | clientList = clientList
                  , sessions = sessions }
                , sendToFrontend clientId <| YourIdIs clientId name )

        ClientDisconnected sessionId clientId ->
            let
                clientList = 
                    Dict.remove clientId model.clientList
            in
                ( { model | clientList = clientList }
                , broadcast (ClientListChanged clientList) )
            

updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        Register name ->
            let
                clientList = 
                    Dict.insert clientId name model.clientList

                sessions =
                    Dict.insert sessionId name model.sessions
            in
                ( { model | clientList = clientList
                  , sessions = sessions}
                , broadcast (ClientListChanged clientList) )

        Select opponentId ->
            ( model 
            , sendToFrontend opponentId <| YouAreSelected clientId )
            
        Start player1 player2 ->
                ( model 
                , sendToPlayers (player1, player2)  <| GameStarted player1 player2)
            
        Reset gameId ->
            ( model
            , sendToPlayers gameId <| GameReset )

        Chose cell gameId ->
            ( model
            , sendToPlayers gameId <| CellChosen cell )

            
            
nextGameId gameList =
    Dict.size gameList
            

sendToPlayers : GameId -> ToFrontend -> Cmd BackendMsg
sendToPlayers gameId msg =
    let
        player1 =
            Tuple.first gameId

        player2 =
            Tuple.second gameId
                
    in
        Cmd.batch [ sendToFrontend player1.clientId msg
                  , sendToFrontend player2.clientId msg ]

            
subscriptions model =
    Sub.batch
        [ Lamdera.onConnect ClientConnected
        , Lamdera.onDisconnect ClientDisconnected
        ]


names = ["Billy", "Rowena", "Salinzer", "Fazil", "Heesun", "Tommy", "Auntie G", "Norbert", "Arial", "Sigmund", "Spector", "Latifa"]

        
