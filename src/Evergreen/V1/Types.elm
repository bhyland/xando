module Evergreen.V1.Types exposing (..)

import Dict
import Lamdera


type alias Name = String


type PlayerId
    = PlayerOne
    | PlayerTwo


type alias Player = 
    { name : Name
    , playerId : PlayerId
    , clientId : Lamdera.ClientId
    , won : Int
    }


type alias GameId = (Player, Player)


type XO
    = X
    | O


type CellData
    = Empty
    | Filled XO


type alias Grid = 
    { cell11 : CellData
    , cell12 : CellData
    , cell13 : CellData
    , cell21 : CellData
    , cell22 : CellData
    , cell23 : CellData
    , cell31 : CellData
    , cell32 : CellData
    , cell33 : CellData
    }


type alias GameData = 
    { grid : Grid
    , currentMove : PlayerId
    }


type alias Game = 
    { gameId : GameId
    , player1 : Player
    , player2 : Player
    , gameData : GameData
    , firstPlayer : PlayerId
    , turns : Int
    , draws : Int
    }


type GameStage
    = ChoosingName
    | ChoosingPlayer
    | WaitingForAcceptance String
    | AcceptOrNot Lamdera.ClientId String
    | Playing Game
    | Draw Game
    | Won PlayerId Game


type alias FrontendModel =
    { gameStage : GameStage
    , clientId : Lamdera.ClientId
    , playerList : (Dict.Dict Lamdera.ClientId Name)
    , name : Name
    }


type alias BackendModel =
    { clientList : (Dict.Dict Lamdera.ClientId Name)
    }


type Cell
    = Cell11
    | Cell12
    | Cell13
    | Cell21
    | Cell22
    | Cell23
    | Cell31
    | Cell32
    | Cell33


type FrontendMsg
    = ChangeName PlayerId String
    | NameChosen String
    | SelectPlayer String Lamdera.ClientId
    | StartGame Name Lamdera.ClientId
    | ResetGame GameId
    | Choose Cell GameId
    | NoOpFrontendMsg


type ToBackend
    = Register String
    | Select Lamdera.ClientId
    | Start Player Player
    | Reset GameId
    | Chose Cell GameId


type BackendMsg
    = ClientConnected Lamdera.SessionId Lamdera.ClientId
    | ClientDisconnected Lamdera.SessionId Lamdera.ClientId


type ToFrontend
    = YourIdIs Lamdera.ClientId String
    | ClientListChanged (Dict.Dict Lamdera.ClientId String)
    | YouAreSelected Lamdera.ClientId
    | GameStarted Player Player
    | GameReset
    | CellChosen Cell