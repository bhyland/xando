module Types exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)
import Lamdera exposing (ClientId, SessionId)


-- My Base Types
    
type alias Game =  { gameId   : GameId
                   , player1  : Player
                   , player2  : Player 
                   , gameData : GameData
                   , firstPlayer : PlayerId
                   , turns : Int
                   , draws : Int
                   }

    
type alias GameId = (Player, Player)
    
type alias Log = List String

type alias Name = String
      
type alias GameData =
    { grid : Grid
    , currentMove : PlayerId
    } 

    
type Cell = Cell11 | Cell12 | Cell13
          | Cell21 | Cell22 | Cell23
          | Cell31 | Cell32 | Cell33
                  
type CellData = Empty | Filled XO

type XO = X | O

    
type alias Player =
    { name : Name
    , playerId : PlayerId
    , clientId : ClientId
    , won : Int
    }

    
type PlayerId = PlayerOne | PlayerTwo


type alias Grid = { cell11 : CellData, cell12 : CellData, cell13: CellData
                  , cell21 : CellData, cell22 : CellData, cell23: CellData
                  , cell31 : CellData, cell32 : CellData, cell33: CellData }



-- Frontend
type alias FrontendModel = { gameStage : GameStage
                           , clientId : ClientId
                           , playerList : Dict ClientId Name
                           , name : Name
                           }

type GameStage = ChoosingName 
               | ChoosingPlayer
               | WaitingForAcceptance String -- Opponent name
               | AcceptOrNot ClientId String  -- Opponent Id, Opponent name
               | Playing Game
               | Draw Game
               | Won PlayerId Game

    
type FrontendMsg = ChangeName PlayerId String
                 | NameChosen String
                 | SelectPlayer String ClientId
                 | StartGame Name ClientId
                 | ResetGame GameId
                 | Choose Cell GameId
                 | NoOpFrontendMsg
                   
type ToBackend
    = Register String
    | Select ClientId
    | Start Player Player
    | Reset GameId
    | Chose Cell GameId
     

                   
-- Backend                     
type alias BackendModel =
    { sessions : Dict SessionId Name
    , clientList : Dict ClientId Name
    }

type BackendMsg
    = ClientConnected SessionId ClientId
    | ClientDisconnected SessionId ClientId


type ToFrontend
    = YourIdIs ClientId String
    | ClientListChanged (Dict ClientId String)
    | YouAreSelected ClientId 
    | GameStarted Player Player
    | GameReset 
    | CellChosen Cell 
