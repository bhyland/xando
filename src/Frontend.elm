module Frontend exposing (..)  

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Html exposing (Html)
import Html.Attributes as Attr
import Lamdera exposing (ClientId, sendToBackend)
import Types exposing (..)
import Url

import Color -- avh4/elm-color

import Element exposing (Element, el, text, row, column, rgb255, px, fill)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Events as Events
import Element.Region as Region

import Dict exposing (Dict)

type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = \m -> NoOpFrontendMsg
        , onUrlChange = \m -> NoOpFrontendMsg
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \m -> Sub.none
        , view = view
        }
        

init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    (initialModel, Cmd.none)

               
initialModel : Model               
initialModel = { gameStage = ChoosingName
               , clientId = ""
               , name = ""
               , playerList = Dict.empty
               }


    
isItMyTurn : Model -> Bool
isItMyTurn model =
    case model.gameStage of
        AcceptOrNot _ _ ->
            True

        Playing game ->
            currentPlayerName game == model.name

        _ ->
            False

                
playerToString : Player -> Name
playerToString player = player.name

                        
togglePlayer : PlayerId -> PlayerId
togglePlayer player =
    if
        player == PlayerOne
    then
        PlayerTwo
    else
        PlayerOne
            

updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend bmsg model =
    case bmsg of

        YourIdIs clientId givenName ->
            ({model | clientId = clientId
             , name = givenName}, Cmd.none)
        
        ClientListChanged newList ->
            ( {model | playerList = newList}
            , Cmd.none)
        
        YouAreSelected opponentId ->
            ( { model | gameStage =
                  AcceptOrNot
                  (Maybe.withDefault "Someone?"
                       <| Dict.get opponentId model.playerList) opponentId }
            , Cmd.none )
                
        GameStarted player1 player2 ->
            ( {model | gameStage = Playing <| initGame player1 player2 }
            , Cmd.none )
                    
        GameReset ->
            let
                mGame = resetGame model
            in
                case mGame of
                    Just game ->
                        ( { model | gameStage = Playing <| game }
                        , Cmd.none )

                    Nothing ->
                        ( { model | gameStage = ChoosingName }
                        , Cmd.none )
                
        CellChosen cell ->
            ( chooseCell cell model
            , Cmd.none )

                

update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of

        ChangeName playerId name ->
            changeName playerId name model
                |> addCmd Cmd.none
        
        NameChosen name ->
            { model | gameStage = ChoosingPlayer }
                |> addCmd (sendToBackend <| Register name)

        SelectPlayer opponentName opponentId ->
            ( { model | gameStage = WaitingForAcceptance opponentName }
            , sendToBackend <| Select opponentId)

        StartGame opponentName opponentId ->
            let
                player1 = { name = model.name
                          , clientId = model.clientId
                          , playerId = PlayerOne
                          , won = 0 }

                player2 = { name = opponentName
                          , clientId = opponentId
                          , playerId = PlayerTwo
                          , won = 0 }
                    
            in
                model
                    |> addCmd (sendToBackend <| Start player1 player2 )
            
        ResetGame gameId ->
            model
                |> addCmd (sendToBackend <| Reset gameId)
            
                   
        Choose cell gameId ->
            model
                |> addCmd (sendToBackend <| Chose cell gameId)


        NoOpFrontendMsg ->
            (model, Cmd.none)
                
                
changeName : PlayerId -> Name -> Model -> Model
changeName playerId newName model =
    { model | name = newName }

                
chooseCell : Cell -> Model -> Model
chooseCell cell model =
    case model.gameStage of
        Playing game ->
            game 
                |> updateGrid cell
                |> updateTurn
                |> checkIfWon model

        _ ->
            doNothing model


updateTurn : Game -> Game
updateTurn game =
                  { game | turns = game.turns - 1}

                      
addCmd : Cmd FrontendMsg -> Model ->  (Model, Cmd FrontendMsg)
addCmd cmd model = ( model, cmd )                

               
doNothing : Model -> Model
doNothing model = model

                  
resetGame : Model -> Maybe Game
resetGame model =
    case model.gameStage of
        Playing game ->
            Just { game | gameData = resetGameData (togglePlayer game.firstPlayer)
                 , firstPlayer = (togglePlayer game.firstPlayer)
                 , turns = 9
                 }
                    
        Draw game ->
            Just { game | gameData = resetGameData (togglePlayer game.firstPlayer)
                 , firstPlayer = (togglePlayer game.firstPlayer)
                 , turns = 9
                 }
            
        Won winnerId game ->
            Just { game | gameData = resetGameData (togglePlayer winnerId)
                 , firstPlayer = (togglePlayer winnerId)
                 , turns = 9 }

        _  ->
            Nothing
            
                
initGame : Player -> Player -> Game
initGame p1 p2 =
    { player1 = p1
    , player2 = p2
    , gameId = (p1, p2)
    , gameData = resetGameData PlayerOne
    , firstPlayer = PlayerOne
    , turns = 9
    , draws = 0
    }

    
updateDraws game =
    {game | draws = game.draws + 1}

        
updateWinnerScore winnerId game =
            let
                player1 =
                    game.player1

                player2 =
                    game.player2
            in
                case winnerId of
                    PlayerOne ->
                        { game | player1 = { player1 | won = player1.won + 1 }
                        }

                    PlayerTwo ->
                        { game | player2 = { player2 | won = player2.won + 1 }
                        }
    
    
resetGameData : PlayerId -> GameData        
resetGameData firstPlayer = { grid = initialGrid 
                            , currentMove = firstPlayer
                            }


type alias Grid = { cell11 : CellData, cell12 : CellData, cell13: CellData
                  , cell21 : CellData, cell22 : CellData, cell23: CellData
                  , cell31 : CellData, cell32 : CellData, cell33: CellData }

    
initialGrid : Grid
initialGrid = Grid Empty Empty Empty
                   Empty Empty Empty
                   Empty Empty Empty

                       
getCell : Cell -> Grid -> CellData                       
getCell cell grid =
    case cell of
        Cell11 -> grid.cell11 
        Cell12 -> grid.cell12
        Cell13 -> grid.cell13
        Cell21 -> grid.cell21
        Cell22 -> grid.cell22
        Cell23 -> grid.cell23
        Cell31 -> grid.cell31
        Cell32 -> grid.cell32
        Cell33 -> grid.cell33

                  
setCell : Cell -> CellData -> Grid -> Grid                       
setCell cell cellData grid = 
        case cell of
        Cell11 -> { grid | cell11 = cellData }
        Cell12 -> { grid | cell12 = cellData }
        Cell13 -> { grid | cell13 = cellData }
        Cell21 -> { grid | cell21 = cellData }
        Cell22 -> { grid | cell22 = cellData }
        Cell23 -> { grid | cell23 = cellData }
        Cell31 -> { grid | cell31 = cellData }
        Cell32 -> { grid | cell32 = cellData }
        Cell33 -> { grid | cell33 = cellData }

    
updateGrid : Cell -> Game -> Game
updateGrid cell game =
    let
        gameData =
            game.gameData
        
        grid =
            gameData.grid
            
        currentCell =
            getCell cell grid

        xo =
            fillCellData game.gameData.currentMove
    in
        
        case currentCell of
            Empty ->
                let
                    updatedGameData =
                        { gameData | grid = setCell cell xo grid
                        }
                in  
                    { game | gameData = updatedGameData
                    }

            Filled _ ->
                game


                    
fillCellData : PlayerId -> CellData
fillCellData playerid =
    case playerid of
        PlayerOne ->
            Filled X -- Simon
                
        PlayerTwo -> -- Grandpa
            Filled O


checkIfWon : Model -> Game -> Model
checkIfWon model game =
    let
        gameData =
            game.gameData
                
        grid =
            game.gameData.grid
                
        wins c1 c2 c3 =
            (c1 == c2 && c2 == c3) &&
            ( isFilled c1 && isFilled c2 && isFilled c3)

        isFilled c =
            case c of
                Filled _ -> True
                Empty    -> False
    in
        if
               (wins grid.cell11 grid.cell22 grid.cell33)
            || (wins grid.cell13 grid.cell22 grid.cell31)    
            || (wins grid.cell11 grid.cell12 grid.cell13)
            || (wins grid.cell21 grid.cell22 grid.cell23)
            || (wins grid.cell31 grid.cell32 grid.cell33)
            || (wins grid.cell11 grid.cell21 grid.cell31)
            || (wins grid.cell12 grid.cell22 grid.cell32)
            || (wins grid.cell13 grid.cell23 grid.cell33)
                
        then
            let
                winnerId = gameData.currentMove
            in
                { model | gameStage = game
                                   |> updateWinnerScore winnerId 
                                   |> Won winnerId
                }
                   

        else
            if
                game.turns <= 0
                    
            then
                { model | gameStage = Draw <| updateDraws game }


            else
                { model | gameStage = 
                     Playing <| { game | gameData =
                                 { gameData | currentMove = togglePlayer gameData.currentMove
                                 }
                           }
                }


---- VIEW ----


view : Model -> Browser.Document FrontendMsg
view model =
    { title = "Play games with your Grandparents!"
    , body =
       [ Element.layout containerStyle <|
      column (mainStyle <| isItMyTurn model)
        [ el headingStyle
              <| text "Tic Tac Toe!"

        , case model.gameStage of
              ChoosingName ->
                  viewAskForName model.name

              ChoosingPlayer ->
                  viewChooseOpponent model

              WaitingForAcceptance opponentName->
                  viewWaitingForAcceptance opponentName model 

              AcceptOrNot opponent clientId ->
                  viewAcceptOrNot opponent clientId model
                      
              Playing game ->
                  viewGame model.name game

              Draw game ->
                  viewDraw model.name game
                      
              Won playerid game ->
                  let
                      player id =
                          if game.player1.playerId == id then
                              game.player1
                          else
                              game.player2
                  in
                      viewWon (player playerid) model.name game

        ]]
    }

    
viewAskForName name =
    column viewStyle <|
        [ column startColStyle [ nameInput PlayerOne name
                               ]

        , Input.button
            buttonStyle
            { onPress = Just <| NameChosen name
            , label = text "Find an Opponent"}
        ]

    
viewChooseOpponent model =
    let
        opponentList =
            List.filter (\x -> Tuple.first x /= model.clientId) (Dict.toList model.playerList)
    in
        column viewStyle <|
            [ Element.paragraph [] [text <| "Hi "
                                        ++ model.name
                                        ++ "! Choose your opponent:"]
                  
            , if
                  opponentList == []
              then
                  Element.paragraph [] [text "Waiting for other players..."]
              else
                  column choosePlayerStyle
                      (List.map playerChoice opponentList)
            ]


viewWaitingForAcceptance : Name ->  Model -> Element FrontendMsg             
viewWaitingForAcceptance opponent model =
         column viewStyle <|
                 [ viewMe model.name PlayerOne
                       
                 , Element.paragraph []
                     [text <| "Waiting for " ++ opponent ++ " to join the game..."]
                         
                 , Input.button
                      buttonStyle
                      { onPress = Just <| NameChosen model.name 
                      , label = text "Cancel"}
                 ]

             
viewAcceptOrNot : Name -> ClientId -> Model -> Element FrontendMsg             
viewAcceptOrNot opponentName opponentId model =
         column viewStyle <|
                 [ viewMe model.name PlayerTwo
                 , Element.paragraph []
                     [ text <| opponentName ++ " wants to play. Accept?"]

                 , Input.button
                      buttonStyle
                      { onPress = Just <| StartGame opponentName opponentId
                      , label = text "Yes"}

                 , Input.button
                      buttonStyle
                      { onPress = Just <| NameChosen model.name 
                      , label = text "No"}
                 ]

             
playerChoice : (ClientId, Name) -> Element FrontendMsg
playerChoice (clientId, playerName) =
    Input.button
        choosePlayerButtonStyle
        { onPress = Just <| SelectPlayer playerName clientId 
        , label = text playerName
        }
        

viewMe : Name -> PlayerId -> Element FrontendMsg
viewMe name playerId =
        el (viewMeStyle playerId)
            <| text name 

                 
nameInput : PlayerId -> String -> Element FrontendMsg
nameInput playerId name =
        Input.text
            []
            { onChange = ChangeName playerId
            , text = name
            , placeholder =
                Just <| Input.placeholder [] (text "FirstName")
                    
            , label =
                Input.labelLeft [ ] <| row (playerLabelStyle playerId)
                    [ text "Your name: "]

            }

            
currentPlayerName : Game -> String
currentPlayerName game =
    case game.gameData.currentMove of
        PlayerOne ->
            game.player1.name
                
        PlayerTwo ->
            game.player2.name


viewScore : Game -> Element FrontendMsg                
viewScore game =
    Element.wrappedRow scoreStyle [ text "Score: "
                                  , el (playerLabelStyle PlayerOne)
                                      (text <| game.player1.name
                                           ++ ": "
                                           ++ String.fromInt game.player1.won)
                                          
                                  , el (playerLabelStyle PlayerTwo)
                                      (text <| game.player2.name
                                           ++ ": "
                                           ++ String.fromInt game.player2.won)

                                  , el [] 
                                      (text <| "Draws: "
                                           ++ String.fromInt game.draws)
                     ]

                
                
viewGame : Name -> Game -> Element FrontendMsg
viewGame myName game =
    let
        currentPlayer =
            currentPlayerName game
               
        player =
            game.gameData.currentMove

        myTurn =
            currentPlayer == myName

        myPlayerId =
            if game.player1.name == myName
            then PlayerOne
            else PlayerTwo
               
    in
        column viewStyle
            [ viewMe myName myPlayerId
            , el (playerLabelStyle player) (text <| currentPlayer ++ "'s turn")
            , viewGrid game.gameId game.gameData myTurn
            , Input.button buttonStyle { onPress = Just <| ResetGame game.gameId
                                       , label = text "I give up!"}

            , viewScore game
            ]

        
viewDraw : Name -> Game -> Element FrontendMsg
viewDraw myName game =
    let
        myPlayerId =
            if game.player1.name == myName
            then PlayerOne
            else PlayerTwo
            
    in
    column viewStyle
        [ viewMe myName myPlayerId

        , row [ Element.centerX ] [ el (playerLabelStyle PlayerOne) <| text  "It's"
                                  , el (playerLabelStyle PlayerTwo) <| text  " a"
                                  , el (playerLabelStyle PlayerOne) <| text  " Dr"
                                  , el (playerLabelStyle PlayerTwo) <| text  "aw!"]
        , viewGrid game.gameId game.gameData True
        , Input.button buttonStyle
            { onPress = Just <| ResetGame game.gameId
            , label = text "Play Again"}
        , viewScore game
        ]

        
viewWon : Player -> Name -> Game -> Element FrontendMsg
viewWon winningPlayer myName game =
    let
        myPlayerId =
            if game.player1.name == myName
            then PlayerOne
            else PlayerTwo
            
    in
    column viewStyle
        [ viewMe myName myPlayerId
              
        , el (playerLabelStyle winningPlayer.playerId)
            (text <| playerToString winningPlayer ++ " Won!")
                
        , viewGrid game.gameId game.gameData False
            
        , Input.button buttonStyle
            { onPress = Just <| ResetGame game.gameId
            , label = text "Play Again"
            }
            
        , viewScore game
        ]

        
viewGrid : GameId -> GameData -> Bool -> Element FrontendMsg
viewGrid gameId data myTurn =
    let
        grid =
            data.grid
               
        viewCell cellid celldata =
            case celldata of
                Filled xo ->
                    el tileStyle <| viewXO xo
                        
                Empty ->
                    Input.button tileStyle
                          { onPress =
                                if myTurn
                                then Just <| Choose cellid gameId
                                else Nothing
                          , label =
                              text ""
                          }
                    
    in
        column gridStyle
            [ row [ ] [ viewCell Cell11 grid.cell11
                      , viewCell Cell12 grid.cell12
                      , viewCell Cell13 grid.cell13 ]
            , row [ ] [ viewCell Cell21 grid.cell21
                      , viewCell Cell22 grid.cell22
                      , viewCell Cell23 grid.cell23 ]
            , row [ ] [ viewCell Cell31 grid.cell31
                      , viewCell Cell32 grid.cell32
                      , viewCell Cell33 grid.cell33 ]
            ]
            
-- Colors and Styles

from c = Element.fromRgb <| Color.toRgba c
    
black : Element.Color 
black = from Color.black

white : Element.Color
white = from Color.white
        
orange : Element.Color
orange = from  Color.orange

blue : Element.Color
blue = from Color.blue
                 
green : Element.Color
green = from Color.green

grey : Element.Color        
grey = from Color.grey

purple : Element.Color
purple =  from Color.purple

red : Element.Color
red = from Color.red
          
viewXO xo =
    case xo of
        X ->
            el
                (playerXOStyle PlayerOne)
                <| text "X"
        O ->
            el
                (playerXOStyle PlayerTwo)
                <| text "O"



playerColor player =
    case player of
        PlayerOne ->
            Font.color green
            

        PlayerTwo ->
            Font.color blue
            
    
                    
playerXOStyle player =
            [ Element.centerX
            , Element.centerY
            , playerColor player
            ]

playerLabelStyle player =
    [ playerColor player
    , Element.centerX]


viewMeStyle player =
        [ playerColor player
        , Element.centerX
        , Font.size 40
        ] 
    
    
containerStyle =
    [ Element.padding 20
    ]
            
mainStyle isMyTurn =
    [ Element.padding 20
    , Element.centerX
    , Element.centerY
    , Border.width 5
    , Border.rounded 20
    , Border.color purple
    , Element.width (fill |> Element.maximum 600)
    , Element.height fill
    , if
          isMyTurn
      then
          Border.glow purple 10
      else
          Border.glow purple 0
    ]

viewStyle =
    [ Element.centerX
    , Element.padding 40
    , Element.spacing 20
    , Element.height fill
    ]

    
headingStyle = 
    [ Element.centerX
    , Element.alignTop
    , Region.heading 1
    , Font.size 60
    , Font.color purple
    ]


gridStyle =
    [ Element.centerX ]

        
tileStyle =
    [ Element.height <| px 100
    , Element.width  <| px 100
    , Border.solid
    , Border.width 5
    , Border.color black
    , Border.rounded 10
    , Background.color <| orange
    , Font.size 80
    , Font.color blue
    ]
        
buttonStyle =
    [ Background.color blue
    , Font.color white
    , Border.solid
    , Border.width 2
    , Border.color black
    , Border.rounded 5
    , Element.padding 10
    , Element.centerX
    , Element.alignBottom
    ]

    
choosePlayerStyle =
    [ Element.spacing 20
    , Element.scrollbars
    , Element.height (fill |> Element.maximum 300)
    ]

choosePlayerButtonStyle =
    [ Background.color purple
    , Font.color white
    , Border.solid
    , Border.width 2
    , Border.color black
    , Border.rounded 5
    , Element.padding 10
    , Element.centerX
    , Element.alignTop
    , Element.width <| px 200
    ]
    
startColStyle =
    [ Element.centerX
    , Element.centerY
    ]

scoreStyle =
    [ Element.spacing 20
    , Element.alignBottom
    ]
