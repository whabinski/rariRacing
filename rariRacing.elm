------------------------------------------------------------------------------------------
-- Needed for importing images
------------------------------------------------------------------------------------------
import Html
import Html.Attributes as Html

appTitle = "Rari Racin"

------------------------------------------------------------------------------------------
-- MISC (DO NOT TOUCH)
------------------------------------------------------------------------------------------

-- use these variables for your collage size
collageWidth = 192
collageHeight = 128

-- depending on the state you can turn on and off typing
allowTyping model = model.typing /= NotTyping 

-- depending on state you can turn on and off animation (the Tick message)
isAnimating model = True

------------------------------------------------------------------------------------------
-- Model & Init & Types
------------------------------------------------------------------------------------------

type alias Model =  
                 {window : Window -- DO NOT TOUCH (UNUSED)
                 
                 , time : Float    
                 , state : State --transition states
                 , difficulty : Difficulty -- level of difficulty
                 , questions : List Questions -- list of question types
                 , typing : TypingState --needed for the typing app text box
                 , answerBox : List String -- a list of characters that are entered into the text box
                 , correct : GameState -- state of getting a question right or wrong
                 , goodGuy : Float --Blue circle position
                 , badGuySpeed : Float --How fast the red circle goes
                 , startTime : Float --Used to reset time to 0 when entering the game state
                 , winState : WinState --The state of winning or losing the game
                 , highlight : Int --to highlight buttons
                 , indicationTimer : Float -- To calculate how long the correct / incorrect message shows up
                 , backgroundTimer : Float -- To calculate how long to move the background after getting a correct answer
                }
init : Model                    
init = { window = { cw = collageWidth, ch = collageHeight, sw = 0, sh = 0 } -- DO NOT TOUCH (UNSUED)

       , time = 0
       , state = TitleScreen
       , difficulty = Easy
       , questions = []
       , typing = NotTyping
       , answerBox = []
       , correct = Waiting
       , goodGuy = -50
       , badGuySpeed = 1
       , startTime = 0.0
       , winState = Other
       , highlight = 0
       , indicationTimer = 1
       , backgroundTimer = 0
       } 

type alias Questions = {q1 : Int, q2 : Int, answer : String, op : String}

type State = TitleScreen 
           | Difficulty 
           | Game 
           | Visualizer
           | Tutorial
           
type TypingState = Typing | NotTyping

type Difficulty = Easy 
                | Medium
                | Hard

type GameState = Waiting | Correct | Incorrect

type WinState = Won | Lost | Other

------------------------------------------------------------------------------------------
-- Helper Functions
------------------------------------------------------------------------------------------

-- FUNCTION TO CONVERT GAMESTATE TO STRING                       
gameStateToString : GameState -> String
gameStateToString x = 
                    if x == Waiting then " "
                    else if x == Correct then "CORRECT"
                    else "INCORRECT"

-- FUNCTION TO TURN MAYBE QUESTION INTO QUESTION
  -- NEEDED FOR ACCESSING LIST.HEAD
getQuestion : Maybe Questions -> Questions
getQuestion q = 
  case q of
     (Just value) -> value
     Nothing ->  {q1 = 0, q2 = 0, answer = "0", op = " "}
     
-- FUNCTION THAT FORMATS THE QUESTION
getQuestionQuestions : Questions -> String
getQuestionQuestions q = (String.fromInt q.q1) ++ " " ++ (q.op) ++ " " ++ (String.fromInt q.q2) ++ " ="

-- TO MAKE THE TEXTBOX
textBox isHighlighted chars =
  [ rect 60 15 |> filled white
  , text (String.join "" <| List.reverse chars ) |> centered |> size 8 |> filled black |> move (0,-2) 
      |> clip (rect 60 15 |> ghost)
  , rect 60 15 |> outlined (solid 1) (if isHighlighted then rgb 0 0 255 else charcoal)
  ] |> group
    
-- FUNCTION TO CONVERT STRING TO CHARACTER    
stringToChar : String -> Char
stringToChar s = 
  let x = List.head (String.toList s)
  in case x of 
       (Just char) -> char
       Nothing -> ' '  
  
-- FUNCTION TO UPDATE STRING THAT IS TYPED INTO TEXT BOX
typeAndDelete soFar code =
    if (String.length code == 1) && (Char.isDigit (stringToChar code)) then 
        code :: soFar 
    else if code == "Backspace" then
        List.drop 1 soFar
    else soFar

-- FUNCTION TO CHECK IF ANSWERBOX MATCHES QUESTION ANSWER
checkAnswer : Model -> Questions -> (List String) -> Bool
checkAnswer model question answer = 
                            if (question.answer == (String.concat(List.reverse(model.answerBox)))) then True
                            else False

-- ran Produces a random number between 1 and 9
  -- for shuffling the list of questions
ran model = Basics.round(Basics.abs(9 * sin(model.time)))

-- FUNCTION TO SHUFFLE THE LIST OF QUESTIONS
shuffle : Model -> List Questions -> List Questions
shuffle model qs = let
                     first = List.take (ran model) qs
                     last = List.drop (ran model) qs
                   in
                     List.append (List.reverse first) (List.reverse last)

----------------------------
-- Functions For Vizualizer
----------------------------

-- TO DETERMINE HIGHEST AND LOWEST NUMBER 
  -- OUTPUTS (Lower, Higher)
numOrder : Float -> Float -> (Float, Float)
numOrder num1 num2 = 
  if num1 > num2 then (num2,num1)
  else (num1,num2)

make1Line num1 num2 col = 
  if num2 > 0
  then
    [ circle 3 |> filled col ]
    ++
    [
    make1Line num1 (num2-1) col |> group |> move (10,0)
    ]
  else []

mult num1 num2 =
  let 
    firstLine = make1Line num1 num2 yellow
  in 
    if num1 > 0
    then
      firstLine
      ++ 
      [
      mult (num1-1) num2 |> group |> move (0,-15)
      ]
    else 
      []

add num1 num2 = group 
  [
  make1Line num1 num2 blue |> group
  ,
  make1Line num2 num1 green |> group |> move (0,-20)
  ]

sub num1 num2 = 

  if num2 > 0
  then
    [ circle 2 |> filled (if num1 >= num2 then red else blue) ]
    ++
    [
    sub num1 (num2-1) |> group |> move (5,0)
    ]
  else []

visualizer model =
                  case (getQuestion (List.head model.questions)).op of 
                  
                     "+" -> add (Tuple.first(numOrder (toFloat (getQuestion (List.head model.questions)).q1) (toFloat (getQuestion (List.head model.questions)).q2))) 
                                (Tuple.second(numOrder (toFloat (getQuestion (List.head model.questions)).q1) (toFloat (getQuestion (List.head model.questions)).q2)))
                                |> move (((Tuple.second(numOrder (toFloat (getQuestion (List.head model.questions)).q1) (toFloat (getQuestion (List.head model.questions)).q2))) * -5),0)
                               
                     "-" -> sub (Tuple.first(numOrder (toFloat (getQuestion (List.head model.questions)).q1) (toFloat (getQuestion (List.head model.questions)).q2))) 
                                (Tuple.second(numOrder (toFloat (getQuestion (List.head model.questions)).q1) (toFloat (getQuestion (List.head model.questions)).q2)))
                                |> group |> move (((Tuple.second(numOrder (toFloat (getQuestion (List.head model.questions)).q1) (toFloat (getQuestion (List.head model.questions)).q2))) * -2.5 + 2),0)
                     
                     "*" -> mult (Tuple.first(numOrder (toFloat (getQuestion (List.head model.questions)).q1) (toFloat (getQuestion (List.head model.questions)).q2)))
                                 (Tuple.second(numOrder (toFloat (getQuestion (List.head model.questions)).q1) (toFloat (getQuestion (List.head model.questions)).q2)))
                                 |> group |> move ((Tuple.second(numOrder (toFloat (getQuestion (List.head model.questions)).q1) (toFloat (getQuestion (List.head model.questions)).q2))) * -5,(Tuple.first(numOrder (toFloat (getQuestion (List.head model.questions)).q1) (toFloat (getQuestion (List.head model.questions)).q2))) * 7.5)
                     
                     otherwise -> mult (Tuple.first(numOrder ((toFloat (getQuestion (List.head model.questions)).q1) / (toFloat (getQuestion (List.head model.questions)).q2)) (toFloat (getQuestion (List.head model.questions)).q2)))
                                 (Tuple.second(numOrder ((toFloat (getQuestion (List.head model.questions)).q1) / (toFloat (getQuestion (List.head model.questions)).q2)) (toFloat (getQuestion (List.head model.questions)).q2)))
                                 |> group |> move (((toFloat (getQuestion (List.head model.questions)).q2) * -5) , (((toFloat (getQuestion (List.head model.questions)).q1) / (toFloat (getQuestion (List.head model.questions)).q2)) * 5))
                                       

correctSignifier model = group 
  [
    case model.correct of
      Correct -> text "CORRECT!" |> centered |> size 30 |> filled green
      Incorrect -> text "INCORRECT!" |> centered |> size 30 |> filled red
      otherwise -> [] |> group
  ]

------------------------------------------------------------------------------------------
-- MyShapes
------------------------------------------------------------------------------------------

myShapes model =
    case model.state of
        TitleScreen  ->
            [
            titleScreenBackground |> scaleX 1.1 |> scaleY 1.02|> move (-2.5,0)
            , group
                  [    roundedRect 70 45 5 |> filled black |> makeTransparent (if model.highlight == 1 then 0.5 else 0)
                  ]    |> move (-45, 1)  |> notifyEnter (Highlight 1) |> notifyLeave (Highlight 0)|> notifyTap ToDifficulty                              --To difficulty state
            , group
                  [    rect 85 17 |> filled black |> makeTransparent (if model.highlight == 1 then 0.5 else 0) |> move (-5,29)    
                  ]    |> move (-41, 4)  |> notifyEnter (Highlight 1) |> notifyLeave (Highlight 0) |> notifyTap ToDifficulty             
                  
            , group
                  [    roundedRect 45 45 5 |> filled black |> makeTransparent (if model.highlight == 2 then 0.5 else 0)   
                  ]    |> move (45, 3) |> notifyEnter (Highlight 2) |> notifyLeave (Highlight 0) |> notifyTap ToTutorial                               --To difficulty state
            , group
                  [    rect 85 17 |> filled black |> makeTransparent (if model.highlight == 2 then 0.5 else 0) |> move (86.5,29)    
                  ]    |> move (-41, 4) |> notifyEnter (Highlight 2) |> notifyLeave (Highlight 0) |> notifyTap ToTutorial   
            ]
         
        Tutorial  ->
            [ square 200 |> filled black      --background
            , text "Tutorial" |> centered |> underline |> size 16 |>  filled white |> move (0,50)   -- Tutorial title text
            , text "Objective" |> centered |> underline |> size 8 |> filled white |> move (-50,25)
            , roundedRect 70 90 5 |> outlined (solid 1) red |> move (-50,-10)        -- objectives box
            , text "The goal of this game is to" |> centered |> size 5 |> filled white  |> move (-50, 0)
            , text "reach the finish line before" |> centered |> size 5 |> filled white  |> move (-50, -10)
            , text "your opponent does" |> centered |> size 5 |> filled white  |> move (-50, -20)
            
            , text "Controls" |> centered |> underline |> size 8 |> filled white |> move (50,25)
            , roundedRect 70 90 5 |> outlined (solid 1) red |> move (50,-10)         -- controls box
            , text "To move your car, type your" |> centered |> size 5 |> filled white  |> move (50, 5)
            , text "answer to the questions" |> centered |> size 5 |> filled white  |> move (50, -5)
            , text "displayed on screen correctly" |> centered |> size 5 |> filled white  |> move (50, -15)
            , text "and quickly into the provided" |> centered |> size 5 |> filled white  |> move (50, -25)
            , text "text box" |> centered |> size 5 |> filled white  |> move (50, -35)
            
            , group
                  [    rect 20 10  |> filled (rgb 200 200 200),
                       triangle 8 |> filled (rgb 200 200 200) |> rotate (degrees 180) |> move (-13,0)      -- Back button
                  ,    text "Back" |> centered |> size 8 |> filled black|> move(-4, -3)
                  ]    |> move (-75, 50 ) |> notifyTap ToTitle
            ]
            
        Difficulty  ->
            [ square 200 |> filled black 
            , text "Choose Your Difficulty" |> centered|> filled white |> move (0,55)
            , text "(by clicking on the vehicle, or the difficulty buttons)" |> centered|> size 4 |>filled white |> move (0,50)
            , group
                  [    hardCar |> move (15, 24)
                  ,    roundedRect 30 10 5 |> filled red |> move (0,10) |> makeTransparent 0 |> notifyEnter (Highlight 5) |> notifyLeave (Highlight 0) |> notifyTap DifficultyHard |> notifyTap ToGame
                  ,    roundedRect 30 10 5 |> filled red |> makeTransparent (if model.highlight == 5 then 1 else 0.5) |> notifyEnter (Highlight 5) |> notifyLeave (Highlight 0) |> notifyTap DifficultyHard |> notifyTap ToGame
                  ,    text "Hard" |> centered |> size 8 |> filled black|> move(0, -3) |> notifyEnter (Highlight 5) |> notifyLeave (Highlight 0) |> notifyTap DifficultyHard |> notifyTap ToGame
                  ]    |> move (55,5) |> scale 1.2   --Hard difficulty button
            , group
                  [    roundedRect 49 48 5 |> outlined (solid 1) red |> move (18, -47)
                  ,    text "+" |> centered |> size 16 |> filled white |>  move (0,-35)  
                  ,    text "-" |> centered |> size 16 |> filled white |>  move (0,-45)
                  ,    text "*" |> centered |> size 16 |> filled white |>  move (0,-60)
                  ,    text "/" |> centered |> size 16 |> filled white |>  move (0,-70)
                  ,    text "Addition" |> centered |> size 6 |> filled white |>  move (17,-31)
                  ,    text "Subtraction" |> centered |> size 6 |> filled white |>  move (20,-43)
                  ,    text "Multiplication" |> centered |> size 6 |> filled white |>  move (23,-55)
                  ,    text "Divison" |> centered |> size 6 |> filled white |>  move (16,-67)
                  ]    |> move (48,17)     
                  
            , group
                  [    mediumCar |> scale 1.5
                  ,    roundedRect 30 10 5 |> filled yellow |> move (0,-13) |> makeTransparent (if model.highlight == 4 then 1 else 0.5)
                  ,    text "Medium" |> centered |> size 8 |> filled black|> move(0, -16)       
                  ]    |> move (0,17.6)|> scale 1.2 |> notifyEnter (Highlight 4) |> notifyLeave (Highlight 0) |> notifyTap DifficultyMedium |> notifyTap ToGame  --Medium difficulty button
            , group
                  [    roundedRect 49 48 5 |> outlined (solid 1) red |> move (18, -47)
                  ,    text "+" |> centered |> size 16 |> filled white |>  move (0,-38)  
                  ,    text "-" |> centered |> size 16 |> filled white |>  move (0,-51)
                  ,    text "*" |> centered |> size 16 |> filled white |>  move (0,-69)
                  ,    text "Addition" |> centered |> size 6 |> filled white |>  move (17,-35)
                  ,    text "Subtraction" |> centered |> size 6 |> filled white |>  move (20,-49)
                  ,    text "Multiplication" |> centered |> size 6 |> filled white |>  move (23,-63)
                  ]    |> move (-18,17)
                  
            , group
                  [    easyCar |> move (0,16)
                  ,    roundedRect 30 10 5 |> filled green |> makeTransparent (if model.highlight == 3 then 1 else 0.5)
                  ,    text "Easy" |> centered |> size 8 |> filled black|> move(0, -3)        
                  ]    |> move (-55, 5) |> scale 1.2 |> notifyEnter (Highlight 3) |> notifyLeave (Highlight 0) |> notifyTap DifficultyEasy |> notifyTap ToGame  --Easy difficulty button
            , group
                  [    roundedRect 49 48 5 |> outlined (solid 1) red |> move (18, -47)
                  ,    text "+" |> centered |> size 16 |> filled white |>  move (0,-44)  
                  ,    text "-" |> centered |> size 16 |> filled white |>  move (0,-61)
                  ,    text "Addition" |> centered |> size 6 |> filled white |>  move (17,-40)
                  ,    text "Subtraction" |> centered |> size 6 |> filled white |>  move (20,-59)
                  ]    |> move (-84,17)
            
            , group
                  [    rect 20 10  |> filled (rgb 200 200 200),
                       triangle 8 |> filled (rgb 200 200 200) |> rotate (degrees 180) |> move (-13,0)      -- Back button
                  ,    text "Back" |> centered |> size 8 |> filled black|> move(-4, -3)
                  ]    |> move (-75, 50 ) |> notifyTap ToTitle
            ]
            
        Game  ->
            [ grass
            , group [ allTrees, bushes, completeRoad ] |> move (0, repeatDistance (-50 * (if (model.time - model.backgroundTimer) < 2 then 3.5 else 1)) 820 350 (model.time - model.startTime))  --  -- x position , repeatDistance (y speed) (y distance) (y start position) model.time
            , if (model.difficulty == Easy) then easyCarTopDown else if (model.difficulty == Medium) then mediumCarTopDown else hardCarTopDown
            , line (-50,50) (50,50) |> outlined (solid 1) white
            , circle 3 |> filled blue |> move (model.goodGuy,50)
            , line (-50,40) (50,40) |> outlined (solid 1) white
            , circle 3 |> filled red |> move (-50 +((model.time - model.startTime) * model.badGuySpeed),40)   
            , finishLine |> move (50,45)
            , text (getQuestionQuestions (getQuestion (List.head model.questions))) |> centered|> filled white |> move (0,20)
            , textBox (model.typing == Typing) model.answerBox |> move (0,10) |> notifyTap Focus
            , group
                  [    rect 20 10  |> filled (rgb 200 200 200),
                       triangle 8 |> filled (rgb 200 200 200) |> rotate (degrees 180) |> move (-13,0)      -- Back button
                  ,    text "Back" |> centered |> size 8 |> filled black|> move(-4, -3)
                  ]    |> move (-75, 50)  |> notifyTap ToDifficulty
            , group
                  [    rect 20 10 |> filled (rgb 200 200 200),
                       triangle 8 |> filled (rgb 200 200 200) |> move (13,0)
                  ,    text "Hint" |> centered|> size 8|> filled black|> move(4, -3)
                  ]    |> move (75, -50)  |> notifyTap ToVisualizer
            , if (model.winState == Won) then gameWon else if (model.winState == Lost) then gameLost else [] |> group
            , correctSignifier model |> move (0,-30) |> makeTransparent (if ((model.time - model.indicationTimer) < 2 && model.winState == Other) then 1 else 0)
            , text (if model.winState == Other then "(Enter a number from your keyboard, then press enter)" else "") |> centered |> size 3.6 |> filled white |> move (0,-4)
            ]
        
        Visualizer  ->
            [ square 300 |> filled black
            , text (getQuestionQuestions (getQuestion (List.head model.questions))) |> centered|> filled white |> move (0,45)
            , visualizer model
            , group
                  [    rect 20 10  |> filled (rgb 200 200 200),
                       triangle 8 |> filled (rgb 200 200 200) |> rotate (degrees 180) |> move (-13,0)      -- Back button
                  ,    text "Back" |> centered |> size 8 |> filled black|> move(-4, -3)
                  ]    |> move (-75, 50)  |> notifyTap ToGame
            , if (getQuestion (List.head model.questions)).op == "+" then text ("There are " ++ Debug.toString(Tuple.first(numOrder (toFloat (getQuestion (List.head model.questions)).q1) (toFloat (getQuestion (List.head model.questions)).q2))) ++ " blue balls, and " ++ Debug.toString(Tuple.second(numOrder (toFloat (getQuestion (List.head model.questions)).q1) (toFloat (getQuestion (List.head model.questions)).q2))) ++ " green balls, how many are there in total?") |> centered |> size 6 |> filled white |> move (0,-55) else [] |> group
            , if (getQuestion (List.head model.questions)).op == "-" then text ("There are " ++ Debug.toString(Tuple.second(numOrder (toFloat (getQuestion (List.head model.questions)).q1) (toFloat (getQuestion (List.head model.questions)).q2))) ++ " total balls, after subtracting " ++ Debug.toString(Tuple.first(numOrder (toFloat (getQuestion (List.head model.questions)).q1) (toFloat (getQuestion (List.head model.questions)).q2))) ++ " red balls, how many are left?") |> centered |> size 6 |> filled white |> move (0,-55) else [] |> group
            , if (getQuestion (List.head model.questions)).op == "*" then text ("There are " ++ (Debug.toString((Tuple.first(numOrder (toFloat (getQuestion (List.head model.questions)).q1) (toFloat (getQuestion (List.head model.questions)).q2))))) ++ " rows of " ++ Debug.toString(Tuple.second(numOrder (toFloat (getQuestion (List.head model.questions)).q1) (toFloat (getQuestion (List.head model.questions)).q2))) ++ " balls, how many are there in total?") |> centered |> size 7 |> filled white |> move (0,-55) else [] |> group
            , if (getQuestion (List.head model.questions)).op == "/" then text (Debug.toString(Tuple.second(numOrder (toFloat (getQuestion (List.head model.questions)).q1) (toFloat (getQuestion (List.head model.questions)).q2))) ++ " total balls can be split up into how many rows of " ++ Debug.toString(Tuple.first(numOrder (toFloat (getQuestion (List.head model.questions)).q1) (toFloat (getQuestion (List.head model.questions)).q2))) ++ "?") |> centered |> size 7 |> filled white |> move (0,-55) else [] |> group
            , if (model.winState == Lost) then gameLost else if (model.winState == Won) then gameWon else [] |> group
            ]            
            
            
gameWon = group 
          [ rect 300 300 |> filled black
          , checkeredFlag
          , text "Congratulations, You WIN!" |> centered |> size 12 |> filled white |> move (0,45)
          , text "Click Play again to try a harder difficulty" |> centered |> size 6 |> filled white |> move (0,-30)
          , text "or go for a victory lap and practise your new math skills" |> centered |> size 6 |> filled white |> move (0,-40)
          , group 
              [    roundedRect 45 10 5 |> filled darkGrey,
                   text "Play again" |> centered |> size 8 |> filled black|> move(0, -3) 
              ]    |> move (0,-50) |> notifyTap ToDifficulty
          
          ]
         
gameLost = group
          [ rect 300 300 |> filled black
          , gameOver
          , text "You Were Too Slow!" |> centered |> size 12 |> filled white |> move (0,45)
          , text "Click Play again to try an easier difficulty" |> centered |> size 6 |> filled white |> move (0,-30)
          , text "or go for a rematch and keep practising your math skills" |> centered |> size 6 |> filled white |> move (0,-40)
          , group 
              [    roundedRect 45 10 5 |> filled darkGrey,
                   text "Play again" |> centered |> size 8 |> filled black|> move(0, -3) 
              ]    |> move (0,-50) |> notifyTap ToDifficulty
          
          ]
------------------------------------------------------------------------------------------
-- Update & MSG
------------------------------------------------------------------------------------------

type Msg = Tick Float 
         | WindowResize (Maybe ( Float, Float )) -- DO NOT TOUCH (UNUSED)
         | ReturnPosition (( Float, Float ) -> Msg) ( Float, Float ) -- DO NOT TOUCH (UNUSED)
         | NoOp -- DO NO TOUCH (UNUSED)
         
         | ToTitle 
         | ToGame 
         | ToDifficulty 
         | ToVisualizer 
         | ToTutorial 
         | DifficultyEasy
         | DifficultyMedium
         | DifficultyHard
         | KeyDown String
         | KeyUp String
         | Focus
         | Highlight Int
         
update msg model = 
  case msg of
    ToTitle  ->
            case model.state of
                Difficulty  -> ({ model | state = TitleScreen }, Cmd.none)
                Tutorial  -> ({ model | state = TitleScreen }, Cmd.none)
                otherwise -> (model, Cmd.none)
        
    ToGame  ->
            case model.state of
                Difficulty  -> ({ model | state = Game , typing = Typing, startTime = model.time, goodGuy = -50, winState = Other}, Cmd.none)
                Visualizer  -> ({ model | state = Game , typing = Typing}, Cmd.none)
                otherwise -> (model, Cmd.none)
        
    ToDifficulty  ->
            case model.state of
                TitleScreen -> ({ model | state = Difficulty }, Cmd.none)
                Game  -> ({ model | state = Difficulty, typing = NotTyping, answerBox = [] }, Cmd.none)
                Visualizer  -> ({ model | state = Difficulty, typing = NotTyping, answerBox = [] }, Cmd.none)
                otherwise -> (model, Cmd.none)
        
    ToVisualizer  ->
            case model.state of
                Game  -> ({ model | state = Visualizer, typing = NotTyping}, Cmd.none)
                otherwise ->  (model, Cmd.none)    
                
    ToTutorial  ->
            case model.state of
                TitleScreen  -> ({ model | state = Tutorial }, Cmd.none)
                otherwise -> (model, Cmd.none)
        
    DifficultyEasy -> ({ model | difficulty = Easy, questions = (shuffle model easyQuestions), correct = Waiting, badGuySpeed = 1 }, Cmd.none)
        
    DifficultyMedium -> ({ model | difficulty = Medium, questions = (shuffle model mediumQuestions), correct = Waiting, badGuySpeed = 2 }, Cmd.none)
        
    DifficultyHard -> ({ model | difficulty = Hard, questions = (shuffle model hardQuestions), correct = Waiting, badGuySpeed = 3 }, Cmd.none)
  
    Focus -> ({ model | typing = (if model.typing == NotTyping then Typing else NotTyping) }, Cmd.none )
    
    KeyUp _ -> ({ model | questions = if(model.correct == Correct) then (shuffle model (List.drop 1 model.questions)) else model.questions,
                          answerBox = if(model.correct == Correct || model.correct == Incorrect) then [] else model.answerBox,
                          goodGuy = if (model.correct == Correct) then (model.goodGuy + 10) else model.goodGuy,       
                          backgroundTimer = if (model.correct == Correct) then model.time else model.backgroundTimer
                }, Cmd.none)
    
    KeyDown code -> ( { model | answerBox = typeAndDelete model.answerBox code,
                                correct = if (code == "Enter" && (checkAnswer model (getQuestion(List.head (model.questions))) model.answerBox)) then Correct 
                                          else if (code == "Enter") then Incorrect 
                                          else Waiting,
                                indicationTimer = model.time
                      }, Cmd.none)

    Highlight num -> ({ model | highlight = num}, Cmd.none)


    Tick t -> ( { model | time = t,
                          winState = case model.winState of 
                                       Won -> Won
                                       Lost -> Lost
                                       otherwise -> if ((model.state == Game || model.state == Visualizer) && (-50 +((model.time - model.startTime)* model.badGuySpeed) >= 50)) then Lost else if (model.goodGuy >= 50) then Won else Other
                }, Cmd.none )
                
                
-- DO NOT TOUCH (UNUSED) 
------------------------------------------------------------------------------------------    
       
    WindowResize mWH ->
      case mWH of
        Just ( w, h ) ->
          ( { model | window = didResize model.window w h
              }
          , Cmd.none
          )
        -- need to get viewport size after the app starts
        Nothing ->
          ( model
          , getViewportSize
          )
    ReturnPosition message ( x, y ) ->
        let
            ( newModel, userCmds ) =
                update
                    (message (convertCoords model.window ( x, y ) ))
                    model
        in
        ( newModel, userCmds )
    NoOp -> ( model, Cmd.none )   
------------------------------------------------------------------------------------------   

------------------------------------------------------------------------------------------
-- Pictues
------------------------------------------------------------------------------------------
finishLine = group 
  [
    rect 8 25 |> filled black
    ,
    rect 4 5 |> filled white |> move (-2,10)
    ,
    rect 4 5 |> filled white |> move (-2,0)
    ,
    rect 4 5 |> filled white |> move (-2,-10)
    ,
    rect 4 5 |> filled white |> move (2,5)
    ,
    rect 4 5 |> filled white |> move (2,-5)
  
  ]

easyCar = group [
      rect 2 5 |> filled (rgb 79 79 79) |> move (5, 7)
      , rect 2 3 |> filled (rgb 79 79 79) |> move (2, 5)
      , rect 10 20 |> filled darkGreen |> rotate (degrees 90)
      , rect 8 10 |> filled darkGreen |> move (-7,6)
      , rect 4 6 |> filled lightBlue |> rotate (degrees 90) |> move (-6.5,7)

      , circle 6 |> filled (rgb 79 79 79) |> move (-8,-4)
      , circle 3 |> filled yellow |> move (-8, -4)
      , circle 3 |> filled (rgb 79 79 79) |> move (8, -6)
      , circle 1 |> filled yellow |> move (8, -6)
      ]

mediumCar = group [
      wedge 5 0.5 |> filled darkOrange |> rotate (degrees 90) |> move(0,1.5)
      , wedge 4 0.5 |> filled white |> rotate (degrees 90)|> move(0,1.5)
      , roundedRect 6 15 4 |> filled darkOrange |> rotate (degrees 90)
      , circle 2 |> filled (rgb 79 79 79) |> move(3,-2.5)
      , circle 2 |> filled (rgb 79 79 79) |> move(-3,-2.5)
      , circle 1.2 |> filled grey |> move (3,-2.5)
      , circle 1.2 |> filled grey |> move (-3,-2.5)
      , wedge 1 0.5 |> filled yellow |> move(7,0)
      , wedge 1 0.5 |> filled darkRed |> move(7,0) |> rotate (degrees 180)
      ]    

easyCarTopDown = group [
      html 1280 1024 (Html.img [Html.src "https://user-images.githubusercontent.com/102701497/163692673-d3f70576-966c-4b97-b3ff-9da569656603.png"] [])
      ] |> scale 0.06 |> move (5,-15)

mediumCarTopDown = group [
      html 1280 1024 (Html.img [Html.src "https://user-images.githubusercontent.com/102701497/161350312-4f9281c2-840f-44a4-8055-73cf2304a238.png"] [])
      ] |> scale 0.06 |> move (-65, 0) |> rotate (degrees 90)

hardCarTopDown = group [
      html 1280 1024 (Html.img [Html.src "https://user-images.githubusercontent.com/102701497/163692775-21e0c73d-2e13-4f84-8b2f-65d3daa57988.png"] [])
      ] |> scale 0.07 |> move (7, 35) |> rotate (degrees -90)

hardCar = group[
             html 1280 1024 (Html.img [Html.src "https://user-images.githubusercontent.com/102701497/160948507-9aafa1e0-03f3-4eee-862c-c74573b19116.png"] [])
      ] |> scale 0.05 |> scaleX -1

titleScreenBackground = group [
            html 1280 1024 (Html.img [Html.src "https://user-images.githubusercontent.com/102701497/160951123-aafc3da9-2d36-4661-a625-ec3fc98dea02.png"] [])
      ] |> scale 0.3 |> move(-85,63)

checkeredFlag = group [
            html 1280 1024 (Html.img [Html.src "https://user-images.githubusercontent.com/102701497/163518351-9c36d5c7-3e4a-4441-a97f-85bf1388d43c.png"] [])
      ] |> scale 0.15 |> move(-61,45)

gameOver = group [
            html 1280 1024 (Html.img [Html.src "https://user-images.githubusercontent.com/102701497/163634325-68e892ba-15ab-45db-9c9c-15df0a7ad5dd.png"] []) |> scale 1.3|> move (-60, 25)
          , html 1280 1024 (Html.img [Html.src "https://user-images.githubusercontent.com/102701497/163635282-a79cc45e-5f63-420f-b411-7769b7349bc7.png"] []) |> scale 0.3 |> move (-90,-60)
          , html 1280 1024 (Html.img [Html.src "https://user-images.githubusercontent.com/102701497/163635282-a79cc45e-5f63-420f-b411-7769b7349bc7.png"] []) |> scale 0.3 |> move (390,-60)          
      ] |> scale 0.3 |> move(-61,25)






road = group[
    rect 1700 80 |> filled black |> rotate (degrees 90) 
    ]

lines = group[
     rect 20 3 |> filled yellow |> rotate (degrees 90)
    ]

completeRoad = group[
    road
  , lines |> move (0, -660)
  , lines |> move (0, -630)
  , lines |> move (0, -600)
  , lines |> move (0, -570)
  , lines |> move (0, -540)
  , lines |> move (0, -510)
  , lines |> move (0, -480)
  , lines |> move (0, -450)
  , lines |> move (0, -420)
  , lines |> move (0, -390)
  , lines |> move (0, -360)
  , lines |> move (0, -330)
  , lines |> move (0, -300)
  , lines |> move (0, -270)
  , lines |> move (0, -240)
  , lines |> move (0, -210)
  , lines |> move (0, -180)
  , lines |> move (0, -150)
  , lines |> move (0, -120)
  , lines |> move (0, -90)
  , lines |> move (0, -60)
  , lines |> move (0, -30)
  , lines
  , lines |> move (0, 30)
  , lines |> move (0, 60)
  , lines |> move (0, 90)
  , lines |> move (0, 120)
  , lines |> move (0, 150)
  , lines |> move (0, 180)
  , lines |> move (0, 210)
  , lines |> move (0, 240)
  , lines |> move (0, 270)
  , lines |> move (0, 300)
  , lines |> move (0, 330)
  , lines |> move (0, 660)
  , lines |> move (0, 630)
  , lines |> move (0, 600)
  , lines |> move (0, 570)
  , lines |> move (0, 540)
  , lines |> move (0, 510)
  , lines |> move (0, 480)
  , lines |> move (0, 450)
  , lines |> move (0, 420)
  , lines |> move (0, 390)
  , lines |> move (0, 360)
    ]
    
grass = group[
    rect 200 800 |> filled darkGreen
    ]
    
bigbush = group[
      html 1280 1024 (Html.img [Html.src "https://user-images.githubusercontent.com/102701497/161287834-e1b1e242-81c0-433c-8dbf-9798e655c8bc.png"] [])
      ] |> scale 0.1 |> move(-90,63)
   
smallbush = group[
      html 1280 1024 (Html.img [Html.src "https://user-images.githubusercontent.com/102701497/161287819-ebea4f44-c24a-4ab3-b6f7-33047bf283bf.png"] [])
      ] |> scale 0.1 |> move(-90,63)

bushes = group[
       bigbush |> move (5, 500)
      , smallbush |> move (0, 470)
      , bigbush |> move (5, 360)
      , smallbush |> move (0, 330)      
      , bigbush |> move (5, 220)
      , smallbush |> move (0, 190)
      , bigbush |> move (5, 80)
      , smallbush |> move (0, 50)
      , bigbush |> move (5, -60)
      , smallbush |> move (0, -90)
      , bigbush |> move (5, -200)
      , smallbush |> move (0, -230)
      , bigbush |> move (5, -340)
      , smallbush |> move (0, -370)
      , bigbush |> move (5, -480)
      , smallbush |> move (0, -510)
      , bigbush |> move (130,290)
      , smallbush |> move (160, 260)
      , bigbush |> move (130,150)
      , smallbush |> move (160, 120)
      , bigbush |> move (130,10)
      , smallbush |> move (160, -20)
      , bigbush |> move (130, -130)
      , smallbush |> move (160, -160)
      , bigbush |> move (130, -270)
      , smallbush |> move (160, -300)
      , bigbush |> move (130, -410)
      , smallbush |> move (160, -440)
      , bigbush |> move (130, -550)
      , smallbush |> move (160, -580)]
   
tree = group[
    html 1280 1024 (Html.img [Html.src "https://user-images.githubusercontent.com/102701497/161285816-44036f99-8aee-4e05-8ae4-bf411df1799a.png"] [])
    ] |> scale 0.1 |> move(-90,63)
    
allTrees = group[
      tree |> move (0, -420)
    , tree |> move (135, -350)
    , tree |> move (0, -280)
    , tree |> move (135, -210)
    , tree |> move (0, -140)
    , tree |> move (135, -70)
    , tree
    , tree |> move (135, 70)
    , tree |> move (0, 140)
    , tree |> move (135, 210)
    , tree |> move(0, 280)
    , tree |> move (135, 350)
    , tree |> move(0, 420)
    ]

------------------------------------------------------------------------------------------
-- Questions
------------------------------------------------------------------------------------------

--easy questions
easyQuestions = [e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14,e15,e16,e17,e18,e19,e20]
e1 = {q1 = 3, q2 = 3, answer = "6", op = "+"}
e2 = {q1 = 2, q2 = 6, answer = "8", op = "+"}
e3 = {q1 = 10, q2 = 5, answer = "5", op = "-"}
e4 = {q1 = 10, q2 = 10, answer = "20", op = "+"}
e5 = {q1 = 9, q2 = 1, answer = "10", op = "+"}
e6 = {q1 = 8, q2 = 8, answer = "0", op = "-"}
e7 = {q1 = 9, q2 = 1, answer = "8", op = "-"}
e8 = {q1 = 4, q2 = 14, answer = "18", op = "+"}
e9 = {q1 = 12, q2 = 0, answer = "12", op = "-"}
e10 = {q1 = 7, q2 = 3, answer = "10", op = "+"}
e11 = {q1 = 8, q2 = 3, answer = "5", op = "-"}
e12 = {q1 = 5, q2 = 4, answer = "9", op = "+"}
e13 = {q1 = 14, q2 = 7, answer = "7", op = "-"}
e14 = {q1 = 4, q2 = 9, answer = "13", op = "+"}
e15 = {q1 = 6, q2 = 1, answer = "7", op = "+"}
e16 = {q1 = 9, q2 = 3, answer = "12", op = "+"}
e17 = {q1 = 7, q2 = 5, answer = "12", op = "+"}
e18 = {q1 = 4, q2 = 14, answer = "18", op = "+"}
e19 = {q1 = 7, q2 = 7, answer = "14", op = "+"}
e20 = {q1 = 9, q2 = 2, answer = "7", op = "-"}

--medium questions
mediumQuestions = [m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19,m20]
m1 = {q1 = 3, q2 = 5, answer = "15", op = "*"}
m2 = {q1 = 12, q2 = 2, answer = "14", op = "+"}
m3 = {q1 = 9, q2 = 5, answer = "4", op = "-"}
m4 = {q1 = 4, q2 = 5, answer = "20", op = "*"}
m5 = {q1 = 3, q2 = 9, answer = "27", op = "*"}
m6 = {q1 = 5, q2 = 7, answer = "35", op = "*"}
m7 = {q1 = 24, q2 = 8, answer = "16", op = "-"}
m8 = {q1 = 9, q2 = 11, answer = "20", op = "+"}
m9 = {q1 = 8, q2 = 4, answer = "32", op = "*"}
m10 = {q1 = 25, q2 = 6, answer = "19", op = "-"}
m11 = {q1 = 20, q2 = 2, answer = "22", op = "+"}
m12 = {q1 = 21, q2 = 9, answer = "12", op = "-"}
m13 = {q1 = 6, q2 = 3, answer = "18", op = "*"}
m14 = {q1 = 14, q2 = 8, answer = "6", op = "-"}
m15 = {q1 = 4, q2 = 4, answer = "16", op = "*"}
m16 = {q1 = 6, q2 = 3, answer = "18", op = "*"}
m17 = {q1 = 21, q2 = 3, answer = "18", op = "-"}
m18 = {q1 = 15, q2 = 5, answer = "10", op = "-"}
m19 = {q1 = 2, q2 = 8, answer = "16", op = "*"}
m20 = {q1 = 3, q2 = 4, answer = "12", op = "*"}



--hard questions
hardQuestions = [h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11,h12,h13,h14,h15,h16,h17,h18,h19,h20]
h1 = {q1 = 11, q2 = 2, answer = "22", op = "*"}
h2 = {q1 = 3, q2 = 12, answer = "36", op = "*"}
h3 = {q1 = 32, q2 = 8, answer = "4", op = "/"}
h4 = {q1 = 45, q2 = 9, answer = "5", op = "/"}
h5 = {q1 = 37, q2 = 19, answer = "18", op = "-"}
h6 = {q1 = 18, q2 = 3, answer = "54", op = "*"}
h7 = {q1 = 19, q2 = 15, answer = "34", op = "+"}
h8 = {q1 = 7, q2 = 4, answer = "28", op = "*"}
h9 = {q1 = 27, q2 = 6, answer = "21", op = "-"}
h10 = {q1 = 3, q2 = 14, answer = "42", op = "*"}
h11 = {q1 = 8, q2 = 6, answer = "48", op = "*"}
h12 = {q1 = 19, q2 = 19, answer = "38", op = "+"}
h13 = {q1 = 27, q2 = 9, answer = "3", op = "/"}
h14 = {q1 = 18, q2 = 12, answer = "30", op = "+"}
h15 = {q1 = 21, q2 = 7, answer = "14", op = "-"}
h16 = {q1 = 14, q2 = 6, answer = "20", op = "+"}
h17 = {q1 = 19, q2 = 0, answer = "0", op = "*"}
h18 = {q1 = 0, q2 = 0, answer = "0", op = "*"}
h19 = {q1 = 17, q2 = 6, answer = "23", op = "+"}
h20 = {q1 = 15, q2 = 3, answer = "45", op = "*"}


------------------------------------------------------------------------------------------
-- Main & View Are Hidden
------------------------------------------------------------------------------------------