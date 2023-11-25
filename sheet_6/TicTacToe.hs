module TicTacToe where
    import Prelude hiding (lookup)
    import Data.Map (Map, empty, member, insert, lookup)
    import Data.Maybe (fromMaybe)

    -- convert grid position to string
    giToStr :: Map Int String -> Int -> String
    giToStr g i = fromMaybe " " (lookup i g)

    -- game type
    data Game = Game (Map Int String) String String String Int

    -- print game state, grid and turn
    instance Show Game where
        show :: Game -> String
        show (Game g s p1 p2 t) = s
            ++ "\n " ++ giToStr g 1 ++ " | " ++ giToStr g 2 ++ " | " ++ giToStr g 3
            ++ "\n-----------"
            ++ "\n " ++ giToStr g 4 ++ " | " ++ giToStr g 5 ++ " | " ++ giToStr g 6
            ++ "\n-----------"
            ++ "\n " ++ giToStr g 7 ++ " | " ++ giToStr g 8 ++ " | " ++ giToStr g 9
            ++ "\nturn: " ++ show t

    -- initialize a new game
    newGame :: String -> String -> Game
    newGame p1 p2 = Game Data.Map.empty "game hasn't finished yet" p1 p2 0

    -- check if player p has won in given grid g
    hasWon :: Map Int String -> String -> Bool
    hasWon g p
        | lookup 1 g == Just p && lookup 2 g == Just p && lookup 3 g == Just p = True
        | lookup 4 g == Just p && lookup 5 g == Just p && lookup 6 g == Just p = True
        | lookup 7 g == Just p && lookup 8 g == Just p && lookup 9 g == Just p = True
        | lookup 1 g == Just p && lookup 4 g == Just p && lookup 7 g == Just p = True
        | lookup 2 g == Just p && lookup 5 g == Just p && lookup 8 g == Just p = True
        | lookup 3 g == Just p && lookup 6 g == Just p && lookup 9 g == Just p = True
        | lookup 1 g == Just p && lookup 5 g == Just p && lookup 9 g == Just p = True
        | lookup 3 g == Just p && lookup 5 g == Just p && lookup 7 g == Just p = True
        | otherwise = False

    -- game logic
    doTurn :: Game -> Int -> Game
    doTurn (Game g s p1 p2 t) m
        | s == "player 1 won" = Game g s p1 p2 t
        | s == "player 2 won" = Game g s p1 p2 t
        | s == "draw"         = Game g s p1 p2 t
        | t >= 9              = Game g "game is already over" p1 p2 t
        | m < 0 || m > 9      = Game g "invalid grid position" p1 p2 t
        | member m g          = Game g "place already has a token" p1 p2 t
        | otherwise = if even t
            then Game (insert m p1 g)
                (if hasWon (insert m p1 g) p1 then "player 1 won" else if (t + 1) == 9 then "draw" else s) p1 p2 (t + 1)
            else Game (insert m p2 g)
                (if hasWon (insert m p2 g) p2 then "player 2 won" else if (t + 1) == 9 then "draw" else s) p1 p2 (t + 1)

    -- apply moves
    doTurns :: Game -> [Int] -> Game
    doTurns g tl
        | null tl   = g
        | otherwise = doTurns (doTurn g (head tl)) (tail tl)
