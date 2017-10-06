module GameServerSpec where

import qualified Test.Tasty as Tasty
import Test.Tasty.Hspec

import Hive.Server.Game

gameServerSpec :: Spec
gameServerSpec = describe "Game Server" $ do
    with (return $ serve gameAPI gameServer)
    -- post to make an open game
    -- verify that it's in the open game list
    -- verify that we can't post a move to a still-open game
    -- join the game as another user
    -- black can't post a move
    -- white posts an invalid move
    -- white posts a valid move
    -- white can't post another move right now
    -- black posts a move
    -- some other rando user can't post a move
    -- unauthorized user can't post a move
    -- websocket is notified when a move is made
        -- might be hard to test?


