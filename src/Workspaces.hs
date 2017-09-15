module Workspaces where

import           XMonad.Core (WorkspaceId)

homeWS :: WorkspaceId
homeWS = "home"

alphaWS :: WorkspaceId
alphaWS = "alpha"

betaWS :: WorkspaceId
betaWS = "beta"

mediaWS :: WorkspaceId
mediaWS = "media"

gamesWS :: WorkspaceId
gamesWS = "games"

floatWS :: WorkspaceId
floatWS = "float"

minimisedWS :: WorkspaceId
minimisedWS = "minimised"

nspWS :: WorkspaceId
nspWS = "NSP"

workspaces :: [WorkspaceId]
workspaces = [ homeWS, alphaWS, betaWS, mediaWS, floatWS, gamesWS
             , "7", "8", "9"
             , minimisedWS, nspWS
             ]
