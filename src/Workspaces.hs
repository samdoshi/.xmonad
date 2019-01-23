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

vmWS :: WorkspaceId
vmWS = "vm"

floatWS :: WorkspaceId
floatWS = "float"

minimisedWS :: WorkspaceId
minimisedWS = "minimised"

workspaces :: [WorkspaceId]
workspaces = [ homeWS, alphaWS, betaWS, mediaWS, floatWS, vmWS
             , "7", "8", "9"
             , minimisedWS
             ]
