module Workspaces where

import           XMonad.Core (WorkspaceId)

import           Machines    (Machine (..))

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

hledgerWS :: WorkspaceId
hledgerWS = "hledger"

minimisedWS :: WorkspaceId
minimisedWS = "minimised"

workspaces :: Machine -> [WorkspaceId]
workspaces Carbon = [ homeWS, alphaWS, betaWS, mediaWS, floatWS, vmWS
                    , "7", "8"
                    , hledgerWS, minimisedWS
                    ]
workspaces _      = [ homeWS, alphaWS, betaWS, mediaWS, floatWS, vmWS
                    , "7", "8", "9"
                    , minimisedWS
                    ]
