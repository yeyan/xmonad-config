import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.SetWMName

import XMonad.Hooks.DynamicLog 
import XMonad.Hooks.ManageDocks 
import XMonad.Util.Run 
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Layout.NoBorders

import XMonad.Actions.PhysicalScreens

import System.IO
import Data.List

import Control.Monad
import Control.Applicative
import System.Process

main = do
    setupMonitors
    xmproc <- spawnPipe "xmobar"
    xmonad $ desktopConfig
        { terminal    = "urxvt"
        , modMask     = mod4Mask
        , startupHook = setWMName "LG3D"
        , layoutHook  = smartBorders $ layoutHook desktopConfig 
        , logHook     = dynamicLogWithPP xmobarPP 
            { ppOutput = hPutStrLn xmproc 
            , ppTitle  = xmobarColor "blue" "" . shorten 50 
            , ppLayout = const ""
            } 
        } `additionalKeys` myKeyBindings

myKeyBindings = 
    [ ((mod4Mask .|. shiftMask, xK_f), spawn "google-chrome")
    , ((mod4Mask .|. shiftMask, xK_g), spawn "sh /opt/idea/bin/idea.sh")
    , ((mod4Mask .|. shiftMask, xK_h), spawn "/opt/eclipse/eclipse")
    , ((0, 0x1008FF11), spawn "amixer set Master 2-")
    , ((0, 0x1008FF13), spawn "amixer set Master 2+")
    , ((0, 0x1008FF12), spawn "amixer set Master toggle")
    ]
    ++
    -- compensate the incorrect order of 
    -- physical screens by rebinding keys
    [((mod4Mask .|. mask, key), f sc)
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)]]

-- Setup physical monitors
setupMonitors = do
    count <- getMonitorCount 
    when (count == 3) $
        spawn $ concat $ intersperse " "
            [ "xrandr"
            , "--output HDMI1"
            , "--mode 1920x1080"
            , "--pos 0x0"
            , "--rotate normal"
            , "--output LVDS1"
            , "--off"
            , "--output VIRTUAL1"
            , "--off "
            , "--output DP1"
            , "--off "
            , "--output VGA1"
            , "--mode 1920x1080"
            , "--pos 1920x0"
            , "--rotate normal"
            ]

getMonitorCount = do
    (_, Just hout, _, _) <-
        createProcess (proc "xrandr" ["-q"]) 
            { env = Just [("DISPLAY", ":0")]
            , std_out = CreatePipe }
    length . filter (isInfixOf " connected") . lines <$> hGetContents hout
    
