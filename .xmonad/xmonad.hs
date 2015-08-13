import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.SetWMName

import XMonad.Hooks.DynamicLog 
import XMonad.Hooks.ManageDocks 
import XMonad.Util.Run 
import XMonad.Util.EZConfig(additionalKeys)

import System.IO
import Data.List

main = do
    setupMonitors
    xmproc <- spawnPipe "xmobar"
    xmonad $ desktopConfig
        { terminal    = "urxvt"
        , modMask     = mod4Mask
        , startupHook = setWMName "LG3D"
        , manageHook  = manageDocks <+> manageHook defaultConfig 
        , layoutHook  = avoidStruts $ layoutHook defaultConfig 
        , logHook     = dynamicLogWithPP xmobarPP 
            { ppOutput = hPutStrLn xmproc 
            , ppTitle  = xmobarColor "blue" "" . shorten 50 
            , ppLayout = const ""
            } 
        , normalBorderColor  = "#000000" -- black
        , focusedBorderColor = "#E0E0E0" -- grey
        } `additionalKeys` myKeyBindings

myKeyBindings = 
    [ ((mod4Mask .|. shiftMask, xK_f), spawn "google-chrome")
    , ((mod4Mask .|. shiftMask, xK_g), spawn "sh /opt/idea/bin/idea.sh")
    , ((0, 0x1008FF11), spawn "amixer set Master 2-")
    , ((0, 0x1008FF13), spawn "amixer set Master 2+")
    , ((0, 0x1008FF12), spawn "amixer set Master toggle")
    ]

-- Setup physical monitors
setupMonitors =
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
