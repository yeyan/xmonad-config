import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Fullscreen
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing 
import XMonad.Config.Xfce
import System.IO

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ desktopConfig
        { terminal    = "urxvt"
        , logHook     = dynamicLogWithPP xmobarPP
            { ppOutput = hPutStrLn xmproc
            , ppTitle  = xmobarColor "green" "". shorten 50
            }
        , modMask     = myModMask
        , borderWidth = 3
        , manageHook  = myManageHook
        , layoutHook  = smartBorders $ layoutHook desktopConfig
        --, layoutHook  = smartBorders $ avoidStruts myLayout
        } `additionalKeys` myKeyBindings 

myModMask = mod4Mask

myManageHook = composeAll
    [ isFullscreen --> doFullFloat 
    , className =? "Sxiv" --> doFullFloat
    , className =? "MuPDF" --> doFullFloat
    , className =? "MComix" --> doFullFloat
    , manageHook desktopConfig
    ]

myKeyBindings = 
    [ ((myModMask .|. shiftMask, xK_f), spawn "firefox")
    , ((0, 0x1008FF11), spawn "amixer set Master 2-")
    , ((0, 0x1008FF13), spawn "amixer set Master 2+")
    , ((0, 0x1008FF12), spawn "amixer set Master toggle")
    ]

myLayout = tiled ||| Mirror tiled ||| Full
    where
        tiled   = spacing 5 $ Tall nmaster delta ratio
        nmaster = 1
        ratio   = 1/2
        delta   = 3/100
