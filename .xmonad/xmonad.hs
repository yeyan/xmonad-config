import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Fullscreen
import XMonad.Util.Run(spawnPipe, safeSpawn)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing 
import System.IO

import Control.Monad (void)
import XMonad.Actions.Volume
import XMonad.Util.Dzen

import XMonad.Wallpaper
import XMonad.Layout.Fullscreen 

main = do
    setRandomWallpaper ["$HOME/Pictures/Wallpapers"]
    xmproc <- spawnPipe "xmobar"
    xmonad $ desktopConfig
        { terminal    = "urxvt"
        , logHook     = dynamicLogWithPP xmobarPP
            { ppOutput = hPutStrLn xmproc
            , ppTitle  = xmobarColor "green" "". shorten 50
            }
        , modMask     = myModMask
        , borderWidth = 3
        , handleEventHook = myEventHook
        , manageHook  = myManageHook
        , layoutHook  = myLayoutHook
        --, layoutHook  = smartBorders $ avoidStruts myLayout
        } `additionalKeys` myKeyBindings 

myModMask = mod4Mask

myEventHook = mempty

myManageHook = composeAll
    [ isFullscreen --> doFullFloat 
    , className =? "Sxiv" --> doFullFloat
    , className =? "MuPDF" --> doFullFloat
    , className =? "MComix" --> doFullFloat
    , className =? "Zathura" --> doFullFloat
    , manageHook desktopConfig
    ]

myLayoutHook = 
    fullscreenFull $ smartBorders $ layoutHook desktopConfig

centered :: DzenConfig
centered =
        --onCurr (center 150 66)
        onCurr (center 160 66)
    >=> font "-*-helvetica-*-r-*-*-64-*-*-*-*-*-*-*"
    >=> addArgs ["-fg", "#80c0ff"]
    >=> addArgs ["-bg", "#000040"]

alert = dzenConfig centered

xAlert message = dzenConfig (centered $ length message) message
    where
        centered len =
            onCurr (center (50 * len) 66)
            >=> font "-*-dejavu sans mono-*-r-*-*-64-*-*-*-*-*-*-*"
            >=> addArgs ["-fg", "#80c0ff"]
            >=> addArgs ["-bg", "#000040"]

myToggleMute =
    toggleMute >>= xAlert . (\val -> if val then "Disabled" else "Enabled")

myKeyBindings = 
    [ ((myModMask .|. shiftMask, xK_f), spawn "chromium")
    , ((myModMask .|. shiftMask, xK_g), spawn "_JAVA_AWT_WM_NONREPARENTING=1 idea.sh")
    , ((0, 0x1008FF11), lowerVolume 2 >>= alert . show . round)
    , ((0, 0x1008FF13), raiseVolume 2 >>= alert . show . round)
    , ((0, 0x1008FF12), myToggleMute)
    ]

myLayout = tiled ||| Mirror tiled ||| Full
    where
        tiled   = spacing 5 $ Tall nmaster delta ratio
        nmaster = 1
        ratio   = 1/2
        delta   = 3/100

