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

import XMonad.Actions.Volume
import XMonad.Util.Dzen

import XMonad.Layout.PerWorkspace
import XMonad.Hooks.ManageHelpers

import XMonad.Layout.Fullscreen

import System.Environment
import XMonad.Util.WindowProperties (getProp32s)

main = do
    setupMonitors
    setAutoLock 5 -- auto lock screen in 5 minutes
    myAutoStartApps
    xmproc <- spawnPipe "xmobar"
    xmonad $ desktopConfig
        { terminal    = "urxvt"
        , modMask     = mod4Mask
        -- , startupHook = setWMName "LG3D"
        -- , layoutHook  = smartBorders $ layoutHook desktopConfig 
        , handleEventHook    = fullscreenEventHook
        , layoutHook  = myLayoutHook
        , logHook     = dynamicLogWithPP xmobarPP 
            { ppOutput = hPutStrLn xmproc 
            --, ppTitle  = xmobarColor "blue" "" . shorten 50 
            , ppLayout = const ""
            } 
        , manageHook  = myManageHook
        } `additionalKeys` myKeyBindings

myModMask = mod4Mask

getWorkingDirectory w = do
    optPid <- getProp32s "_NET_WM_PID" w
    case optPid of
        Just pid -> return $ show pid
        Nothing -> io $ getEnv "HOME"

test window = do
    n <- getWorkingDirectory window
    --n <- workingDirectory window
    dzenConfig centered2 (n)

centered2 =
        onCurr (center 500 66)
    >=> font "-*-helvetica-*-r-*-*-64-*-*-*-*-*-*-*"
    >=> addArgs ["-fg", "#80c0ff"]
    >=> addArgs ["-bg", "#000040"]

myKeyBindings = 
    [ ((myModMask .|. shiftMask, xK_f), spawn "chromium-browser")
    , ((myModMask .|. shiftMask, xK_g), spawn "_JAVA_AWT_WM_NONREPARENTING=1 /opt/idea/bin/idea.sh")
    , ((myModMask .|. shiftMask, xK_l), spawn myScreensaver)
    --, ((myModMask, ), toggleMute >>= muteAlert)
    , ((myModMask, xK_F11), lowerVolume 2 >>= volumeAlert)
    , ((myModMask, xK_F12), raiseVolume 2 >>= volumeAlert)
    ]
    ++
    -- compensate the incorrect order of 
    -- physical screens by rebinding keys
    [((mod4Mask .|. mask, key), f sc)
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)]]

-- manage hooks
myManageHook = composeAll 
    [ manageDocks
    , isFullscreen --> doFullFloat
    , className =? "Zathura" --> doFullFloat
    , className =? "Thunderbird" --> doShift "9"
    , className =? "Slack" --> doShift "9"
    , manageHook desktopConfig
    ]

-- layout hooks
myLayoutHook = 
    fullscreenFull $ onWorkspace "9" Full $ 
        smartBorders $ layoutHook desktopConfig

-- auto start applications
myAutoStartApps =
    mapM_ spawn
        [ "thunderbird"
        , "slack"
        ]

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
    when (count == 2) $
        spawn $ concat $ intersperse " " 
            [ "xrandr"
            , "--output VGA-1"
            , "--off"
            , "--output DVI-D-1"
            , "--mode 1920x1080"
            , "--pos 0x0"
            , "--rotate normal"
            , "--output HDMI-1"
            , "--primary"
            , "--mode 1920x1080"
            , "--pos 1920x0"
            , "--rotate norma"
            ]

getMonitorCount = do
    (_, Just hout, _, _) <-
        createProcess (proc "xrandr" ["-q"]) 
            { env = Just [("DISPLAY", ":0")]
            , std_out = CreatePipe }
    length . filter (isInfixOf " connected") . lines <$> hGetContents hout

-- volume control
muteAlert True  = dzenConfig centered "On"
muteAlert False = dzenConfig centered "Off"

volumeAlert = dzenConfig centered . show . round

centered =
        onCurr (center 150 66)
    >=> font "-*-helvetica-*-r-*-*-64-*-*-*-*-*-*-*"
    >=> addArgs ["-fg", "#80c0ff"]
    >=> addArgs ["-bg", "#000040"]

-- The command to lock the screen or show the screensaver.
myScreensaver = "/usr/bin/gnome-screensaver-command --lock"

type Minute = Int

setAutoLock :: Minute -> IO ()
setAutoLock min = do
    spawn $ "xautolock -time " ++ show min ++ " -locker \"gnome-screensaver-command -l\" -notify 10 -notifier \"notify-send -t 5000 -i gtk-dialog-info 'Locking in 10 seconds'\""
