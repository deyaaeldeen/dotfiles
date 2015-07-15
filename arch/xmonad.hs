import XMonad

import XMonad.Util.EZConfig
import XMonad.Hooks.EwmhDesktops hiding (fullscreenEventHook)
import XMonad.Actions.GridSelect


main :: IO ()
main = xmonad $ ewmh defaultConfig
         { modMask = mod4Mask
         , terminal = "xterm -e tmux"
         }
         `additionalKeys`
         [((mod4Mask, xK_e), spawn "emc")
         ,((mod4Mask, xK_g), spawn "google-chrome-stable")]
