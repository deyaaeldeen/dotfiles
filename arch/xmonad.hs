import XMonad

import XMonad.Util.EZConfig
import XMonad.Hooks.EwmhDesktops hiding (fullscreenEventHook)


main :: IO ()
main = xmonad $ ewmh defaultConfig
         { modMask = mod4Mask
         , terminal = "xterm -e tmux attach"
         }
         `additionalKeys`
         [((mod4Mask, xK_e), spawn "emc")
         ,((mod4Mask, xK_g), spawn "google-chrome-stable")]
