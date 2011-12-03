import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)
import Application          (withMeToo)
import Prelude              (IO)

main :: IO ()
main = defaultMain fromArgs withMeToo