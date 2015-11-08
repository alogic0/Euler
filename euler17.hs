import qualified Text.Numeral.Language.EN as EN
import Text.Numeral.Grammar.Reified ( defaultInflection )
import qualified Data.Text as T
import Data.Char
import Data.Maybe

-- it works in ghci, gives 21124
--
-- let euler17 = sum $ map (length . filter isAlpha . (T.unpack)) $ fromJust $ sequence (map (EN.gb_cardinal defaultInflection) [1 .. 1000] :: [Maybe T.Text])


