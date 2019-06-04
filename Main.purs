-- Can use this on http://try.purescript.org/?backend=flare

module Main where
import Prelude

import Data.Number (fromString)
import Data.Int as DI
import Data.Maybe (fromMaybe)

import Text.Smolder.HTML as H
import Text.Smolder.Markup ((!), text)
import Text.Smolder.HTML.Attributes as A

import Color (black, toHexString)

import Flare.Smolder (runFlareHTML)
import Flare

main = runFlareHTML "controls" "output" ui

ui = markup <$> string "OPT" "5000"
            <*> string "PRC" "10.00"
            <*> string "FMV" "100.00"
            <*> string "STC" "0.24"
            <*> string "LTC" "0.15"
            <*> intSlider "" 0 5000 2500

markup opt price fmv stc ltc slider = do
  let p_opt = fromMaybe 0.0 $ fromString opt
  let p_fmv = fromMaybe 0.0 $ fromString fmv
  let p_prc = fromMaybe 0.0 $ fromString price

  let p_stc = 1.0 - (fromMaybe 0.0 $ fromString stc)
  let p_ltc = 1.0 - (fromMaybe 0.0 $ fromString ltc)
  let p_slider = DI.toNumber slider


  let p_value = p_fmv - p_prc
  let full_value = p_value * p_opt
  let max_ltc_value = full_value * p_ltc
  let max_ltc_cash  = p_opt * p_prc
  let stc_opt = p_slider
  let ltc_opt = p_opt - p_slider
  let stc_value = p_value * stc_opt * p_stc
  let ltc_value = p_value * ltc_opt * p_ltc
  let stc_cash  = stc_opt * p_prc
  let ltc_cash = ltc_opt * p_prc

  let d_value = max_ltc_value - ltc_value
  let d_cash  = max_ltc_cash - ltc_cash

  H.div do
    H.p $ text ("full value : " <> show full_value)
    H.p $ text ("max value : " <> show max_ltc_value)
    H.p $ text ("stc opt: " <> show stc_opt)
    H.p $ text ("stc value : " <> show stc_value)
    H.p $ text ("stc cash : " <> show stc_cash)
    H.p $ text ("ltc opt: " <> show ltc_opt)
    H.p $ text ("ltc value : " <> show ltc_value)
    H.p $ text ("ltc cash: " <> show ltc_cash)
    H.br
    H.p $ text ("potential loss : " <> show d_value)
    H.p $ text ("potential cash diff : " <> show d_cash)
    H.p $ text ("potential loss - stc value : " <> show (d_value - stc_value))
    H.p $ text ("full potential loss : " <> show (d_value - stc_value - d_cash))
