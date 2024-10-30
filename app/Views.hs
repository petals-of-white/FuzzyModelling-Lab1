module Views where

import qualified Data.Map                    as Map
import           Fuzzy.Base
import           Fuzzy.FiniteUniversum
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core
import           Text.Printf
import           Variant

displayFiniteFuzzySet :: (PrintfArg k, PrintfArg v) => FuzzyFiniteUniversum v k -> UI Element
displayFiniteFuzzySet (FuzzyFiniteUniversum fuzzyMap) = do
    UI.table #+ [
        UI.tr #+ [
            UI.td # set UI.text (printf "%v, %.3g" el mfValue)
            | (el, mfValue) <- first20]
        ]
    where first20 = take 20 $ Map.toList fuzzyMap

setOps :: FuzzyFiniteUniversum Double Double -> FuzzyFiniteUniversum Double Double -> Int -> UI Element

setOps (FuzzyFiniteUniversum fuzzyA) (FuzzyFiniteUniversum fuzzyB) v =

    let t = v `mod` 3
        implementation =
            case v of
                0 -> MaxMin
                1 -> Algebraic
                2 -> Bounded
                _ -> MaxMin
        (FuzzyFiniteUniversum setOr, FuzzyFiniteUniversum setAnd) = case implementation of
                        MaxMin ->
                            let a = MaxMinFU fuzzyA
                                b = MaxMinFU fuzzyB

                                (MaxMinFU maxMinOr) = a ?|| b
                                (MaxMinFU maxMinAnd) = a ?&& b
                            in (FuzzyFiniteUniversum maxMinOr, FuzzyFiniteUniversum maxMinAnd)

                        Algebraic ->
                            let a = AlgrebraicFU fuzzyA
                                b = AlgrebraicFU fuzzyB

                                (AlgrebraicFU algOr) = a ?|| b
                                (AlgrebraicFU algAnd) = a ?&& b
                            in (FuzzyFiniteUniversum algOr, FuzzyFiniteUniversum algAnd)
                        Bounded ->
                            let a = BoundedFU fuzzyA
                                b = BoundedFU fuzzyB

                                (BoundedFU boundedOr) = a ?|| b
                                (BoundedFU boundedAnd) = a ?&& b
                            in (FuzzyFiniteUniversum boundedOr, FuzzyFiniteUniversum boundedAnd)
    in

    UI.div #+ [
        let txt = printf "T = %d, %s" t (show implementation) in
        UI.p # set UI.text txt,
        UI.table #+ (
            (UI.th #+ [UI.td # set UI.text "Об'єднання C"]) :
            [UI.tr #+ [UI.td # set UI.text (printf "{%v, %v}" el mfValue)]
                | (el, mfValue) <- Map.toList setOr]

        ),
        UI.table #+ [
            UI.th #+ (
                (UI.td # set UI.text "Перетин D") :
                [UI.tr #+ [UI.td # set UI.text (printf "{%v, %v}" el mfValue)]
                | (el, mfValue) <- Map.toList setAnd]
            )

        ],
        UI.table #+ [
            UI.th #+ [
                UI.td # set UI.text "Доповнення A",
                UI.td # set UI.text "Доповнення B"
            ]
        ]
    ]

displaySetInfo :: FuzzyFiniteUniversum Double Int -> FuzzyFiniteUniversum Double Double -> UI Element
displaySetInfo fuzzyA fuzzyB =
    UI.table #+ [
        UI.th #+ [
            UI.td # set UI.text "Множина",
            UI.td # set UI.text "Висота",
            UI.td # set UI.text "Мода",
            UI.td # set UI.text "Носій",
            UI.td # set UI.text "Ядро",
            UI.td # set UI.text "Множина альфа рівня"
        ],
        UI.tr #+ [
            fSetRow "A" fuzzyA,
            fSetRow "B" fuzzyB
        ]
    ]
    where fSetRow setName fuzzySet =
            UI.tr #+ [
                UI.td # set UI.text setName,
                UI.td # set UI.text (show (height fuzzySet)),
                UI.td # set UI.text (show (mode fuzzySet)),
                UI.td # set UI.text (show (supp fuzzySet)),
                UI.td # set UI.text (show (core fuzzySet)),
                UI.td # set UI.text (show (alphacut fuzzySet 0.3))
            ]
displayAB :: Int -> UI Element
displayAB v =
    UI.div #+ [
        showSet "A" setA,
        showSet "B" setB
    ]
    where
        setA = makeA v
        setB = makeB v
        showSet setName fuzzySet =
            UI.div # set UI.style [("display", "flex"), ("flex-direction", "row")] #+ [
                UI.span # set UI.text (setName ++ " =" ),
                displayFiniteFuzzySet fuzzySet
            ]
