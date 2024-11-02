module Views where

import qualified Data.Map                    as Map
import           Fuzzy.Base
import           Fuzzy.FiniteUniversum
import           Fuzzy.TrapeziumMF           (TrapeziumMF (..))
import           Fuzzy.TriangleMF            (TriangleMF (..))
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core
import           Text.Printf
import           Variant


appHeader :: UI Element
appHeader = UI.header #+ [UI.h1 # set UI.text "Лабораторна робота №1. Рівас Сіваш Максим ЗК-31мн"]


displayFiniteFuzzySet :: (PrintfArg k, PrintfArg v) => FuzzyFiniteUniversum v k -> UI Element

displayFiniteFuzzySet (FuzzyFiniteUniversum fuzzyMap) = do
    UI.table #+ [
        UI.tr #+ [
            UI.td # set UI.text (printf "%v, %v" el mfValue)
            | (el, mfValue) <- first20]
        ]
    where first20 = take 20 $ Map.toList fuzzyMap


setOps :: FuzzyFiniteUniversum Double Double -> FuzzyFiniteUniversum Double Double -> Int -> UI Element

setOps (FuzzyFiniteUniversum fuzzyA) (FuzzyFiniteUniversum fuzzyB) v =
    let t = v `mod` 3
        implementation =
            case t of
                0 -> MaxMin
                1 -> Algebraic
                2 -> Bounded
                _ -> MaxMin

        (FuzzyFiniteUniversum setOr, FuzzyFiniteUniversum setAnd, FuzzyFiniteUniversum notA, FuzzyFiniteUniversum notB) = case implementation of
                        MaxMin ->
                            let a = MaxMinFU fuzzyA
                                b = MaxMinFU fuzzyB

                                (MaxMinFU maxMinOr) = a ?|| b
                                (MaxMinFU maxMinAnd) = a ?&& b
                                (MaxMinFU maxMinNotA) = fnot (MaxMinFU fuzzyA)
                                (MaxMinFU maxMinNotB) = fnot (MaxMinFU fuzzyB)
                            in (FuzzyFiniteUniversum maxMinOr, FuzzyFiniteUniversum maxMinAnd,
                                FuzzyFiniteUniversum maxMinNotA, FuzzyFiniteUniversum maxMinNotB)

                        Algebraic ->
                            let a = AlgrebraicFU fuzzyA
                                b = AlgrebraicFU fuzzyB

                                (AlgrebraicFU algOr) = a ?|| b
                                (AlgrebraicFU algAnd) = a ?&& b
                                (AlgrebraicFU algNotA) = fnot (AlgrebraicFU fuzzyA)
                                (AlgrebraicFU algNotB) = fnot (AlgrebraicFU fuzzyB)

                            in (FuzzyFiniteUniversum algOr, FuzzyFiniteUniversum algAnd,
                                FuzzyFiniteUniversum algNotA, FuzzyFiniteUniversum algNotB)
                        Bounded ->
                            let a = BoundedFU fuzzyA
                                b = BoundedFU fuzzyB

                                (BoundedFU boundedOr) = a ?|| b
                                (BoundedFU boundedAnd) = a ?&& b
                                (BoundedFU boundedNotA) = fnot (BoundedFU fuzzyA)
                                (BoundedFU boundedNotB) = fnot (BoundedFU fuzzyB)

                            in (FuzzyFiniteUniversum boundedOr, FuzzyFiniteUniversum boundedAnd,
                                FuzzyFiniteUniversum boundedNotA, FuzzyFiniteUniversum boundedNotB)
    in

    UI.div #+ [
        let txt = printf "T = %d, %s" t (show implementation) in
        UI.p # set UI.text txt,
        UI.table #+ (
            (UI.tr #+ [UI.th # set UI.text "Об'єднання C"]) :
            [UI.tr #+ [UI.td # set UI.text (printf "{%v, %v}" el mfValue)]
                | (el, mfValue) <- Map.toList setOr]

        ),
        UI.table #+ (
            UI.tr #+ [UI.th # set UI.text "Перетин D"]
            :
            [UI.tr #+ [UI.td # set UI.text (printf "{%v, %v}" el mfValue)]
                | (el, mfValue) <- Map.toList setAnd]


        ),
        UI.table #+ (
            UI.tr #+ [
                UI.th # set UI.text "Доповнення A",
                UI.th # set UI.text "Доповнення B"
            ]
            :
            [UI.tr #+ [
                UI.td # set UI.text (printf "{%v, %v}" elA mfA),
                UI.td # set UI.text (printf "{%v, %v}" elB mfB)
            ] | ((elA, mfA), (elB, mfB)) <- zip (Map.toList notA) (Map.toList notB)]

            )
    ]

displaySetInfo :: FuzzyFiniteUniversum Double Double -> FuzzyFiniteUniversum Double Double -> UI Element

displaySetInfo fuzzyA fuzzyB =
    UI.table #+ [
        UI.tr #+ [
            UI.th # set UI.text "Множина",
            UI.th # set UI.text "Висота",
            UI.th # set UI.text "Мода",
            UI.th # set UI.text "Носій",
            UI.th # set UI.text "Ядро",
            UI.th # set UI.text "Множина альфа рівня"
        ],
        fSetRow "A" fuzzyA,
        fSetRow "B" fuzzyB
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
displayAB :: FuzzyFiniteUniversum Double Double -> FuzzyFiniteUniversum Double Double -> UI Element

displayAB setA setB =
    UI.div #+ [
        showSet "A" setA,
        showSet "B" setB
    ]
    where
        showSet setName fuzzySet =
            UI.div # set UI.style [("display", "flex"), ("flex-direction", "row")] #+ [
                UI.span # set UI.text (setName ++ " =" ),
                displayFiniteFuzzySet fuzzySet
            ]

triangleFuzzyNumberInfo :: TriangleMF Double -> TriangleMF Double -> UI Element
triangleFuzzyNumberInfo (TriangleMF aa ba ca) (TriangleMF ab bb cb) =
    UI.div #+ [
        UI.p # set UI.text "Трикутні числа",
        UI.p # set UI.text ("T1 = " ++ show (aa,ba,ca)),
        UI.p # set UI.text ("T2 = " ++ show (ab,bb,cb))
    ]

trapeziumFuzzyNumberInfo :: TrapeziumMF Double -> TrapeziumMF Double -> UI Element
trapeziumFuzzyNumberInfo (TrapeziumMF aa ba ca da) (TrapeziumMF ab bb cb db) =
    UI.div #+ [
        UI.p # set UI.text "Трапецієподібні числа",
        UI.p # set UI.text ("T1 = " ++ show (aa,ba,ca,da)),
        UI.p # set UI.text ("T2 = " ++ show (ab,bb,cb,db))
    ]

arithOpSection ::
    (FuzzyFiniteUniversum Double Double, FuzzyFiniteUniversum Double Double) ->
    (TriangleMF Double, TriangleMF Double) ->
    (TrapeziumMF Double, TrapeziumMF Double) ->
    UI Element

arithOpSection (fuzzyA, fuzzyB) (triangleA, triangleB) (trapeziumA, trapeziumB) =
    UI.table #+ [
        UI.tr #+ [
            UI.td,
            UI.th # set UI.text "Додавання",
            UI.th # set UI.text "Віднімання",
            UI.th # set UI.text "Множення",
            UI.th # set UI.text "Ділення"],
        UI.tr #+ [
            UI.td # set UI.text "A і B",
            UI.td # set UI.text (show (fuzzyA + fuzzyB)),
            UI.td # set UI.text (show (fuzzyA - fuzzyB)),
            UI.td # set UI.text (show (fuzzyA * fuzzyB)),
            UI.td # set UI.text (show (fuzzyA / fuzzyB))
            ],
        UI.tr #+ [
            UI.td # set UI.text "TriangleA і TriangleB",
            UI.td # set UI.text (show (triangleA + triangleB)),
            UI.td # set UI.text (show (triangleA - triangleB)),
            UI.td # set UI.text (show (triangleA * triangleB)),
            UI.td # set UI.text (show (triangleA / triangleB))
            ],

        UI.tr #+ [
            UI.td # set UI.text "TrapeziumA + TrapeziumB",
            UI.td # set UI.text (show (trapeziumA + trapeziumB)),
            UI.td # set UI.text (show (trapeziumA - trapeziumB)),
            UI.td # set UI.text (show (trapeziumA * trapeziumB)),
            UI.td # set UI.text (show (trapeziumA / trapeziumB))
            ]
    ]
