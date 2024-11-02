module Widgets where

import qualified Data.Map                    as Map
import           Fuzzy.Base
import           Fuzzy.FiniteUniversum
import           Fuzzy.TrapeziumMF
import           Fuzzy.TriangleMF
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core
import           Text.Printf
import           Variant


type SetName = String


data FuzzySet v k = FuzzySet {
    fuzzySetEl      :: Element,
    fuzzySetTidings :: Tidings (FuzzyFiniteUniversum v k, SetName)
}

instance Widget (FuzzySet k v) where getElement = fuzzySetEl

appHeader :: UI Element
appHeader = UI.div #+ [UI.h1 # set UI.text "Лабораторна робота №1. Рівас Сіваш Максим ЗК-31мн"]


fuzzySet :: (PrintfArg v, PrintfArg k) => Behavior (FuzzyFiniteUniversum v k, SetName) -> UI (FuzzySet v k)
fuzzySet bFuzzyData = do
    section <- UI.div # set UI.style [("display", "flex"), ("flex-direction", "row")]
    tRow <- UI.tr
    _ <- element section #+ [
            UI.span # sink UI.text (snd <$> bFuzzyData),
            UI.table #+ [element tRow]
            ]
    onChanges (fst <$> bFuzzyData) (\fuzzy -> do
        cells <- makeCells fuzzy
        element tRow # set UI.children cells
        )
    return
        FuzzySet {
            fuzzySetEl = section,
            fuzzySetTidings = tidings bFuzzyData never
        }
    where
        makeCells (FuzzyFiniteUniversum fuzzyMap) =
            sequence $
            [UI.td # set UI.text (printf "%v, %v" el mfValue)| (el, mfValue) <- first20]
            where first20 = take 20 $ Map.toList fuzzyMap

fuzzyAB ::
    (PrintfArg v, PrintfArg k) =>
    Behavior (FuzzyFiniteUniversum v k, FuzzyFiniteUniversum v k)
    -> UI Element
fuzzyAB bSets =
    UI.div #+ [
        getElement <$> fuzzySet ((, "A") . fst <$> bSets),
        getElement <$> fuzzySet ((, "B") . snd <$> bSets)
    ]

displaySetInfo :: Behavior VariantData -> UI Element
displaySetInfo bVarData = do
    UI.table #+ [
        UI.tr #+ [
            UI.th # set UI.text "Множина",
            UI.th # set UI.text "Висота",
            UI.th # set UI.text "Мода",
            UI.th # set UI.text "Носій",
            UI.th # set UI.text "Ядро",
            UI.th # set UI.text "Множина альфа рівня"
        ],
        fSetRow (("A",) . varSetA <$> bVarData),
        fSetRow (("B",) . varSetA <$> bVarData)
        ]

    where fSetRow bFuzzyData =
            let bSet = snd <$> bFuzzyData in
            UI.tr #+ [
                UI.td # sink UI.text (fst <$> bFuzzyData),
                UI.td # sink UI.text (show . height <$> bSet),
                UI.td # sink UI.text (show . mode <$> bSet),
                UI.td # sink UI.text (show . supp <$> bSet),
                UI.td # sink UI.text (show . core <$> bSet),
                UI.td # sink UI.text (show . flip alphacut 0.15 <$> bSet)
            ]


triangleFuzzyNumberInfo :: Behavior (TriangleMF Double, TriangleMF Double) -> UI Element
triangleFuzzyNumberInfo bTriangles =
    let toTuple (TriangleMF a b c) = (a,b,c) in
    UI.div #+ [
        UI.p # set UI.text "Трикутні числа",
        UI.p # sink UI.text ((\(t1, _) -> "T1 =" ++ show (toTuple t1)) <$> bTriangles),
        UI.p # sink UI.text ((\(_, t2) -> "T2 =" ++ show (toTuple t2)) <$> bTriangles)
    ]

trapeziumFuzzyNumberInfo :: Behavior (TrapeziumMF Double, TrapeziumMF Double) -> UI Element
trapeziumFuzzyNumberInfo bTrapezia =
    let toTuple (TrapeziumMF a b c d) = (a,b,c,d) in
    UI.div #+ [
        UI.p # set UI.text "Трапецієподібні числа",
        UI.p # sink UI.text (("Trap1 = " ++) . show . toTuple . fst <$> bTrapezia),
        UI.p # sink UI.text (("Trap2 = " ++) . show . toTuple . snd <$> bTrapezia)
    ]



setOps :: Behavior VariantData  -> UI Element
setOps bVarData  = do

    unionSection <- UI.table
    intersectionSection <- UI.table
    compl <- UI.table
    let showSet tabl fName (FuzzyFiniteUniversum fuzzy) = do
            tableRows <-
                sequence $
                (UI.tr #+ [UI.th # set UI.text fName ]) :
                [UI.tr #+ [UI.td # set UI.text (printf "{%v, %v}" el mfValue)]
                | (el, mfValue) <- Map.toList fuzzy]

            _ <- element tabl # set UI.children tableRows

            return ()

    onChanges bVarData (\VariantData {varSetUnion=c, varSetIntersection=d, varNotA=complA, varNotB=complB} -> do
        showSet unionSection "Об'єднання C" c
        showSet intersectionSection "Перетин D" d
        showComplement compl complA complB
        )

    UI.div #+ [
        UI.p # sink UI.text implInfo,
        element unionSection,
        element intersectionSection,
        element compl
        ]


    where implInfo =
            fmap (\VariantData {varImplmentation, varT} -> printf "T = %d, %s" varT (show varImplmentation))
            bVarData

showComplement :: Element -> FuzzyFiniteUniversum Double Double -> FuzzyFiniteUniversum Double Double -> UI ()
showComplement tabl (FuzzyFiniteUniversum notA) (FuzzyFiniteUniversum notB) = do
    tRows <- sequence (UI.tr #+ [
                UI.th # set UI.text "Доповнення A",
                UI.th # set UI.text "Доповнення B"
            ]
            :
            [UI.tr #+ [
                UI.td # set UI.text (printf "{%v, %v}" elA mfA),
                UI.td # set UI.text (printf "{%v, %v}" elB mfB)
            ] | ((elA, mfA), (elB, mfB)) <- zip (Map.toList notA) (Map.toList notB)]

        )
    _ <- element tabl # set UI.children tRows
    return ()


arithOpSection ::Behavior VariantData -> UI Element
arithOpSection  bVarData = do
    UI.table #+ [
        UI.tr #+ [
            UI.td,
            UI.th # set UI.text "Додавання",
            UI.th # set UI.text "Віднімання",
            UI.th # set UI.text "Множення",
            UI.th # set UI.text "Ділення"],
        UI.tr #+ [
            UI.td # set UI.text "A і B",
            UI.td # sink UI.text (show . uncurry (+) <$> twoFuzzy),
            UI.td # sink UI.text (show . uncurry (-) <$> twoFuzzy),
            UI.td # sink UI.text (show . uncurry (*) <$> twoFuzzy),
            UI.td # sink UI.text (show . uncurry (/) <$> twoFuzzy)
            ],
        UI.tr #+ [
            UI.td # set UI.text "TriangleA і TriangleB",
            UI.td # sink UI.text (show . uncurry (+) <$> twoTriangles),
            UI.td # sink UI.text (show . uncurry (-) <$> twoTriangles),
            UI.td # sink UI.text (show . uncurry (*) <$> twoTriangles),
            UI.td # sink UI.text (show . uncurry (/) <$> twoTriangles)
            ],

        UI.tr #+ [
            UI.td # set UI.text "TrapeziumA + TrapeziumB",
            UI.td # sink UI.text (show . uncurry (+) <$> twoTrapezia),
            UI.td # sink UI.text (show . uncurry (-) <$> twoTrapezia),
            UI.td # sink UI.text (show . uncurry (*) <$> twoTrapezia),
            UI.td # sink UI.text (show . uncurry (/) <$> twoTrapezia)
            ]
        ]
    where
        twoFuzzy = fmap (\VariantData {varSetA=a, varSetB=b} -> (a,b)) bVarData
        twoTriangles = fmap varTriangles bVarData
        twoTrapezia = fmap varTrapezia bVarData

