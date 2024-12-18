module Widgets where

import           Data.List                   (intercalate, intersperse)
import qualified Data.Map                    as Map
import qualified Data.Set                    as S
import qualified Elements                    as UI
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
    section <- UI.div
    tRow <- UI.tr
    _ <- element section #+ [
            UI.table #. "fuzzy-content pure-table pure-table-bordered" #+ [UI.tbody #+ [element tRow]]
            ]
    onChanges bFuzzyData  (\(fuzzy,sName) -> do
        cells <- makeCells sName fuzzy
        element tRow # set UI.children cells
        )
    return
        FuzzySet {
            fuzzySetEl = section,
            fuzzySetTidings = tidings bFuzzyData never
        }
    where
        makeCells fName (FuzzyFiniteUniversum fuzzyMap) =
            sequence $
            (UI.th # set UI.text fName) : take 20 ([UI.td # set UI.text (printf "%.2f, %.2f" el mfValue)
                                                   | (el, mfValue) <- Map.toList fuzzyMap] ++ repeat UI.td)

fuzzyAB ::
    (PrintfArg v, PrintfArg k) =>
    Behavior (FuzzyFiniteUniversum v k, FuzzyFiniteUniversum v k)
    -> UI Element
fuzzyAB bSets =
    UI.div #+ [
        getElement <$> fuzzySet ((, "A") . fst <$> bSets),
        getElement <$> fuzzySet ((, "B") . snd <$> bSets)
    ]

showSet :: (Show a) => S.Set a -> String
showSet s = '{': init (tail $ show $ S.toList s) ++ "}"

displaySetInfo :: Behavior VariantData -> UI Element
displaySetInfo bVarData = do
    UI.table #. "pure-table pure-table-bordered" #+ [
        UI.thead #+ [
            UI.tr #+ [
                UI.th # set UI.text "Множина",
                UI.th # set UI.text "Висота",
                UI.th # set UI.text "Мода",
                UI.th # set UI.text "Носій",
                UI.th # set UI.text "Ядро",
                UI.th # set UI.text "Множина альфа рівня"
            ]
        ],
        UI.tbody #+ [
            fSetRow ((\variant -> ("A", varSetA variant, varAlpha variant)) <$> bVarData),
            fSetRow ((\variant -> ("B", varSetB variant, varAlpha variant)) <$> bVarData)
        ]

        ]

    where
        fSetRow bFuzzyData =
            let bName = (\(n,_, _) -> n) <$> bFuzzyData
                bSet = (\(_,s, _) -> s) <$> bFuzzyData
            in
            UI.tr #+ [
                UI.td # sink UI.text bName,
                UI.td # sink UI.text (show . height <$> bSet),
                UI.td # sink UI.text (showSet . mode <$> bSet),
                UI.td # sink UI.text (showSet . supp <$> bSet),
                UI.td # sink UI.text (showSet . core <$> bSet),
                UI.td # sink UI.text (showSet . (\(_b,s,alpha) -> alphacut s alpha) <$> bFuzzyData)
            ]

triangleToTuple :: TriangleMF a -> (a,a,a)
triangleToTuple (TriangleMF a b c) = (a,b,c)

trapeziumToTuple :: TrapeziumMF d -> (d, d, d, d)
trapeziumToTuple (TrapeziumMF a b c d) = (a,b,c,d)

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

    unionSection <- UI.table #. "pure-table pure-table-bordered"
    intersectionSection <- UI.table #. "pure-table pure-table-bordered"
    compl <- UI.table #. "pure-table pure-table-bordered"
    let showSet tabl fName (FuzzyFiniteUniversum fuzzy) = do
            header <- UI.thead #+ [UI.tr #+ [UI.th # set UI.text fName ]]
            tBody <- UI.tbody #+ [
                UI.tr #+ [UI.td # set UI.text (printf "<%.2f, %.2f>" el mfValue)]
                | (el, mfValue) <- Map.toList fuzzy
                ]

            _ <- element tabl # set UI.children [header,tBody]

            return ()

    onChanges bVarData (\VariantData {varSetUnion=c, varSetIntersection=d, varNotA=complA, varNotB=complB} -> do
        showSet unionSection "Об'єднання C" c
        showSet intersectionSection "Перетин D" d
        showComplement compl complA complB
        )

    UI.div #+ [
        UI.p #. "pure-u-1-1" # sink UI.text implInfo,
        UI.div #. "pure-u-6-24" #+ [element unionSection] ,
        UI.div #. "pure-u-6-24" #+ [element intersectionSection],
        UI.div #. "pure-u-12-24" #+ [element compl]
        ]


    where implInfo =
            fmap (\VariantData {varImplmentation, varT} -> printf "T = %d, %s" varT (show varImplmentation))
            bVarData

showComplement :: Element -> FuzzyFiniteUniversum Double Double -> FuzzyFiniteUniversum Double Double -> UI ()
showComplement tabl (FuzzyFiniteUniversum notA) (FuzzyFiniteUniversum notB) = do
    h <- UI.thead #+ [
        UI.tr #+ [
                UI.th # set UI.text "Доповнення A",
                UI.th # set UI.text "Доповнення B"
            ]
        ]

    tBody <- UI.tbody #+ [
            UI.tr #+ [
                UI.td # set UI.text (printf "<%.2f, %.2f>" elA mfA),
                UI.td # set UI.text (printf "<%.2f, %.2f>" elB mfB)
            ]
            | ((elA, mfA), (elB, mfB)) <- zip (Map.toList notA) (Map.toList notB)]


    _ <- element tabl # set UI.children [h, tBody]
    return ()

showFuzzy :: SimpleFuzzy -> String
showFuzzy (FuzzyFiniteUniversum fuzzyMap) =
    '{' : intercalate ", " (map showPair $ Map.toList fuzzyMap) ++"}"
    where showPair (el,mfValue) = printf "<%.2f, %.2f>" el mfValue


showTriangle :: TriangleMF Double -> String
showTriangle (TriangleMF a b c) = printf "(%.2f, %.2f, %.2f)" a b c

showTrapezium :: TrapeziumMF Double -> String
showTrapezium (TrapeziumMF a b c d) = printf "(%.2f, %.2f, %.2f, %.2f)" a b c d

arithOpSection ::Behavior VariantData -> UI Element
arithOpSection  bVarData = do
    UI.table #. "pure-table pure-table-bordered" #+ [
        UI.thead #+ [
            UI.tr #+ [
                UI.td # set UI.text "Множини",
                UI.th # set UI.text "Додавання",
                UI.th # set UI.text "Віднімання",
                UI.th # set UI.text "Множення",
                UI.th # set UI.text "Ділення"
            ]
        ],
        UI.tbody #+ [
            UI.tr #+ [
                UI.td # set UI.text "A і B",
                UI.td # sink UI.text (showFuzzy . uncurry (+) <$> twoFuzzy),
                UI.td # sink UI.text (showFuzzy . uncurry (-) <$> twoFuzzy),
                UI.td # sink UI.text (showFuzzy . uncurry (*) <$> twoFuzzy),
                UI.td # sink UI.text (showFuzzy . uncurry (/) <$> twoFuzzy)
                ],
            UI.tr #+ [
                UI.td # set UI.text "TriangleA і TriangleB",
                UI.td # sink UI.text (showTriangle . uncurry (+) <$> twoTriangles),
                UI.td # sink UI.text (showTriangle . uncurry (-) <$> twoTriangles),
                UI.td # sink UI.text (showTriangle . uncurry (*) <$> twoTriangles),
                UI.td # sink UI.text (showTriangle . uncurry (/) <$> twoTriangles)
                ],

            UI.tr #+ [
                UI.td # set UI.text "TrapeziumA і TrapeziumB",
                UI.td # sink UI.text (showTrapezium . uncurry (+) <$> twoTrapezia),
                UI.td # sink UI.text (showTrapezium . uncurry (-) <$> twoTrapezia),
                UI.td # sink UI.text (showTrapezium . uncurry (*) <$> twoTrapezia),
                UI.td # sink UI.text (showTrapezium . uncurry (/) <$> twoTrapezia)
                ]
        ]

        ]
    where
        twoFuzzy = fmap (\VariantData {varSetA=a, varSetB=b} -> (a,b)) bVarData
        twoTriangles = fmap varTriangles bVarData
        twoTrapezia = fmap varTrapezia bVarData

