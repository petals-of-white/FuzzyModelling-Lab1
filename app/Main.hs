module Main where
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny.SVG  as SVG
import           Paths_FuzzyModelling_Lab1
import           Text.Read                   (readMaybe)
import           Variant
import qualified Widgets


main :: IO ()
main = do
    staticPath <- getDataFileName "wwwroot/"
    startGUI defaultConfig {
    jsPort = Just 8023,
    jsStatic = Just staticPath
} setup

setup :: Window -> UI ()
setup window = do
        UI.addStyleSheet window "main.css"
        _ <- return window # set UI.title "Лабораторна робота №1"
        gLabel <- UI.label # set UI.text "g = " # set UI.for "g"
        gInput <- UI.input # set UI.type_ "number" # set UI.id_ "g" # set UI.name "g" # set SVG.min "0" # set SVG.max "9"
        kLabel <- UI.label # set UI.text "k = " # set UI.for "k"
        kInput <- UI.input # set UI.type_ "number" # set UI.id_ "k" # set UI.name "k" # set SVG.min "0" # set SVG.max "9"
        vValue <- UI.label # set UI.text "V = "


        let (defaultG, defaultK) = (3,3) :: (Int,Int)
            eValidG =  filterJust $
                    (\n -> if n >= 0 && n <= 9 then Just n else Nothing) <$>
                    filterJust
                    (readMaybe <$> UI.valueChange gInput) :: Event Int

            eValidK = filterJust $
                    (\n -> if n >= 0 && n <= 9 then Just n else Nothing) <$>
                    filterJust
                    (readMaybe <$> UI.valueChange kInput) :: Event Int

        validGs <- stepper defaultG eValidG
        validKs <- stepper defaultK eValidK

        let validVarInput = uncurry varData <$> liftA2 (,) validGs validKs

        _ <- element vValue # sink UI.text (("V = " ++) . show .  varV <$> validVarInput)

        _ <- getBody window #+ [
            Widgets.appHeader,
            UI.div #+ [
                UI.div #+ [
                    element gLabel, element gInput,
                    element kLabel, element kInput, element vValue
                ],

                getElement <$> Widgets.fuzzyAB ((\VariantData {varSetA=a, varSetB=b} ->(a,b)) <$> validVarInput),
                getElement <$> Widgets.displaySetInfo validVarInput,
                getElement <$> Widgets.setOps validVarInput,
                Widgets.triangleFuzzyNumberInfo (varTriangles <$> validVarInput),
                Widgets.trapeziumFuzzyNumberInfo (varTrapezia <$> validVarInput),
                Widgets.arithOpSection validVarInput
                ]
            ]

        return ()

