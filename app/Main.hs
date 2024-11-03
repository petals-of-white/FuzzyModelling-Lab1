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
        _ <- UI.getHead window #+ [
                UI.meta # set UI.name "viewport" # set UI.content "width=device-width, initial-scale=1" ,
                UI.link
                    # set UI.rel "stylesheet" # set UI.href "https://cdn.jsdelivr.net/npm/purecss@3.0.0/build/pure-min.css"
                    # set (UI.attr "integrity") "sha384-X38yfunGUhNzHpBaEBsWLO+A0HDYOQi8ufWDkZ0k9e0eXz/tH3II7uKZ9msv++Ls"
                    # set (UI.attr "crossorigin") "anonymous"
            ]
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
            UI.mkElement "main" # set UI.id_ "main" #. "pure-g" #+ [
                UI.div # set UI.id_ "displayABSection" #. "pure-u-1-1" #+ [
                    element gLabel, element gInput,
                    element kLabel, element kInput,
                    element vValue
                ],

                (getElement <$> Widgets.fuzzyAB ((\VariantData {varSetA=a, varSetB=b} ->(a,b)) <$> validVarInput)) #. " pure-u-1-1",
                (getElement <$> Widgets.displaySetInfo validVarInput) #. "pure-table pure-table-bordered pure-u-1-1",
                (getElement <$> Widgets.setOps validVarInput) #. "pure-u-1-1",
                Widgets.triangleFuzzyNumberInfo (varTriangles <$> validVarInput) #. "pure-u-1-2",
                Widgets.trapeziumFuzzyNumberInfo (varTrapezia <$> validVarInput) #. "pure-u-1-2",
                Widgets.arithOpSection validVarInput #. "pure-table pure-table-bordered pure-u-1-1"
                ]
            ]

        return ()

