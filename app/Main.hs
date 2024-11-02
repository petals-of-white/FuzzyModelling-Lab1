module Main where
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny.SVG  as SVG
import           Paths_FuzzyModelling_Lab1
import           Variant
import           Views

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

        calcButton <- UI.button # set UI.type_ "button" # set UI.text "Обчислити"
        setOpsSection <- UI.div

        setsAB <- UI.div

        setsInfo <- UI.div

        arithSection <- UI.div

        fuzzyNumberSection <- UI.div

        _ <- getBody window #+ [
            appHeader,
            UI.div #+ [
                UI.div #+ [element gLabel, element gInput,
                element kLabel, element kInput, element vValue],
                element calcButton,
                element setsAB,
                element setsInfo,
                element setOpsSection,
                element fuzzyNumberSection,
                element arithSection
                ]
            ]

        on UI.click calcButton $ const $ do
            g <- read <$> gInput # get UI.value
            k <- read <$> kInput # get UI.value
            let --g = 2
                --k = 5
                VariantData {
                    varV = v,
                    varSetA = setA, varSetB = setB,
                    varTriangles = (t1,t2),
                    varTrapezia = (trap1, trap2)} = varData g k


            vValue # set' UI.text ("V = " ++ show v)

            sets <- displayAB setA setB
            setOpsEl <- setOps setA setB v
            setsInfoEl <- displaySetInfo setA setB
            arithOpSectionEl <- arithOpSection (setA,setB) (t1,t2) (trap1, trap2)
            trianglesEl <- triangleFuzzyNumberInfo t1 t2
            trapeziaEl <- trapeziumFuzzyNumberInfo trap1 trap2
            
            _ <- element setOpsSection # set UI.children [setOpsEl]
            _ <- element setsAB # set UI.children [sets]
            _ <- element setsInfo # set UI.children [setsInfoEl]
            _ <- element arithSection # set UI.children [arithOpSectionEl]
            _ <- element fuzzyNumberSection # set UI.children [trianglesEl, trapeziaEl]
            return ()

