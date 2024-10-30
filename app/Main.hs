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

        -- TODO: set type
        calcButton <- UI.button # set UI.text "Обчислити"

        -- TODO: set title
        arithopselect <- UI.listBox (pure (enumFromTo Add Div)) (pure $ Just Sub) (pure $ \op -> UI.label # set UI.text (show op))


        setOpsSection <- UI.div

        setsAB <- UI.div

        setsInfo <- UI.div


        _ <- getBody window #+ [
            UI.header #+ [UI.h1 # set UI.text "Лабораторна робота №1. Рівас Сіваш Максим ЗК-31мн"],
            UI.div #+ [
                UI.div #+ [element gLabel, element gInput,
                element kLabel, element kInput, element vValue],

                element calcButton, element arithopselect,
                element setsAB,
                element setsInfo,
                element setOpsSection

                ]
            ]

        -- on UI.click calcButton $ const $ do
        -- g <- gInput # get UI.value
        -- k <- kInput # get UI.value
        let g = "2"
            k = "5"
        let v = calcV (read g) (read k)
            setA = makeA v
            setB = makeB v

        vValue # set' UI.text ("V = " ++ show v)

        sets <- displayAB setA setB
        _ <- element setOpsSection #+ [setOps setA setB v]
        _ <- element setsAB # set UI.children [sets]
        _ <- element setsInfo #+ [displaySetInfo setA setB]

        _ <- element calcButton # set UI.text "I have been clicked!"
        return ()

