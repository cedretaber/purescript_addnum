module Main where

import Prelude
import Effect (Effect)
import Data.Maybe (Maybe(..))
import Data.Int (fromString)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = { x :: Int, y :: Int }

data Field = X | Y

data Query a = Update Field String a

calc :: forall m. H.Component HH.HTML Query Unit Void m
calc = 
    H.component
        { initialState: const initialState
        , render
        , eval
        , receiver: const Nothing
        }
    where

    initialState :: State
    initialState = { x: 0, y: 0 }

    render :: State -> H.ComponentHTML Query
    render s =
        HH.div_
            [ HH.input
                [ HP.type_ HP.InputText
                , HP.value $ show s.x
                , HE.onValueChange $ HE.input $ Update X
                ]
            , HH.text " + "
            , HH.input
                [ HP.type_ HP.InputText
                , HP.value $ show s.y
                , HE.onValueChange $ HE.input $ Update Y
                ]
            , HH.text $ " = " <> (show $ s.x + s.y)
            ]

    eval :: Query ~> H.ComponentDSL State Query Void m
    eval (Update xy s next) =
        let
            n = case fromString s of
                Just n -> n
                Nothing -> 0
        in do
            _ <- H.modify $ case xy of
                X -> (_ { x = n })
                Y -> (_ { y = n })
            pure next

main :: Effect Unit
main = HA.runHalogenAff do
    body <- HA.awaitBody
    runUI calc unit body