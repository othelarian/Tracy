module Home exposing (HomePhase, HomeMsg, homeGotError, viewHome)

import Html exposing (Html, button, div, input, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

-- HOME TYPES

type HomePhase
    = HomeCheck
    | HomeError
    | HomeInit
    | HomeGetInfo
    | HomeList
    --
    -- TODO : finir les phases
    --
    --
    | HomeClean
    --
    --| HomeAdd
    --| HomeRemove
    --

type HomeMsg
    --
    -- TODO : tout reste à faire dans cette partie
    --
    = HMCheck
    --
    | HMRetry
    --

-- PROCESSING ERRORS

homeGotError : HomePhase -> HomePhase -> (HomePhase, HomePhase)
homeGotError actPhase prevPhase =
    case actPhase of
        HomeCheck -> (HomeError, HomeCheck)
        HomeError -> (HomeError, HomeError)
        HomeInit -> (HomeError, HomeCheck)
        HomeGetInfo -> (prevPhase, actPhase)
        --
        --
        -- TODO : faire les autres cas
        --
        _ -> (actPhase, prevPhase)

-- VIEW

viewHome : () -> HomePhase -> (Html msg) -> Html msg
viewHome _ phase diverror =
    div [class "core"]
        [ div [class "zone_status"]
            (case phase of
                HomeCheck -> [text "Vérification ..."]
                HomeError ->
                    [ span [class "error"] [text "Il y a eu un problème !"]
                    --
                    -- TODO : réactiver ce bouton !!
                    --
                    --, button [onClick HomeRetry, class "button"] [text "Réessayer"]
                    ]
                HomeInit -> [text "Initialisation ..."]
                HomeGetInfo -> [text "Importation ..."]
                HomeList ->
                    [ span [] [text "Liste des Projets"]
                    --
                    -- TODO : bouton à réactiver !!
                    --
                    --, button [onClick AddProject] [text "Ajouter"]
                    --
                    ]
                    --
                    -- TODO : lister tous les projets
                    --
                    -- TODO : d'abord les boutons
                    --
                    -- TODO : puis la liste des projets
                    --
                --
                -- TODO : ajouter les cas manquant
                --
                _ -> [text "another status"]
                --
            )
        , diverror
        , div [] [
            --
            -- TODO : la liste des projets
            --
            -- TODO : test avec HTTP from elm
            --
            --
        ]
        ]
