module Types exposing (BackendModel, BackendMsg(..), FrontendModel, FrontendMsg(..), InnerModel(..), ToBackend(..), ToFrontend(..), TokenData)

import Http
import Lamdera exposing (ClientId, Url, UrlRequest)


type alias FrontendModel =
    { key : Lamdera.Key
    , inner : InnerModel
    }


type InnerModel
    = Initial
    | WithCode String
    | CouldNotGetIdentity
    | WithIdentity String


type FrontendMsg
    = OnUrlChange Url
    | OnUrlRequest UrlRequest


type alias BackendModel =
    {}


type BackendMsg
    = GotIdentity ClientId (Result Http.Error String)


type alias TokenData =
    { accessToken : String
    , expiresIn : Int
    , refreshToken : String
    , scope : List String
    }


type ToBackend
    = GetIdentityRequest { code : String }


type ToFrontend
    = GetIdentityResponse (Result () String)
