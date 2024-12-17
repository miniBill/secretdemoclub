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
    | CouldNotGetToken
    | WithToken TokenData
    | CouldNotGetIdentity
    | WithIdentity String


type FrontendMsg
    = OnUrlChange Url
    | OnUrlRequest UrlRequest


type alias BackendModel =
    {}


type BackendMsg
    = GotToken ClientId (Result Http.Error TokenData)
    | GotIdentity ClientId (Result Http.Error String)


type alias TokenData =
    { accessToken : String
    , expiresIn : Int
    , refreshToken : String
    , scope : List String
    }


type ToBackend
    = GetTokenRequest { code : String }
    | GetIdentityRequest TokenData


type ToFrontend
    = GetTokenResponse (Result () TokenData)
    | GetIdentityResponse (Result () String)
