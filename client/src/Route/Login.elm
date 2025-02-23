module Route.Login exposing (view)

import Html
import Html.Attributes
import Url.Builder
import View exposing (View)


view : View msg
view =
    { title = "Login required"
    , body =
        [ Html.a
            [ Html.Attributes.href
                (Url.Builder.crossOrigin "https://www.patreon.com"
                    [ "oauth2", "authorize" ]
                    [ Url.Builder.string "response_type" "code"
                    , Url.Builder.string "client_id" "XeirGp33CAMMls3EC_mTdZWgMp0XcgzPwkWH8POaLMRX29OUe9AwGVDk4Djn_kMn"
                    , Url.Builder.string "redirect_uri" "https://uriel.tail1b193.ts.net/feed"
                    , Url.Builder.string "scope" "identity identity.memberships"
                    ]
                )
            ]
            [ Html.text "Login" ]
        ]
    }
