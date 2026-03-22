module Filter exposing (Filter(..), FilterData, current, empty)

import Set exposing (Set)


type Filter
    = Filtering { current : FilterData, previous : FilterData }
    | Filtered FilterData


type alias FilterData =
    { year : Maybe Int
    , categories : Set String
    , search : String
    }


empty : FilterData
empty =
    { year = Nothing
    , categories = Set.empty
    , search = ""
    }


current : Filter -> FilterData
current filter =
    case filter of
        Filtered c ->
            c

        Filtering d ->
            d.current
