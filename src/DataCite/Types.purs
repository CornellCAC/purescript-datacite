module DataCite.Types where
  
import Prelude
import Data.String.NonEmpty (NonEmptyString)

type ResourceRows = (
  resource_identifier :: NonEmptyString
  -- ^ A persistent identifier that identifies a resource.
, resource_creators :: Array Creator
{- , resource_titles :: Titles
, resource_publisher :: Publisher
, resource_publicationYear :: YearType
, resource_resourceType :: ResourceType
, resource_subjects :: Maybe Subjects
, resource_contributors :: Maybe Contributors
, resource_dates :: Maybe Dates
, resource_language :: Maybe String
  -- ^ Primary language of the resource. Allowed values are taken 
  --  from IETF BCP 47, ISO 639-1 language codes.
  --  Currently just modeled as a string
, resource_alternateIdentifiers :: Maybe AlternateIdentifiers
, resource_relatedIdentifiers :: Maybe RelatedIdentifiers
, resource_sizes :: Maybe Sizes
, resource_formats :: Maybe Formats
, resource_version :: Maybe Xsd.XsdString
, resource_rightsList :: Maybe RightsList
, resource_descriptions :: Maybe Descriptions
, resource_geoLocations :: Maybe GeoLocations
, resource_fundingReferences :: Maybe FundingReferences -}
)

type Resource = Record ResourceRows

-- TODO: incomplete
type CreatorRows = (
  creatorName :: String
, nameType :: String
)

type Creator = Record CreatorRows


{- 
-- | Uniquely identifies a creator or contributor, according to 
--   various identifier schemes.
data NameIdentifier = NameIdentifier NonemptycontentStringType NameIdentifierAttributes deriving (Eq,Show)
data NameIdentifierAttributes = NameIdentifierAttributes
    { nameIdentifierAttributes_nameIdentifierScheme :: Xsd.XsdString
    , nameIdentifierAttributes_schemeURI :: Maybe Xs.AnyURI
    } -}