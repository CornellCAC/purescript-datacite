module DataCite.Types where
  
import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.String.NonEmpty (NonEmptyString)

type ResourceRows = (
  data :: Data
)

type Resource = Record ResourceRows

type DataRows = (
  id :: NonEmptyString
  -- ^ A persistent identifier that identifies a resource.
, "type" :: NonEmptyString
, attributes :: Attributes
, relationships :: Relationships
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

type Data = Record DataRows

type AttributesRows = (
  doi :: NonEmptyString
, prefix :: NonEmptyString
, suffix :: NonEmptyString
, identifiers :: Array Identifier
, alternateIdentifiers :: Array AlternateIdentifier
, creators :: NonEmptyArray Creator
)

-- | Combines the Identifier and AlternateIdentifier properties from XML.
type Attributes = Record AttributesRows

-- TODO Not entirely sure about how this relates to doi
type IdentifierRows = (
  identifier :: String
, identifierType :: String
)
type Identifier = Record IdentifierRows

type AlternateIdentifierRows = (
  alternateIdentifier :: String
, alternateIdentifierType :: String
)
type AlternateIdentifier = Record AlternateIdentifierRows

-- TODO: incomplete
type CreatorRows = (
  name :: String
--, `type` :: String
)

type Creator = Record CreatorRows


type RelationshipsRows = (
)

type Relationships = Record RelationshipsRows


{- 
-- | Uniquely identifies a creator or contributor, according to 
--   various identifier schemes.
data NameIdentifier = NameIdentifier NonemptycontentStringType NameIdentifierAttributes deriving (Eq,Show)
data NameIdentifierAttributes = NameIdentifierAttributes
    { nameIdentifierAttributes_nameIdentifierScheme :: Xsd.XsdString
    , nameIdentifierAttributes_schemeURI :: Maybe Xs.AnyURI
    } -}