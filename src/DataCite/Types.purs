module DataCite.Types where

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Maybe (Maybe)
import Data.String.NonEmpty (NonEmptyString)
import DataCite.Types.Common

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

{-, resource_publisher :: Publisher
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

type SchemaVersion = {major :: Int, minor :: Int}
dataCiteVersion :: SchemaVersion
dataCiteVersion = {major: 4, minor: 3}

type AttributesRows = (
  doi :: NonEmptyString
, prefix :: NonEmptyString
, suffix :: NonEmptyString
, identifiers :: Array Identifier
, alternateIdentifiers :: Array Identifier
, creators :: NonEmptyArray Creator
, titles :: NonEmptyArray Title
, publisher :: NonEmptyString
, container :: Maybe Container
)

-- | Combines the Identifier and AlternateIdentifier properties from XML.
type Attributes = Record AttributesRows

type CreatorRows = (
  name :: NonEmptyString
, nameType :: Maybe NonEmptyString
, givenName :: Maybe NonEmptyString
, familyName :: Maybe NonEmptyString
, affiliation :: Array NonEmptyString
-- TODO: nameIdentifiers , e.g. 10.5281/zenodo.4072428
)

type Creator = Record CreatorRows

type TitleRows = (
  title :: String
  -- TODO: titleType
)

type Title = Record TitleRows

type ContainerRows = (
  "type" :: Maybe NonEmptyString
, identifier :: NonEmptyString
, identifierType :: IdentifierType
)

type Container = Record ContainerRows

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