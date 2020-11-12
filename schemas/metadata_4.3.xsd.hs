{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module V'
  ( module V'
  , module Include'datacite'titleType'v4'xsd
  , module Include'datacite'contributorType'v4'xsd
  , module Include'datacite'dateType'v4'xsd
  , module Include'datacite'resourceType'v4'xsd
  , module Include'datacite'relationType'v4'xsd
  , module Include'datacite'relatedIdentifierType'v4'xsd
  , module Include'datacite'funderIdentifierType'v4'xsd
  , module Include'datacite'descriptionType'v4'xsd
  , module Include'datacite'nameType'v4'xsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import Text.XML.HaXml.OneOfN
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
import Include'datacite'titleType'v4'xsd
import Include'datacite'contributorType'v4'xsd
import Include'datacite'dateType'v4'xsd
import Include'datacite'resourceType'v4'xsd
import Include'datacite'relationType'v4'xsd
import Include'datacite'relatedIdentifierType'v4'xsd
import Include'datacite'funderIdentifierType'v4'xsd
import Include'datacite'descriptionType'v4'xsd
import Include'datacite'nameType'v4'xsd
import Include'xml'xsd
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
data Resource = Resource
        { resource_identifier :: Identifier
          -- ^ A persistent identifier that identifies a resource.
        , resource_creators :: Creators
        , resource_titles :: Titles
        , resource_publisher :: Publisher
        , resource_publicationYear :: YearType
        , resource_resourceType :: ResourceType
        , resource_subjects :: Maybe Subjects
        , resource_contributors :: Maybe Contributors
        , resource_dates :: Maybe Dates
        , resource_language :: Maybe Xs.Language
          -- ^ Primary language of the resource. Allowed values are taken 
          --   from IETF BCP 47, ISO 639-1 language codes.
        , resource_alternateIdentifiers :: Maybe AlternateIdentifiers
        , resource_relatedIdentifiers :: Maybe RelatedIdentifiers
        , resource_sizes :: Maybe Sizes
        , resource_formats :: Maybe Formats
        , resource_version :: Maybe Xsd.XsdString
        , resource_rightsList :: Maybe RightsList
        , resource_descriptions :: Maybe Descriptions
        , resource_geoLocations :: Maybe GeoLocations
        , resource_fundingReferences :: Maybe FundingReferences
        }
        deriving (Eq,Show)
instance SchemaType Resource where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Resource
            `apply` parseSchemaType "identifier"
            `apply` parseSchemaType "creators"
            `apply` parseSchemaType "titles"
            `apply` parseSchemaType "publisher"
            `apply` parseSchemaType "publicationYear"
            `apply` parseSchemaType "resourceType"
            `apply` optional (parseSchemaType "subjects")
            `apply` optional (parseSchemaType "contributors")
            `apply` optional (parseSchemaType "dates")
            `apply` optional (parseSchemaType "language")
            `apply` optional (parseSchemaType "alternateIdentifiers")
            `apply` optional (parseSchemaType "relatedIdentifiers")
            `apply` optional (parseSchemaType "sizes")
            `apply` optional (parseSchemaType "formats")
            `apply` optional (parseSchemaType "version")
            `apply` optional (parseSchemaType "rightsList")
            `apply` optional (parseSchemaType "descriptions")
            `apply` optional (parseSchemaType "geoLocations")
            `apply` optional (parseSchemaType "fundingReferences")
    schemaTypeToXML s x@Resource{} =
        toXMLElement s []
            [ schemaTypeToXML "identifier" $ resource_identifier x
            , schemaTypeToXML "creators" $ resource_creators x
            , schemaTypeToXML "titles" $ resource_titles x
            , schemaTypeToXML "publisher" $ resource_publisher x
            , schemaTypeToXML "publicationYear" $ resource_publicationYear x
            , schemaTypeToXML "resourceType" $ resource_resourceType x
            , maybe [] (schemaTypeToXML "subjects") $ resource_subjects x
            , maybe [] (schemaTypeToXML "contributors") $ resource_contributors x
            , maybe [] (schemaTypeToXML "dates") $ resource_dates x
            , maybe [] (schemaTypeToXML "language") $ resource_language x
            , maybe [] (schemaTypeToXML "alternateIdentifiers") $ resource_alternateIdentifiers x
            , maybe [] (schemaTypeToXML "relatedIdentifiers") $ resource_relatedIdentifiers x
            , maybe [] (schemaTypeToXML "sizes") $ resource_sizes x
            , maybe [] (schemaTypeToXML "formats") $ resource_formats x
            , maybe [] (schemaTypeToXML "version") $ resource_version x
            , maybe [] (schemaTypeToXML "rightsList") $ resource_rightsList x
            , maybe [] (schemaTypeToXML "descriptions") $ resource_descriptions x
            , maybe [] (schemaTypeToXML "geoLocations") $ resource_geoLocations x
            , maybe [] (schemaTypeToXML "fundingReferences") $ resource_fundingReferences x
            ]
 
elementResource :: XMLParser Resource
elementResource = parseSchemaType "resource"
elementToXMLResource :: Resource -> [Content ()]
elementToXMLResource = schemaTypeToXML "resource"
 
newtype NonemptycontentStringType = NonemptycontentStringType Xsd.XsdString deriving (Eq,Show)
instance Restricts NonemptycontentStringType Xsd.XsdString where
    restricts (NonemptycontentStringType x) = x
instance SchemaType NonemptycontentStringType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (NonemptycontentStringType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType NonemptycontentStringType where
    acceptingParser = fmap NonemptycontentStringType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (StrLength (Occurs (Just 1) Nothing))
    simpleTypeText (NonemptycontentStringType x) = simpleTypeText x
 
-- | Uniquely identifies a creator or contributor, according to 
--   various identifier schemes.
data NameIdentifier = NameIdentifier NonemptycontentStringType NameIdentifierAttributes deriving (Eq,Show)
data NameIdentifierAttributes = NameIdentifierAttributes
    { nameIdentifierAttributes_nameIdentifierScheme :: Xsd.XsdString
    , nameIdentifierAttributes_schemeURI :: Maybe Xs.AnyURI
    }
    deriving (Eq,Show)
instance SchemaType NameIdentifier where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ do
          a0 <- getAttribute "nameIdentifierScheme" e pos
          a1 <- optional $ getAttribute "schemeURI" e pos
          reparse [CElem e pos]
          v <- parseSchemaType s
          return $ NameIdentifier v (NameIdentifierAttributes a0 a1)
    schemaTypeToXML s (NameIdentifier bt at) =
        addXMLAttributes [ toXMLAttribute "nameIdentifierScheme" $ nameIdentifierAttributes_nameIdentifierScheme at
                         , maybe [] (toXMLAttribute "schemeURI") $ nameIdentifierAttributes_schemeURI at
                         ]
            $ schemaTypeToXML s bt
instance Extension NameIdentifier NonemptycontentStringType where
    supertype (NameIdentifier s _) = s
 
newtype Edtf = Edtf Xsd.XsdString deriving (Eq,Show)
instance Restricts Edtf Xsd.XsdString where
    restricts (Edtf x) = x
instance SchemaType Edtf where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (Edtf x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType Edtf where
    acceptingParser = fmap Edtf acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (Pattern (-)?[0-9]{4}(-[0-9]{2})?(-[0-9]{2})?(T([0-9]{2}:){2}[0-9]{2}Z)?)
    --      (Pattern \d{2}(\d{2}|\?\?|\d(\d|\?))(-(\d{2}|\?\?))?~?\??)
    --      (Pattern \d{6}(\d{2}|\?\?)~?\??)
    --      (Pattern \d{8}T\d{6})
    --      (Pattern ((-)?(\d{4}(-\d{2})?(-\d{2})?)|unknown)/((-)?(\d{4}(-\d{2})?(-\d{2})?)|unknown|open))
    simpleTypeText (Edtf x) = simpleTypeText x
 
-- | Uniquely identifies an affiliation, according to various 
--   identifier schemes.
data Affiliation = Affiliation NonemptycontentStringType AffiliationAttributes deriving (Eq,Show)
data AffiliationAttributes = AffiliationAttributes
    { affiliationAttributes_affiliationIdentifier :: Maybe Xsd.XsdString
    , affiliationAttributes_affiliationIdentifierScheme :: Maybe Xsd.XsdString
    , affiliationAttributes_schemeURI :: Maybe Xs.AnyURI
    }
    deriving (Eq,Show)
instance SchemaType Affiliation where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ do
          a0 <- optional $ getAttribute "affiliationIdentifier" e pos
          a1 <- optional $ getAttribute "affiliationIdentifierScheme" e pos
          a2 <- optional $ getAttribute "schemeURI" e pos
          reparse [CElem e pos]
          v <- parseSchemaType s
          return $ Affiliation v (AffiliationAttributes a0 a1 a2)
    schemaTypeToXML s (Affiliation bt at) =
        addXMLAttributes [ maybe [] (toXMLAttribute "affiliationIdentifier") $ affiliationAttributes_affiliationIdentifier at
                         , maybe [] (toXMLAttribute "affiliationIdentifierScheme") $ affiliationAttributes_affiliationIdentifierScheme at
                         , maybe [] (toXMLAttribute "schemeURI") $ affiliationAttributes_schemeURI at
                         ]
            $ schemaTypeToXML s bt
instance Extension Affiliation NonemptycontentStringType where
    supertype (Affiliation s _) = s
 
newtype YearType = YearType Xs.Token deriving (Eq,Show)
instance Restricts YearType Xs.Token where
    restricts (YearType x) = x
instance SchemaType YearType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (YearType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType YearType where
    acceptingParser = fmap YearType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (Pattern [\d]{4})
    simpleTypeText (YearType x) = simpleTypeText x
 
data Point = Point
        { point_pointLongitude :: [LongitudeType]
        , point_pointLatitude :: [LatitudeType]
        }
        deriving (Eq,Show)
instance SchemaType Point where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Point
            `apply` between (Occurs (Just 1) Nothing)
                            (parseSchemaType "pointLongitude")
            `apply` between (Occurs (Just 1) Nothing)
                            (parseSchemaType "pointLatitude")
    schemaTypeToXML s x@Point{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "pointLongitude") $ point_pointLongitude x
            , concatMap (schemaTypeToXML "pointLatitude") $ point_pointLatitude x
            ]
 
data Box = Box
        { box_westBoundLongitude :: [LongitudeType]
        , box_eastBoundLongitude :: [LongitudeType]
        , box_southBoundLatitude :: [LatitudeType]
        , box_northBoundLatitude :: [LatitudeType]
        }
        deriving (Eq,Show)
instance SchemaType Box where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Box
            `apply` between (Occurs (Just 1) Nothing)
                            (parseSchemaType "westBoundLongitude")
            `apply` between (Occurs (Just 1) Nothing)
                            (parseSchemaType "eastBoundLongitude")
            `apply` between (Occurs (Just 1) Nothing)
                            (parseSchemaType "southBoundLatitude")
            `apply` between (Occurs (Just 1) Nothing)
                            (parseSchemaType "northBoundLatitude")
    schemaTypeToXML s x@Box{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "westBoundLongitude") $ box_westBoundLongitude x
            , concatMap (schemaTypeToXML "eastBoundLongitude") $ box_eastBoundLongitude x
            , concatMap (schemaTypeToXML "southBoundLatitude") $ box_southBoundLatitude x
            , concatMap (schemaTypeToXML "northBoundLatitude") $ box_northBoundLatitude x
            ]
 
newtype LongitudeType = LongitudeType Xs.Float deriving (Eq,Show)
instance Restricts LongitudeType Xs.Float where
    restricts (LongitudeType x) = x
instance SchemaType LongitudeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (LongitudeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType LongitudeType where
    acceptingParser = fmap LongitudeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs (Just (-180)) (Just 180)))
    simpleTypeText (LongitudeType x) = simpleTypeText x
 
newtype LatitudeType = LatitudeType Xs.Float deriving (Eq,Show)
instance Restricts LatitudeType Xs.Float where
    restricts (LatitudeType x) = x
instance SchemaType LatitudeType where
    parseSchemaType s = do
        e <- element [s]
        commit $ interior e $ parseSimpleType
    schemaTypeToXML s (LatitudeType x) = 
        toXMLElement s [] [toXMLText (simpleTypeText x)]
instance SimpleType LatitudeType where
    acceptingParser = fmap LatitudeType acceptingParser
    -- XXX should enforce the restrictions somehow?
    -- The restrictions are:
    --      (RangeR (Occurs (Just (-90)) (Just 90)))
    simpleTypeText (LatitudeType x) = simpleTypeText x
