-- | Types that are most likely to be used directly by 
-- | other libraries and applications.
module DataCite.Types.Common where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Data.Enum (class BoundedEnum, class Enum, class SmallBounded, upFromIncluding)
import Data.Generic.Rep (class Generic, Constructor(..), Sum(..), NoArguments(..), to)
import Data.Generic.Rep.Bounded as GBounded
import Data.Generic.Rep.Enum as GEnum
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord as GOrd
import Data.Generic.Rep.Show (genericShow)
import Data.String.NonEmpty (NonEmptyString)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Unfoldable1 (class Unfoldable1)
import Foreign (Foreign)
import Foreign as Foreign
import Simple.JSON as JSON

-- | The type of the Identifier and RelatedIdentifier.
-- | see datacite-relatedIdentifierType-v4.xsd for the source of truth
data IdentifierType
  = ARK      -- ^ Archival Resource Key
  | ArXiv    -- ^ arxiv.org
  | Bibcode
  | DOI
  | EAN13
  | EISSN
  | Handle
  | IGSN
  | ISBN
  | ISSN
  | ISTC
  | LISSN
  | LSID
  | PMID
  | PURL
  | UPC
  | URL
  | URN
  -- | W3Id -- DC 4.3

derive instance genericIdentifierType :: Generic IdentifierType _
instance showIdentifierType :: Show IdentifierType where
  show ArXiv = "arXiv"
  show Bibcode = "bibcode"
  -- show W3Id = "w3id"  -- | W3Id -- DC 4.3
  show other = genericShow other
instance eqIdentifierType :: Eq IdentifierType where
  eq = genericEq
instance ordIdentifierType :: Ord IdentifierType where
  compare x y = GOrd.genericCompare x y
instance boundedIdentifierType :: Bounded IdentifierType where
  bottom = GBounded.genericBottom
  top = GBounded.genericTop
instance enumIdentifierType :: Enum IdentifierType where
  pred = GEnum.genericPred
  succ = GEnum.genericSucc
instance boundedEnumIdentifierType :: BoundedEnum IdentifierType where
  cardinality = GEnum.genericCardinality
  toEnum = GEnum.genericToEnum
  fromEnum = GEnum.genericFromEnum
instance smallBoundedIdentifierType :: SmallBounded IdentifierType

allIdentifierTypes :: forall u. Unfoldable1 u => u IdentifierType
allIdentifierTypes = upFromIncluding bottom

instance identifierTypeReadForeign :: JSON.ReadForeign IdentifierType where
  readImpl = enumReadForeign

-- | Description of the relationship of the resource being
--   registered (A) and the related resource (B).
--   See datacite-relationType-v4.xsd
data RelationType =
  IsCitedBy
  | Cites
  | IsSupplementTo
  | IsSupplementedBy
  | IsContinuedBy
  | Continues
  | IsNewVersionOf
  | IsPreviousVersionOf
  | IsPartOf
  | HasPart
  | IsReferencedBy
  | References
  | IsDocumentedBy
  | Documents
  | IsCompiledBy
  | Compiles
  | IsVariantFormOf
  | IsOriginalFormOf
  | IsIdenticalTo
  | HasMetadata
  | IsMetadataFor
  | Reviews
  | IsReviewedBy
  | IsDerivedFrom
  | IsSourceOf
{-   | Describes    -- DC 4.3
  | IsDescribedBy
  | HasVersion
  | IsVersionOf
  | Requires
  | IsRequiredBy
  | Obsoletes
  | IsObsoletedBy
 -}
derive instance genericRelationType :: Generic RelationType _
instance showRelationType :: Show RelationType where
  show = genericShow
instance eqRelationType :: Eq RelationType where
  eq = genericEq
instance ordRelationType :: Ord RelationType where
  compare x y = GOrd.genericCompare x y
instance boundedRelationType :: Bounded RelationType where
  bottom = GBounded.genericBottom
  top = GBounded.genericTop
instance enumRelationType :: Enum RelationType where
  pred = GEnum.genericPred
  succ = GEnum.genericSucc
instance boundedEnumRelationType :: BoundedEnum RelationType where
  cardinality = GEnum.genericCardinality
  toEnum = GEnum.genericToEnum
  fromEnum = GEnum.genericFromEnum
instance smallBoundedRelationType :: SmallBounded RelationType

allRelationTypes :: forall u. Unfoldable1 u => u RelationType
allRelationTypes = upFromIncluding bottom

instance relationTypeReadForeign :: JSON.ReadForeign RelationType where
  readImpl = enumReadForeign

-- | The general type of a resource. See datacite-resourceType-v4.xsd
data ResourceTypeGeneral =
  Audiovisual
  -- | DataPaper -- DC 4.3
  | Dataset
  | Event
  | Image
  | InteractiveResource
  | Model
  | PhysicalObject
  | ResourceCollection
  | Service
  | Software
  | Sound
  | Text
  | Workflow
  | Other

derive instance genericResourceTypeGeneral :: Generic ResourceTypeGeneral _
instance showResourceTypeGeneral :: Show ResourceTypeGeneral where
  show = genericShow
instance eqResourceTypeGeneral :: Eq ResourceTypeGeneral where
  eq = genericEq
instance ordResourceTypeGeneral :: Ord ResourceTypeGeneral where
  compare x y = GOrd.genericCompare x y
instance boundedResourceTypeGeneral :: Bounded ResourceTypeGeneral where
  bottom = GBounded.genericBottom
  top = GBounded.genericTop
instance enumResourceTypeGeneral :: Enum ResourceTypeGeneral where
  pred = GEnum.genericPred
  succ = GEnum.genericSucc
instance boundedEnumResourceTypeGeneral :: BoundedEnum ResourceTypeGeneral where
  cardinality = GEnum.genericCardinality
  toEnum = GEnum.genericToEnum
  fromEnum = GEnum.genericFromEnum
instance smallBoundedResourceTypeGeneral :: SmallBounded ResourceTypeGeneral

allResourceTypeGenerals :: forall u. Unfoldable1 u => u ResourceTypeGeneral
allResourceTypeGenerals = upFromIncluding bottom

instance resourceTypeGeneralReadForeign :: JSON.ReadForeign ResourceTypeGeneral where
  readImpl = enumReadForeign


type BaseIdRows r = (
  identifier :: NonEmptyString
, identifierType :: IdentifierType
| r
)
type Identifier = Record (BaseIdRows())

type RelatedIdentifierRows = BaseIdRows (relationType :: RelationType)
type RelatedIdentifier = Record RelatedIdentifierRows

type AltId = Record (
  alternateIdentifier :: NonEmptyString
, alternateIdentifierType :: IdentifierType
)

altIdToId ::  AltId -> Identifier
altIdToId altId = {
  identifier: altId.alternateIdentifier
, identifierType: altId.alternateIdentifierType
}

-- The below are quite generic and are copied from
-- https://purescript-simple-json.readthedocs.io/en/latest/generics-rep.html

enumReadForeign :: forall a rep
   . Generic a rep
  => EnumReadForeign rep
  => Foreign
  -> Foreign.F a
enumReadForeign f =
  to <$> enumReadForeignImpl f

class EnumReadForeign rep where
  enumReadForeignImpl :: Foreign -> Foreign.F rep

instance sumEnumReadForeign ::
  ( EnumReadForeign a
  , EnumReadForeign b
  ) => EnumReadForeign (Sum a b) where
  enumReadForeignImpl f
      = Inl <$> enumReadForeignImpl f
    <|> Inr <$> enumReadForeignImpl f

instance constructorEnumReadForeign ::
  ( IsSymbol name
  ) => EnumReadForeign (Constructor name NoArguments) where
  enumReadForeignImpl f = do
    s <- JSON.readImpl f
    if s == name
        then pure $ Constructor NoArguments
        else throwError <<< pure <<< Foreign.ForeignError $
            "Enum string " <> s <> " did not match expected string " <> name
    where
      name = reflectSymbol (SProxy :: SProxy name)