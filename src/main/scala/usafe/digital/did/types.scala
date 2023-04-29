package usafe.digital.did

import cats.data.NonEmptyList
import cats.syntax.all.*
import io.lemonlabs.uri.Uri

import scala.util.control.NoStackTrace

object types:

  type DocOptionalArray[+A] = Option[NonEmptyList[A]]

  type ParsedDid = Either[DidValidationFault, Did]

  opaque type RawDid = String


  object RawDid:
    def apply(value: String): RawDid = value


  opaque type MethodName = String
  object MethodName:
    def apply(value: String): MethodName = value

  opaque type MethodSpecificId = String
  object MethodSpecificId:
    def apply(value: String): MethodSpecificId = value

  opaque type Path = String
  object Path:
    def apply(value: String): Path = value

  opaque type Fragment = String
  object Fragment:
    def apply(value: String): Fragment = value

  final case class Parameter(key: String, value: String)

  final case class Parameters(private val ps: NonEmptyList[Parameter]):
    def asList: List[Parameter] = ps.toList
    def asMultiMap: Map[String, scala.collection.immutable.Seq[String]] = ps.toList.groupMap(_.key)(_.value)



  final case class Did (
    methodName: MethodName,
    methodSpecificId: MethodSpecificId,
    methodParameters: DocOptionalArray[Parameter] = none,
    path: Option[Path] = none,
    queryParameters: DocOptionalArray[Parameter] = none,
    fragment: Option[Fragment] = none,
  )

  //TODO: Add Proof
  final case class DidDocument(
    context: NonEmptyList[Uri],
    id: Did,
    publicKey: DocOptionalArray[PublicKey],
    authentication: DocOptionalArray[Authentication],
    controller: Option[Did],
    service: DocOptionalArray[Service]
  )

  sealed trait PublicKeyType extends Product with Serializable

  object PublicKeyType:
    case object Ed25519VerificationKey2018 extends PublicKeyType
    case object RsaVerificationKey2018 extends PublicKeyType
    case object EcdsaKoblitzSignature2016 extends PublicKeyType
    case object EcdsaSecp256k1VerificationKey2019 extends PublicKeyType

    def fromString(input: String): Either[DidDocumentValidationFault, PublicKeyType] = input match
      case "Ed25519VerificationKey2018" => Ed25519VerificationKey2018.asRight
      case "RsaVerificationKey2018" => RsaVerificationKey2018.asRight
      case "EcdsaKoblitzSignature2016" => EcdsaKoblitzSignature2016.asRight
      case "EcdsaSecp256k1VerificationKey2019" => EcdsaSecp256k1VerificationKey2019.asRight
      case x => DidDocumentValidationFault(s"$x is an unsupported public key type").asLeft




  sealed trait PublicKeyEncoding extends Product with Serializable

  object PublicKeyEncoding:
    case object PublicKeyPem extends PublicKeyEncoding
    case object PublicKeyJwk extends PublicKeyEncoding
    case object PublicKeyHex extends PublicKeyEncoding
    case object PublicKeyBase64 extends PublicKeyEncoding
    case object PublicKeyBase58 extends PublicKeyEncoding
    case object PublicKeyMultibase extends PublicKeyEncoding
    case object EthereumAddress extends PublicKeyEncoding

    def fromString(input: String): Either[DidDocumentValidationFault, PublicKeyEncoding] = input match
      case "publicKeyPem" => PublicKeyPem.asRight
      case "publicKeyJwk" => PublicKeyJwk.asRight
      case "publicKeyHex" => PublicKeyHex.asRight
      case "publicKeyBase64" => PublicKeyBase64.asRight
      case "publicKeyBase58" => PublicKeyBase58.asRight
      case "publicKeyMultibase" => PublicKeyMultibase.asRight
      case "ethereumAddress" => EthereumAddress.asRight
      case x => DidDocumentValidationFault(s"$x is an unsupported public key encoding").asLeft




  final case class PublicKeyValue(
    value: String,
    publicKeyEncoding: PublicKeyEncoding
  )

  final case class PublicKey(
    id: Did,
    `type`: PublicKeyType,
    controller: Did,
    publicKeyValue: PublicKeyValue
  )

  sealed trait Authentication extends Product with Serializable

  object Authentication:
    case class Referenced(pkId: Did) extends Authentication
    case class Embedded(pk: PublicKey) extends Authentication


  final case class Service(
    id: Did,
    `type`: String,
    serviceEndpoint: Uri
  )

  final case class DidValidationFault(reason: String) extends NoStackTrace:
    override val getMessage: String = reason


  final case class DidDocumentValidationFault(reason: String) extends NoStackTrace:
    override val getMessage: String = reason

