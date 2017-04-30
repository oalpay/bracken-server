package brackenapp
package model

import org.bson.types.ObjectId
import org.joda.time.DateTime

import net.liftweb._
import common._
import http.{StringField => _, BooleanField => _, IntField => _, _}
import mongodb.record._
import mongodb.record.field._
import record.field.{PasswordField => _, _}
import util.{FieldError, FieldContainer}
import net.liftmodules.mongoauth._
import net.liftmodules.mongoauth.field._
import net.liftmodules.mongoauth.model._
import xml.Text
import net.liftweb.json.JsonAST._
import net.liftweb.json.JsonDSL._
import mongodb.BsonDSL._
import Box._

case class ApiUser(email:String,name:String,password:String)

case class ApiCard(name:String,number:String,date:Int,cvc:Int)

class UserCard private () extends BsonRecord[UserCard] {
  def meta = UserCard

  object name extends StringField(this,64)
  object number extends StringField(this, 64)
  object date extends IntField(this)
  object cvc extends IntField(this)
}

object UserCard extends UserCard with BsonMetaRecord[UserCard]

class User private () extends MongoAuthUser[User] with BaseRecord[User] with ObjectIdPk[User] {
  def meta = User

  lazy val userIdAsString = id.toString

  object email extends EmailField(this, 254) {
    override def displayName = "E-Posta"
    override def setFilter = trim _ :: toLower _ :: super.setFilter

    private def valUnique(msg: => String)(value: String): List[FieldError] = {
      owner.meta.findAll(name, value).filter(_.id.get != owner.id.get).map(u =>
        FieldError(this, Text(msg))
      )
    }

    override def validations =
      valUnique("Bu e-posta adresi zaten sistemde kayıtlı") _  ::
        valMaxLen(254, "E-posta adresı en fazla 254 karakter olabilir") _ ::
        super.validations
  }
  // email address has been verified by clicking on a LoginToken link
  object verified extends BooleanField(this) {
    override def displayName = "Verified"
  }
  object password extends PasswordField(this, 6, 32) {
    override def displayName = "Şifre"
  }
  object permissions extends PermissionListField(this)
  object roles extends StringRefListField(this, Role) {
    def permissions: List[Permission] = objs.flatMap(_.permissions.get)
    def names: List[String] = objs.map(_.id.get)
  }

  lazy val authPermissions: Set[Permission] = (permissions.get ::: roles.permissions).toSet
  lazy val authRoles: Set[String] = roles.names.toSet
  lazy val fancyEmail = AuthUtil.fancyEmail(name.get, email.get)

  object locale extends LocaleField(this) {
    override def displayName = "Dil"
    override def defaultValue = "en_US"
  }
  object timezone extends TimeZoneField(this) {
    override def displayName = "Zaman Dilimi"
    override def defaultValue = "America/Chicago"
  }

  object name extends StringField(this, 64) {
    override def displayName = "İsim"

    override def validations =
      valMaxLen(64, "İsim en fazla 64 karakter olabilir") _ :: valMinLen(3, "İsim en az 3 karakter olabilir") _ ::
      super.validations
  }
  object location extends StringField(this, 64) {
    override def displayName = "Yer"

    override def validations =
      valMaxLen(64, "Yer en fazla 64 karakter olabilir") _ ::
      super.validations
  }
  object bio extends TextareaField(this, 160) {
    override def displayName = "Bio"

    override def validations =
      valMaxLen(160, "Bio en fazla 64 karakter olabilir") _ ::
      super.validations
  }
  
  object card extends BsonRecordField(this,UserCard)
  
  
	def setCardFromApi( json : JValue ) {
		implicit val formats = net.liftweb.json.DefaultFormats
		val apiCard = json.extract[ApiCard]
		card.set(UserCard.createRecord.name(apiCard.name).number(apiCard.number).date(apiCard.date).cvc(apiCard.cvc))
	}

  /*
   * FieldContainers for various LiftScreeens.
   */
  def accountScreenFields = new FieldContainer {
    def allFields = List(name, email, locale, timezone)
  }

  def profileScreenFields = new FieldContainer {
    def allFields = List(name, location, bio)
  }

  def registerScreenFields = new FieldContainer {
    def allFields = List(name,email)
  }

  def whenCreated: DateTime = new DateTime(id.get.getTime)
  
}

object User extends User with ProtoAuthUserMeta[User] with Loggable {
  import mongodb.BsonDSL._

  override def collectionName = "user.users"

  ensureIndex((email.name -> 1), true)

  def findByEmail(in: String): Box[User] = find(email.name, in)

  def findByStringId(id: String): Box[User] =
    if (ObjectId.isValid(id)) find(new ObjectId(id))
    else Empty

  override def onLogIn: List[User => Unit] = List(user => User.loginCredentials.remove())
  override def onLogOut: List[Box[User] => Unit] = List(
    x => logger.debug("User.onLogOut called."),
    boxedUser => boxedUser.foreach { u =>
      ExtSession.deleteExtCookie()
    }
  )

  /*
   * MongoAuth vars
   */
  private lazy val siteName = MongoAuth.siteName.vend
  private lazy val sysUsername = MongoAuth.systemUsername.vend
  private lazy val indexUrl = MongoAuth.indexUrl.vend
  private lazy val registerUrl = MongoAuth.registerUrl.vend
  private lazy val loginTokenAfterUrl = MongoAuth.loginTokenAfterUrl.vend

  /*
   * LoginToken
   */
  override def handleLoginToken: Box[LiftResponse] = {
    val resp = S.param("token").flatMap(LoginToken.findByStringId) match {
      case Full(at) if (at.expires.isExpired) => {
        at.delete_!
        RedirectWithState(indexUrl, RedirectState(() => { S.error("Giriş anahtarının süresi doldu") }))
      }
      case Full(at) => find(at.userId.get).map(user => {
        if (user.validate.length == 0) {
          user.verified(true)
          user.save
          logUserIn(user)
          at.delete_!
          RedirectResponse(loginTokenAfterUrl)
        }
        else {
          at.delete_!
          regUser(user)
          RedirectWithState(registerUrl, RedirectState(() => { S.notice("Lütfen kayıt formunu tamamlayınız") }))
        }
      }).openOr(RedirectWithState(indexUrl, RedirectState(() => { S.error("Kullanıcı bulunamadı") })))
      case _ => RedirectWithState(indexUrl, RedirectState(() => { S.warning("Giriş anahtarı bulunamadı") }))
    }

    Full(resp)
  }

  // send an email to the user with a link for logging in
  def sendLoginToken(user: User): Unit = {
    import net.liftweb.util.Mailer._

    val token = LoginToken.createForUserId(user.id.get)

    val msgTxt =
      """
        |Someone requested a link to change your password on the %s website.
        |
        |If you did not request this, you can safely ignore it. It will expire 48 hours from the time this message was sent.
        |
        |Follow the link below or copy and paste it into your internet browser.
        |
        |%s
        |
        |Thanks,
        |%s
      """.format(siteName, token.url, sysUsername).stripMargin

    sendMail(
      From(MongoAuth.systemFancyEmail),
      Subject("%s Password Help".format(siteName)),
      To(user.fancyEmail),
      PlainMailBodyType(msgTxt)
    )
  }

  /*
   * ExtSession
   */
  def createExtSession(uid: ObjectId) = ExtSession.createExtSession(uid)

  /*
  * Test for active ExtSession.
  */
  def testForExtSession: Box[Req] => Unit = {
    ignoredReq => {
      if (currentUserId.isEmpty) {
        ExtSession.handleExtSession match {
          case Full(es) => find(es.userId.get).foreach { user => logUserIn(user, false) }
          case Failure(msg, _, _) =>
            logger.warn("Error logging user in with ExtSession: %s".format(msg))
          case Empty =>
        }
      }
    }
  }

  // used during login process
  object loginCredentials extends SessionVar[LoginCredentials](LoginCredentials(""))
  object regUser extends SessionVar[User](createRecord.email(loginCredentials.get.email))
  
	def createFromApi( json : JValue ) : Box[User] = {
		for { 
			 apiUser <- json.extractOpt[ApiUser] ?~ "Can not decode user json" ~> 400
			 user <- User.createRecord.name(apiUser.name).email(apiUser.email).password(apiUser.password).validateToBox
		} yield user.save(true)
	}
	
	def unapply(id:String):Option[User] = find(id).toOption
}

case class LoginCredentials(email: String, isRememberMe: Boolean = false)

object SystemUser {
  private val email = "bilgi@brackenapp.com"

  lazy val user: User = User.find("email", email) openOr {
    User.createRecord
      .name("BrackenApp")
      .email(email)
      .locale("en_US")
      .timezone("America/Chicago")
      .verified(true)
      .password("529920", true)
      .save
  }
}
