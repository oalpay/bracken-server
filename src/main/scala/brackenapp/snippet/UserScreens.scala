package brackenapp
package snippet

import config.Site
import lib.Gravatar
import model._

import scala.xml._

import net.liftweb._
import common._
import http.{LiftScreen, S}
import util.FieldError
import util.Helpers._

/*
 * Use for editing the currently logged in user only.
 */
sealed trait BaseCurrentUserScreen extends BaseScreen {
  object userVar extends ScreenVar(User.currentUser.openOrThrowException("User is empty"))

  override def localSetup {
    Referer(Site.account.url)
  }
}

object AccountScreen extends BaseCurrentUserScreen {
  addFields(() => userVar.get.accountScreenFields)

  def finish() {
    userVar.get.save
    S.notice("Account settings saved")
  }
}

sealed trait BasePasswordScreen {
  this: LiftScreen =>

  def pwdName: String = "Parola"
  def pwdMinLength: Int = 6
  def pwdMaxLength: Int = 32

  val passwordField = password(pwdName, "", trim,
    valMinLen(pwdMinLength, "Parola en az "+pwdMinLength+" karakter olmadılır"),
    valMaxLen(pwdMaxLength, "Parola en fazla "+pwdMaxLength+" olmadılır"),
    ("tabindex" -> "1")
  )
  val confirmPasswordField = password("Parola doğrulayın", "", trim, ("tabindex" -> "1"))

  def passwordsMustMatch(): Errors = {
    if (passwordField.get != confirmPasswordField.get)
      List(FieldError(confirmPasswordField, "Parola uyuşmalı"))
    else Nil
  }
}


object PasswordScreen extends BaseCurrentUserScreen with BasePasswordScreen {
  override def pwdName = "Yeni Şifre"
  override def validations = passwordsMustMatch _ :: super.validations

  def finish() {
    userVar.get.password(passwordField.get)
    userVar.get.password.hashIt
    userVar.get.save
    S.notice("Yeni parola kayıt edildi")
  }
}

/*
 * Use for editing the currently logged in user only.
 */
object ProfileScreen extends BaseCurrentUserScreen {
  def gravatarHtml =
    <span>
      <div class="gravatar">
        {Gravatar.imgTag(userVar.get.email.get, 60)}
      </div>
      <div class="gravatar">
        <h4>Change your avatar at <a href="http://gravatar.com" target="_blank">Gravatar.com</a></h4>
        <p>
          We are using {userVar.get.email.get}. It may take time for changes made on gravatar.com to appear on our site.
        </p>
      </div>
    </span>

  val gravatar = displayOnly("Resim", gravatarHtml)

  addFields(() => userVar.get.profileScreenFields)

  def finish() {
    userVar.get.save
    S.notice("Profil değişikliği kayıt edildi")
  }
}

// this is needed to keep these fields and the password fields in the proper order
trait BaseRegisterScreen extends BaseScreen {
  object userVar extends ScreenVar(User.regUser.get)
  override def finishButton = <button>{ S?"Kaydol" }</button> % ("class" -> "btn btn-primary") % ("tabindex" -> "1")

  addFields(() => userVar.get.registerScreenFields)
}

/*
 * Use for creating a new user.
 */
object RegisterScreen extends BaseRegisterScreen with BasePasswordScreen {
  override def validations = passwordsMustMatch _ :: super.validations

  val rememberMe = builder("", User.loginCredentials.get.isRememberMe, ("tabindex" -> "1"))
    .help(Text("Tekrar geldiğimde beni hatırla."))
    .make

  override def localSetup {
    Referer(Site.home.url)
  }

  def finish() {
    val user = userVar.get
    user.password(passwordField.get)
    user.password.hashIt
    user.save(true)
    User.logUserIn(user, true)
    if (rememberMe) User.createExtSession(user.id.get)
    S.notice("Kayıt olduğunuz için teşekkür ederiz!")
  }
}
