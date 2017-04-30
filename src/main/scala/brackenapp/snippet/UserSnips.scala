package brackenapp
package snippet

import config.Site
import lib.{Gravatar, AppHelpers}
import model.{User, LoginCredentials}

import scala.xml._

import net.liftweb._
import common._
import http.{DispatchSnippet, S, SHtml, StatefulSnippet}
import http.js.JsCmd
import http.js.JsCmds._
import util._
import Helpers._

import net.liftmodules.mongoauth.LoginRedirect
import net.liftmodules.mongoauth.model.ExtSession

sealed trait UserSnippet extends AppHelpers with Loggable {

  protected def user: Box[User]

  protected def serve(snip: User => NodeSeq): NodeSeq =
    (for {
      u <- user ?~ "User not found"
    } yield {
      snip(u)
    }): NodeSeq

  protected def serve(html: NodeSeq)(snip: User => CssSel): NodeSeq =
    (for {
      u <- user ?~ "User not found"
    } yield {
      snip(u)(html)
    }): NodeSeq

  def header(xhtml: NodeSeq): NodeSeq = serve { user =>
    <div id="user-header">
      {gravatar(xhtml)}
      <h3>{name(xhtml)}</h3>
    </div>
  }

  def gravatar(xhtml: NodeSeq): NodeSeq = {
    val size = S.attr("size").map(toInt) openOr Gravatar.defaultSize.vend

    serve { user =>
      Gravatar.imgTag(user.email.get, size)
    }
  }


  def name(xhtml: NodeSeq): NodeSeq = serve { user =>
      Text(user.name.get)
  }

  def title(xhtml: NodeSeq): NodeSeq = serve { user =>
    <lift:head>
      <title lift="Menu.title">{"BrackenApp: %*% - "+user.name.get}</title>
    </lift:head>
  }
}

object CurrentUser extends UserSnippet {
  protected def user = User.currentUser
}

object ProfileLocUser extends UserSnippet {

  protected def user = User.currentUser

  import java.text.SimpleDateFormat

  val df = new SimpleDateFormat("MMM d, yyyy")

  def profile(html: NodeSeq): NodeSeq = serve(html) { user =>
    /*val editLink: NodeSeq =
      if (User.currentUser.filter(_.id.get == user.id.get).getDefined)
        <a href={Site.editProfile.url} class="btn btn-info"><i class="icon-edit icon-white"></i> Edit Your Profile</a>
      else
        NodeSeq.Empty*/

    "#id_avatar *" #> Gravatar.imgTag(user.email.get) &
    "#id_name *" #> <h3>{user.name.get}</h3> &
    "#id_location *" #> user.location.get &
    "#id_whencreated" #> df.format(user.whenCreated.toDate).toString &
    "#id_bio *" #> user.bio.get
//    "#id_editlink *" #> editLink
  }
}

object UserLogin extends Loggable {

  def render = {
    // form vars
    var password = ""
    var hasPassword = false
    var remember = User.loginCredentials.get.isRememberMe

    val radios = SHtml.radioElem[Boolean](
      Seq(false, true),
      Full(hasPassword)
    )(it => it.foreach(hasPassword = _))

    def doSubmit(): JsCmd = {
      S.param("email").map(e => {
        val email = e.toLowerCase.trim
        // save the email and remember entered in the session var
        User.loginCredentials(LoginCredentials(email, remember))

        if (hasPassword && email.length > 0 && password.length > 0) {
          User.findByEmail(email) match {
            case Full(user) if (user.password.isMatch(password)) =>
              logger.debug("pwd matched")
              User.logUserIn(user, true)
              if (remember) User.createExtSession(user.id.get)
              else ExtSession.deleteExtCookie()
              RedirectTo(LoginRedirect.openOr(Site.pageList.loc.calcDefaultHref))
            case _ =>
              S.error("Geçersiz kullanıcı adı veya şifre")
              Noop
          }
        }
        else if (hasPassword && email.length <= 0 && password.length > 0) {
          S.error("id_email_err", "Lütfen e-posta adresinizi girin")
          Noop
        }
        else if (hasPassword && password.length <= 0 && email.length > 0) {
          S.error("id_password_err", "Lütfen parolanızı girin")
          Noop
        }
        else if (hasPassword) {
          S.error("id_email_err", "Lütfen e-posta adresinizi girin")
          S.error("id_password_err", "Lütfen parolanızı girin")
          Noop
        }
        else if (email.length > 0) {
          // see if email exists in the database
          User.findByEmail(email) match {
            case Full(user) =>
              User.sendLoginToken(user)
              User.loginCredentials.remove()
              S.notice("E-posta adresinize hesabınıza nasıl giriş yapabileceğinizi gösteren bir mesaj gönderildi")
              Noop
            case _ =>
              RedirectTo(Site.register.url)
          }
        }
        else {
          S.error("id_email_err", "Lütfen e-posta adresinizi girin")
          Noop
        }
      }) openOr {
        S.error("id_email_err", "Lütfen e-posta adresinizi girin")
        Noop
      }
    }

    def cancel() = S.seeOther(Site.home.url); Noop

    "#id_email [value]" #> User.loginCredentials.get.email &
    "#id_password" #> SHtml.password(password, password = _) &
    "#no_password" #> radios(0) &
    "#yes_password" #> radios(1) &
    "name=remember" #> SHtml.checkbox(remember, remember = _) &
    "#id_submit" #> SHtml.hidden(doSubmit)
  }
}

object UserTopbar {
  def render = {
    User.currentUser match {
      case Full(user) =>
        <ul class="nav pull-right" id="user">
          <li class="dropdown" data-dropdown="dropdown">
            <a href="#" class="dropdown-toggle" data-toggle="dropdown">
              {Gravatar.imgTag(user.email.get, 20)}
              <span>{user.email.get}</span>
              <b class="caret"></b>
            </a>
            <ul class="dropdown-menu">
              <li><a href={Site.account.menu.loc.calcDefaultHref}><i class="icon-user"></i> Hesap</a></li>
              <li><lift:Menu.item name="Account" donthide="true" linktoself="true"><i class="icon-cog"></i> Settings</lift:Menu.item></li>
              <li class="divider"></li>
              <li><lift:Menu.item name="Logout" donthide="true"><i class="icon-off"></i> Çıkış yap</lift:Menu.item></li>
            </ul>
          </li>
        </ul>
      case _ if (S.request.flatMap(_.location).map(_.name).filterNot(it => List("Login", "Register").contains(it)).isDefined) =>
        <ul class="nav pull-right">
          <li><a href="/login">Giriş yap</a></li>
        </ul>
      case _ => NodeSeq.Empty
    }
  }
}
