package brackenapp
package config

import model.User

import net.liftweb._
import common._
import common.Full
import http.{TransientRequestVar, RequestVar, SessionVar, PlainTextResponse, S}
import sitemap._
import sitemap.Loc._
import brackenapp.model.{Page, Content}

import net.liftmodules.mongoauth.Locs
import sitemap.Loc.CalcValue
import sitemap.Loc.LocGroup

object MenuGroups {
  val SettingsGroup = LocGroup("settings")
  val TopBarGroup = LocGroup("topbar")
  val PageGroup = LocGroup("page")
  val ContentGroup = LocGroup("content")
}

/*
 * Wrapper for Menu locations
 */
case class MenuLoc(menu: Menu) {
  lazy val url: String = S.contextPath + menu.loc.calcDefaultHref
  lazy val fullUrl: String = S.hostAndPath + menu.loc.calcDefaultHref
}

object Site extends Locs with Logger {

  import MenuGroups._

  // locations (menu entries)
  val home = MenuLoc(Menu.i("Anasayfa") / "index" >> TopBarGroup)
  val loginToken = MenuLoc(buildLoginTokenMenu)
  val logout = MenuLoc(buildLogoutMenu)
  /*
  private val profileParamMenu = Menu.param[User]("User", S ? "Profil",
    User.find _,
    _.id.get.toString
  ) / "settings" / "account"  >> Loc.CalcValue(() => User.currentUser)
  lazy val profileLoc = profileParamMenu.toLoc
  */
  val password = MenuLoc(Menu.i("Şifre") / "settings" / "password" >> RequireLoggedIn >> SettingsGroup)
  val account = MenuLoc(Menu.i("Hesap") / "settings" / "account" >> SettingsGroup >> RequireLoggedIn)
  val register = MenuLoc(Menu.i("Kaydol") / "register" >> RequireNotLoggedIn)

  //page menu items
  object currentPage extends SessionVar(Page.createRecord)

  def pageDecoder(pageId: String) = Page.find(pageId).map(currentPage(_))

  def pageEncoder(page: Page) = page.id.toString()

  val pageList = Menu.i("Sayfalar") / "page" / "list" >> RequireLoggedIn >> TopBarGroup

  val pageInfo = Menu.param[Page](
    "Page Detail",
    S ? "Bilgi",
    pageDecoder,
    pageEncoder
  ) / "page" / * / "info" >> RequireLoggedIn >> PageGroup >> CalcValue(() => Full(currentPage.get))

  val pageImages = Menu.param[Page](
    "Page Images",
    S ? "Resimler",
    pageDecoder,
    pageEncoder
  ) / "page" / * / "images" >> RequireLoggedIn >> PageGroup >> CalcValue(() => Full(currentPage.get))

  val pageContent = Menu.param[Page](
    "Page Content",
    S ? "İçerik",
    pageDecoder,
    pageEncoder
  ) / "page" / * / "content" >> RequireLoggedIn >> PageGroup >> CalcValue(() => Full(currentPage.get))

  val pageTables = Menu.param[Page](
    "Page Tables",
    S ? "Masalar",
    pageDecoder,
    pageEncoder
  ) / "page" / * / "tables" >> RequireLoggedIn >> PageGroup >> CalcValue(() => Full(currentPage.get))

  val pageComment = Menu.param[Page](
    "Page Comments",
    S ? "Yorumlar",
    pageDecoder,
    pageEncoder
  ) / "page" / * / "comment" >> RequireLoggedIn >> PageGroup >> CalcValue(() => Full(currentPage.get))

  val contentCreate = Menu.param[Page](
    "Add Content",
    S ? "İçerik Ekle",
    pageDecoder,
    pageEncoder
  ) / "content" / * / "create" >> RequireLoggedIn >> Hidden >> CalcValue(() => Full(currentPage.get))


  object requestContent extends TransientRequestVar(Content.createRecord)

  def contentDecoder(contentId: String) = Content.find(contentId).map(requestContent(_))

  def contentEncoder(content: Content) = content.id.toString()

  val contentShare = Menu.param[Content](
    "Content Share",
    S ? "Paylaş",
    contentDecoder,
    contentEncoder
  ) / "content" / * / "share" >> ContentGroup >> CalcValue(() => Full(requestContent.get))

  val contentEdit = Menu.param[Content](
    "Content Detail",
    S ? "Detay",
    contentDecoder,
    contentEncoder
  ) / "content" / * / "edit" >> RequireLoggedIn >> ContentGroup >> CalcValue(() => Full(requestContent.get))

  val contentImages = Menu.param[Content](
    "Content Images",
    S ? "Resimler",
    contentDecoder,
    contentEncoder
  ) / "content" / * / "images" >> RequireLoggedIn >> ContentGroup >> CalcValue(() => Full(requestContent.get))


  val userNews = Menu.param[User](
    "User Feed",
    S ? "Feed",
    (user: String) => User.findByStringId(user),
    (user: User) => user.id.get.toString
  ) / "user" / * / "feed" >> Hidden

  val pageFeed = Menu.param[Page](
    "Page Feed",
    S ? "Page Feed",
    (page: String) => Page.find(page),
    (page: Page) => page.id.get.toString
  ) / "page" / * / "feed" >> Hidden

  val homeFeed = MenuLoc(Menu.i("HomeFeed") / "feed" >> Hidden)

  private def menus = List(
    home.menu,
    Menu.i("Login") / "login" >> RequireNotLoggedIn,
    //register.menu,
    loginToken.menu,
    logout.menu,
    account.menu,
    password.menu,
    pageList,
    pageInfo,
    pageImages,
    pageContent,
    pageTables,
    pageComment,
    contentShare,
    contentEdit,
    contentImages,
    contentCreate,
    userNews,
    pageFeed,
    homeFeed.menu,
    Menu.i("Demo") / "demo" >> TopBarGroup,
    Menu.i("İletişim") / "contact" >> TopBarGroup,
    Menu.i("Form") / "form" >> Hidden,
    Menu.i("Throw") / "throw" >> Hidden,
    Menu.i("Error") / "error" >> Hidden,
    Menu.i("404") / "404" >> Hidden,
    Menu.i("Google") / "googlec9ae029d9bf5c474" >> Hidden
  )

  /*
   * Return a SiteMap needed for Lift
   */
  def siteMap: SiteMap = SiteMap(menus: _*)
}
