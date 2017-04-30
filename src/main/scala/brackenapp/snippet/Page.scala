package brackenapp
package snippet

import net.liftweb._
import mongodb.{Limit, Skip}
import http._
import common._
import util.Helpers._
import brackenapp.model.{Page => PageModel, User, Content}
import mongodb.BsonDSL._
import scala.xml._
import xml.Text
import config.Site
import net.liftweb.http.js.JE._
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._


/**
 * Declare the fields on the screen
 */

object Pages extends Logger {
	object requestPage extends RequestVar(PageModel.createRecord)

  def all = {
    User.currentUser match {
      case Full(user) => {
        val pages = PageModel.findAll(("user" -> user.id.get))
        ".page-entry" #> pages.map(page => {
          ".page-name *" #> page.name &
            ".page-desc *" #> page.description &
            ".image-cover [src]" #> page.coverImage &
            ".goto-page [href]" #> Site.pageInfo.link.createPath(page) &
			".modal [id+]" #> page.id &
			".page-delete-button [href+]" #> page.id &
            ".page-delete-confirm-button" #> SHtml.link(Site.pageList.loc.calcDefaultHref, () => page.delete_! , Text("Sil"))
			})
      }
      case _ =>
        ".page-entry" #> (None: Option[NodeSeq])
    }
  }

  def create = {
    User.currentUser match {
      case Full(user) => {
        var name = ""
        def process() = {
          val page = PageModel.createRecord.name(name).user(user.id.get)
          page.validate match {
            case Nil => {
              page.save(true)
              S.notice(S ? "Kaydedildi")
            }
            case err => S.error(S ? err.head.toString())
          }
        }
        "name=name" #> SHtml.text(name, name = _) &
          "type=submit" #> SHtml.onSubmitUnit(process)
      }
      case _ =>
        "user_login_required" #> (None: Option[NodeSeq])
    }
  }
}

class PageSideBarSnippet {
  def render = ".page-name *" #> Text(Site.currentPage.name.get)
}


class PageSnippet(page: PageModel) extends Logger {
	
  def qrCode = {
	  var table = ""
	  def process() : JsCmd = {
		var qrscr = "http://chart.apis.google.com/chart?cht=qr&chs=300x300&chl=" + page.id.get.toString() + ";"+table+"&chld=H|0"
	  	SetHtml("qrCode", 
			<div>
				<div id='section-to-print'>
	                <img id='qr-code' class='qr-code' src={qrscr}/>
				</div>
			  	<p>
					<a class='btn btn-primary' href='javascript:window.print();'><i class='icon-print icon-white'></i> Bastır</a>
			  	</p>
	        </div>)
	  }
	  "#inputTable" #> SHtml.text(table, s => table = s) &
	  "type=submit" #> SHtml.ajaxSubmit(S ? "Üret", process)
  }

  def edit = {
    var latitude :: longitude :: rest = page.location.get
    def process() = {
      page.location(List(latitude, longitude))
      page.validate match {
        case Nil => {
          page.save(true)
          S.notice(S ? "Kaydedildi")
		  S.redirectTo(Site.pageInfo.calcHref(page))
        }
        case err => S.error(S ? err.head.toString())
      }
    }
    "#inputName" #> SHtml.text(page.name.get, page.name(_)) &
      "#inputDescription" #> SHtml.textarea(page.description.get, page.description(_)) &
	  "#inputCurrency" #> SHtml.text(page.currency.get, page.currency(_)) &
      "#pageLatitude" #> SHtml.text(latitude.toString, (lat) => latitude = lat.toDouble) &
      "#pageLongitude" #> SHtml.text(longitude.toString, (lon) => longitude = lon.toDouble) &
	  "type=submit" #> SHtml.onSubmitUnit(process)
  }

  def image = {
    "name=pageId [value]" #> page.id.toString() &
      "#page-icon-image [src]" #> page.iconImage.get &
      "#page-cover-image [src]" #> page.coverImage.get
  }
  
  def linkToCreateContent = {
    ".content-add" #> SHtml.hrefFunc(() => S.redirectTo(Site.contentCreate.loc.calcDefaultHref))
  }
  
  def createContent : NodeSeq = {
	  ContentScreen.content.get.pageId(page.id.get)
	  val count = Content.count(("pageId" -> page.id.get.toString))+1
	  ContentScreen.content.get.order.set(count.toInt)
	  ContentScreen.toForm
  }
}