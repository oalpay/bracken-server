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

class ContentPaginatorSnippet(pageModal: PageModel) extends PaginatorSnippet[Content] {
	object requestContent extends RequestVar(Content.createRecord)
	
  override val itemsPerPage = 10

  override def count = Content.count(("pageId" -> pageModal.id.toString))

  override def page = Content.findAll(("pageId" -> pageModal.id.toString), ("order" -> 1), Skip(curPage * itemsPerPage), Limit(itemsPerPage))

  override def prevXml: NodeSeq = Unparsed("&laquo;")

  override def nextXml: NodeSeq = Unparsed("&raquo;")

  override def pageXml(newFirst: Long, ns: NodeSeq): NodeSeq =
    if (first == newFirst || newFirst < 0 || newFirst >= count)
      <li>
        <a href="#">
          {ns}
        </a>
      </li>
    else
      <li>
        <a href={pageUrl(newFirst)}>
          {ns}
        </a>
      </li>
	

  def renderPage = ".content-entry *" #> page.map(content => {
    ".content-image-edit [href]" #> Site.contentImages.link.createPath(content) andThen
      ".image-icon [src]" #> content.iconImage &
        ".content-order *" #> content.order.get.toInt &
        ".content-name *" #> content.name &
        ".content-description *" #> content.description &
		".content-price *" #> content.price &
        ".content-category *" #> content.category &
        ".content-edit [href]" #> Site.contentEdit.link.createPath(content) &
		".modal [id+]" #> content.id &
		".content-delete-button [href+]" #> content.id &
	  	".content-delete-confirm-button" #> SHtml.link(Site.pageContent.loc.calcDefaultHref, () => content.delete_! , Text("Sil"))
  })
}

class ContentSnippet(content:Content) extends Logger {
	def images = "name=contentId [value]" #> content.id.toString() &
	  			".image-icon [src]" #> content.iconImage.get &
	  		 	".image-cover [src]" #> content.coverImage.get
				
	def edit : NodeSeq = {
  	  ContentScreen.content.set(content)
  	  ContentScreen.toForm
	}
	
	def googlePageLocation = PageModel.find(content.pageId.get).map( page => 
						"http://maps.googleapis.com/maps/api/staticmap?zoom=17&size=350x416&sensor=false&center=" + 
						page.location.get(0) + "," + page.location.get(1) + "&markers=color:blue%7Clabel:S%7C" + 
						page.location.get(0) + "," + page.location.get(1)
					).openOr("")

	def page = PageModel.find(content.pageId.get)
	
	def share = ".pageName *" #> page.map(content.name.get.toString + " @ " + _.name.get.toString ) &
				"#shareMap [src]" #> googlePageLocation &
				".image-cover [src]" #> content.coverImage.get &
				".image-icon [src]" #> page.map(_.iconImage.get) &
				"property=og:title" #> "hello"
				
	def title = "* [content]" #> PageModel.find(content.pageId.get).map( content.name.get + " @ " + _.name.get )
	def description = "* [content]" #> ""
	def image = "* [content]" #>  (S.hostAndPath + content.iconImage.get)
	def url =  "* [content]" #>  (S.hostAndPath + "/content/" + content.id.get + "/share")
}


object ContentScreen extends BasePageScreen {

  object content extends ScreenVar[Content](Content.createRecord)
  
  val orderField = field(content.get.order.displayName, content.get.order.get.toInt)

  addFields(() => content.get.name)

  addFields(() => content.get.description)
  
  addFields(() => content.get.price)

  addFields(() => content.get.category)
  
  //addFields(() => content.get.style)

  def finish() {
	  val orderDirty = orderField.get match{
		  case dirty if dirty != content.get.order.get => {
			  content.get.order.set(dirty - 0.5)
			  true
		  }
		  case _ => false
	  }
	content.get.save(true)
	if (orderDirty) Content.updateOrders(content.get.pageId.get)
  }

}