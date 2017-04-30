package brackenapp
package snippet

import net.liftweb._
import mongodb.{Limit, Skip}
import http._
import common._
import util.Helpers._
import brackenapp.model.{Page, User, Content, Comment}
import mongodb.BsonDSL._
import scala.xml._
import xml.Text
import config.Site
import net.liftweb.http.js.JE._
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import com.foursquare.rogue.LiftRogue._


class CommentPaginatorSnippet(pageModel: Page) extends PaginatorSnippet[Comment] {

	override val itemsPerPage = 10

	override def count = Comment.where( _.page eqs pageModel.id.get).count()

	override def page = Comment.where(_.page eqs pageModel.id.get).skip(curPage * itemsPerPage).limit(itemsPerPage).orderDesc(_.createdAt).fetch()

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
	
	def rating(rate:Int) = ( ".progress-bar [class+]" #> (rate match {
			case 0 => "progress-bar-danger"
			case 1 => "progress-bar-info"
			case 2 => "progress-bar-success"}))


	def renderPage = ".comment-entry *" #> page.map(comment => {
	    ".comment-time [title]" #> comment.createdAt &
	    ".comment-user *" #> comment.user &
	    ".comment-service *" #> rating(comment.service.get) &
		".comment-food *" #> rating(comment.food.get) &
	    ".comment-text *" #> comment.text })
}
