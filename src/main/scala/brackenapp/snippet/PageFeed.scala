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


class PageFeed(page: PageModel) extends Logger {
	
  def render = {
    "body [style]" #> ("background-image:url('%s')" format page.coverImage.get)
  }

}