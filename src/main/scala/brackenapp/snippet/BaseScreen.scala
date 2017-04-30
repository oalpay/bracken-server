package brackenapp
package snippet

import scala.xml._

import net.liftweb._
import common._
import http._
import js.JsCmds._
import util.Helpers._

/*
 * Base all LiftScreens off this. Currently configured to use bootstrap.
 */

trait BaseScreen extends CssBoundLiftScreen {
	class BRCssClassBinding extends CssClassBinding {
	    override def label = "w-label"
	}
	override lazy val cssClassBinding = new BRCssClassBinding
	def formName = "customBinding"
	
	override def cancelButton = <button>{ S?"İptal" }</button>
	override def finishButton = <button>{ S?"Kaydet" }</button>

	override def additionalAttributes: MetaData = {
	val cssCls = new UnprefixedAttribute("class", "form-horizontal", Null)
	cssCls.append(super.additionalAttributes)
	}

	def displayOnly(fieldName: => String, html: => NodeSeq) =
	new Field {
	  type ValueType = String
	  override def name = fieldName
	  override implicit def manifest = buildIt[String]
	  override def default = ""
	  override def toForm: Box[NodeSeq] = Full(html)
	}
}

trait BasePageScreen extends BaseScreen {	
	override val defaultToAjax_? = true
}