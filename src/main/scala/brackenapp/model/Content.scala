package brackenapp
package model

import org.bson.types.ObjectId

import net.liftweb._
import common._
import http.{StringField => _, BooleanField => _, IntField => _, _}
import mongodb.record._
import mongodb.record.field._
import record.{LifecycleCallbacks}
import record.field._
import util.FieldError
import mongodb.BsonDSL._
import scala.xml._
import util.Helpers._


class Content private() extends MongoRecord[Content] with ObjectIdPk[Content]  with Logger {
  def meta = Content

  object pageId extends ObjectIdRefField(this, Page) {
    private def valPage(id: ObjectId): List[FieldError] = {
      Page.find(id) match {
        case Empty => List(FieldError(this, Text("Page does not exist")))
        case _ => Nil
      }
    }

    override def validations = valPage _ :: super.validations
  }
  
  object order extends DoubleField(this) {
    override val defaultValue = 1d

    override def displayName = S ? "Sıra"
	
	override def toForm = super.toForm.map {
		case elem @ Elem(_, "input", _, _, child @ _*) => elem.asInstanceOf[Elem] % ("value" -> this.valueBox.map(_.toInt.toString).openOr("")) 
	}

    def orderValidator(newValue: Double): List[FieldError] = newValue match {
      case negative if negative < 1 => List(FieldError(this, S ? "Sıra numarası pozitif olmalıdır"))
      case _ => Nil 
    }

    override def validations = orderValidator _ :: super.validations
  }

  object name extends StringField(this, 100) {
    override def displayName = S ? "İsim"

    override def validations =
      valMinLen(3, S ? "Name must be at least 3 characters") _ ::
        super.validations
  }

  object category extends StringField(this, 64) {
    override def displayName = S ? "Kategori"

    override def toForm = super.toForm match {
      case Full(elem: Elem) => {
        val categories = Content.findAll(("pageId" -> this.owner.pageId.toString)).flatMap(_.category.get match {
          case name if name.nonEmpty => List(name)
          case _ => Nil
        }).distinct
        val dataSource = "[\"" + categories.mkString("\",\"") + "\"]"
        Full(elem % ("data-source" -> dataSource) % ("data-provide" -> "typeahead") % ("class" -> "typeahead"))
      }
      case _ => Empty
    }
  }
  
  object style extends IntField(this) {
    override def displayName = S ? "Tarz"
	override def defaultValue = 1
  }

  object description extends TextareaField(this, 100) {
    override def displayName = S ? "Açıklama"
  }

  object iconImage extends StringField(this, 64)

  object coverImage extends StringField(this, 64)
  
  object price extends DoubleField(this){
  	override def displayName = S ? "Fiyat"
  }

}

object Content extends Content with MongoMetaRecord[Content]{
    def setImageSrc(contentId: String, imageType: String, imageId: String) = {
      Content.find(contentId) match {
        case Full(content) =>
          imageType match {
            case "II" => content.iconImage(imageId).save
            case "IC" => content.coverImage(imageId).save
            case _ => error("invalid imagetype:" + imageType)
          }
        case Failure(msg, exception, chain) => error(msg)
        case Empty => error("Content not found:" + contentId)
      }
    }
	
	def delete(content:Content) = {
		content.delete_!
		updateOrders(content.pageId.get)
	} 

    def updateOrders(pageId:ObjectId) = {
        Content.findAll(("pageId" -> pageId.toString),("order" -> 1)).foldLeft(1)( (i,c) => {
			c.order(i).save
			i+1;
  	  })
    }
	
	def unapply(id:String):Option[Content] = find(id).toOption
}