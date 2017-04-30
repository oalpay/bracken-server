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

class Page private() extends MongoRecord[Page] with ObjectIdPk[Page] {
  def meta = Page

  object name extends StringField(this, 64) {
    override def validations = valMinLen(1, "Sayfa ismi boş bırakılamaz") _ :: super.validations
  }

  object description extends TextareaField(this, 160)
  
  object currency extends StringField(this, 3)

  object user extends ObjectIdRefField(this, User) {
    private def valUser(id: ObjectId): List[FieldError] = {
      Page.count(("user" -> id)) match {
        case count if count >= 3 => List(FieldError(this, Text(S ? "En fazla 3 sayfa yaratabilirsiniz")))
        case _ => Nil
      }
    }

    override def validations = valUser _ :: super.validations
  }

  object iconImage extends StringField(this, 64) {
    override def defaultValue = "/img/80x80.png"
  }

  object coverImage extends StringField(this, 64) {
    override def defaultValue = "/img/320x367.png"
  }

  object location extends MongoListField[Page, Double](this) {
    override def defaultValue = List(0d, 0d)
  }

}

object Page extends Page with MongoMetaRecord[Page]  with Logger {
	
	ensureIndex((location.name -> "2d"))
	
    def setImageSrc(pageId: String, imageType: String, imageId: String) = {
      Page.find(pageId) match {
        case Full(page) =>
          imageType match {
            case "II" => page.iconImage(imageId).save;
            case "IC" => page.coverImage(imageId).save
            case _ => error("invalid imagetype:" + imageType)
          }
		case Empty => error("page not found:" + pageId)
        case Failure(msg, exception, chain) => error(msg)
      }
    }
	
	def unapply(id:String):Option[Page] = find(id).toOption
}
