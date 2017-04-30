package brackenapp
package model

import org.bson.types.ObjectId

import net.liftweb._
import json.Extraction._
import common._
import http.{StringField => _, BooleanField => _, IntField => _, _}
import mongodb.{Limit, Skip}
import mongodb.record._
import mongodb.record.field._
import record.{LifecycleCallbacks}
import record.field._
import util.FieldError
import mongodb.BsonDSL._
import scala.xml._
import util.Helpers._
import com.foursquare.index.{IndexedRecord}
import com.foursquare.rogue.LiftRogue._

case class ApiComment(page: String, user: String, text:String, service:Int, food:Int)

class Comment private() extends MongoRecord[Comment] with ObjectIdPk[Comment] with IndexedRecord[Comment]{
  def meta = Comment
	object page extends ObjectIdRefField(this, Page)
	
	object user extends StringField(this, 64)
	
	object text extends StringField(this, 64)

	object service extends IntField(this)

	object food extends IntField(this)

	object createdAt extends TimeStampField(this)
}

object Comment extends Comment with MongoMetaRecord[Comment]  with Logger {	
	def unapply(id:String):Option[Comment] = find(id).toOption
	
	def findLastByUser(userId:String, pageId:String) = 
		Comment.where(_.page eqs new ObjectId(pageId)).and(_.user eqs userId).orderDesc(_.createdAt).limit(1).fetch()
	
	def createFromApi(comment:ApiComment) = fromJValue(decompose(comment))
}