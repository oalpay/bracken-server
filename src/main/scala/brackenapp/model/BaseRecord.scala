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
import record._
import record.field._
import util.FieldError
import mongodb.BsonDSL._
import scala.xml._
import util.Helpers._
import java.util.Date
import java.text.SimpleDateFormat


object IsoDateFormater extends SimpleDateFormat("yyyy-MM-dd'T'HH:mmZ")

class TimeStampField[OwnerType <: Record[OwnerType]](rec: OwnerType)  extends LongField[OwnerType](rec){
  override def defaultValue = System.currentTimeMillis
  override def asHtml = valueBox.map( t=> Text(IsoDateFormater.format(new Date(t)))) openOr Text("")
}

trait BaseRecord[OwnerType <: MongoRecord[OwnerType]] {
	self: OwnerType  =>
	
	def validateToBox: Box[OwnerType] = {
		val errors = self.validate
		if(errors.size > 0 )
			ParamFailure(errors.foldLeft("")( (errorTxt,e) => errorTxt + ";" + e.toString ),409)
			else
				Full(self)
	}
}