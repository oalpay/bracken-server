package brackenapp
package model

import org.bson.types.ObjectId
import net.liftweb._
import net.liftweb.json.JsonAST._
import net.liftweb.json.JsonDSL._
import net.liftweb.json.Printer._ 
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
import Box._
import brackenapp.comet._
import java.util.Date
import java.text.SimpleDateFormat
import com.foursquare.index.{IndexedRecord}
import com.foursquare.rogue.LiftRogue._


class CartItem private () extends BsonRecord[CartItem] {
  def meta = CartItem

  object quantity extends IntField(this)
  object contentId extends ObjectIdRefField(this, Content)
  object name extends StringField(this, 64)
  object price extends DoubleField(this)
}

object CartItem extends CartItem with BsonMetaRecord[CartItem]

class Cart private () extends MongoRecord[Cart] with ObjectIdPk[Cart] with BaseRecord[Cart] with Logger {
  def meta = Cart
  
  object pageId extends ObjectIdRefField(this, Page)
  
  object tabId extends ObjectIdRefField(this, Tab){
	  def valTab(in: ObjectId) : List[FieldError] = {
		  if( Tab.find(in).isEmpty)
		  	List(FieldError(this, Text(S ? "Tab is not valid")))
		  else
			Nil
	  }
	  override def validations = valTab _ :: super.validations
  }
  
  object userId extends ObjectIdRefField(this, User)
  
  object status extends IntField(this){
	  override def defaultValue = 1
  }
  object createdAt extends TimeStampField(this)
  
  object lastUpdatedAt extends TimeStampField(this)

  object items extends BsonRecordListField(this, CartItem)
  
  override def save = {
	  lastUpdatedAt(System.currentTimeMillis)
	  super.save
	  OrderServer.findPage(pageId.get.toString).map( _ !  CartChangedList( CartChanged(tabId.get, id.get, status.get) :: Nil) )
	  this  
  }
  
  override def update = {
	  lastUpdatedAt(System.currentTimeMillis)
	  super.update
	  OrderServer.findPage(pageId.get.toString).map( _ !  CartChangedList( CartChanged(tabId.get, id.get, status.get) :: Nil) )
	  this
  }
  
  def total = status.get match {
	  case Cart.Canceled => 0d
	  case _ =>  items.get.foldLeft(0.0)( (t,i) => t+(i.quantity.get * i.price.get ))
  }
}

object Cart extends Cart with MongoMetaRecord[Cart] with Logger{
	val Received = 1
	val Processing = 2
	val Delivered = 3
	val Canceled = 4
	
	def createFromApi( json : JValue ) : Box[Cart] = {
		for{ 
			apiCart <- json.extractOpt[ApiCart] ?~ "Can not decode json"
			tab <- Tab.find(apiCart.tabId)
			cart <- Cart.createRecord.pageId(tab.pageId.get).tabId(tab.id.get).userId( new ObjectId(apiCart.userId) ).status(Received).
				items(
					for { 
						item <- apiCart.items
						content <- Content.find(item.contentId)
					} yield CartItem.createRecord.quantity(item.quantity).contentId(content.id.get).name(content.name.get).price(content.price.get)
				).validateToBox
		} yield cart.save
	}
	
	def findAllFromApi(tabId:Box[String]) : Box[List[Cart]]= {
		for (tab <- tabId ?~ "tabId is missing") yield Cart.where( _.tabId eqs new ObjectId(tab)).fetch
	}
	
    def updateFromApi(cart:Cart, json:JValue ) : Box[Cart] = {
		def updateStatus(cart:Cart,status:Int) = 
			if((cart.status.get == Received && status == Canceled))Full(cart.status(status)) 
			else ParamFailure("Invalid request",409) 
		for{
			apiStatus <- json.extractOpt[ApiStatus] ?~ "Can not decode json" ~> 400
			newCart <- updateStatus(cart,apiStatus.status)
		} yield newCart.update
    }
	
	def unapply(id:String):Option[Cart] = find(id).toOption
}

class Tab private () extends MongoRecord[Tab] with ObjectIdPk[Tab] with BaseRecord[Tab] with Logger{
  def meta = Tab
  
  object tableId extends StringField(this,60)
  object pageId extends ObjectIdRefField(this, Page)
  object status extends IntField(this){
	  override def defaultValue = Tab.Open
  }
  
  object createdAt extends TimeStampField(this)
  object lastUpdatedAt extends TimeStampField(this)
  
  override def update = {
	  lastUpdatedAt(System.currentTimeMillis)
	  super.update
  }
  
  def updateAndNotify = {
	  update
	  OrderServer.findPage(pageId.get.toString).map( _ !  TabChanged(id.get,status.get) )
  }
  
  def totalAmount = {
  	  Cart.where(_.tabId eqs id.get).fetch.foldLeft(0d)( _+_.total)
  }
}

object Tab extends Tab with MongoMetaRecord[Tab] with Logger{
	val Open = 0
	val Checkout = 1
	
	def createFromApi( json : JValue ) : Box[Tab] = {
		for { 
			 apiTab <- json.extractOpt[ApiTab] ?~ "Can not decode json" ~> 400
			 tab <- Tab.createRecord.tableId(apiTab.tableId).pageId(new ObjectId(apiTab.pageId)).validateToBox
		} yield tab.save
	}
	
	def findAllFromApi(pageId: Box[String], tableId: Box[String], status: Box[String]) : Box[List[Tab]] = {
		for {
			page <- pageId ?~ "page is missing" 
			table <- tableId ?~ "table is missing"
		} yield Tab.where(_.pageId eqs  new ObjectId(page) ).
			and(_.tableId eqs  table).andOpt(status.map(_.toInt).
			toOption)(_.status eqs _).fetch
	}
	
	def totalPayment(tabId:ObjectId) = {
		Transaction.where(_.tabId eqs tabId).fetch.foldLeft(0d)( (s,t)=> s + t.amount.get)
	}
	
	def unapply(id:String):Option[Tab] = find(id).toOption
}

case class ApiStatus(status:Int)

case class ApiCartItem(quantity: Int, contentId: String)

case class ApiCart(tabId: String, userId: String,status:Int, items: List[ApiCartItem])

case class ApiTab(tableId: String, pageId: String,status:Int)

