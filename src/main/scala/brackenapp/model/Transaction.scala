package brackenapp
package model

import net.liftweb.common._
import net.liftweb.common.Box._
import net.liftweb.record.field._
import org.bson.types.ObjectId
import net.liftweb.mongodb.record._
import net.liftweb.mongodb.record.field._
import net.liftweb.json.JsonAST._
import com.foursquare.index.{IndexedRecord}
import com.foursquare.rogue.LiftRogue._
import brackenapp.comet._

case class ApiPayment(tabId:String,userId:String,card:String,amount:Double)

class Transaction private () extends MongoRecord[Transaction] with ObjectIdPk[Transaction] with BaseRecord[Transaction] with Logger{
  def meta = Transaction
  
  object tabId extends ObjectIdRefField(this, Tab)
  object user extends ObjectIdRefField(this, User)
  object card extends StringField(this,100)
  object amount extends DoubleField(this)
  object createdAt extends TimeStampField(this)
  
  def saveAndNotifyPage(pageId:ObjectId) = {
	  save(true)
	  OrderServer.findPage(pageId.toString).map( _ !  TabBalanceChanged(tabId.get,Tab.totalPayment(tabId.get)) )
	  this
  }
}

object Transaction extends Transaction with MongoMetaRecord[Transaction] with Logger{
	def createFromApi(json: JValue) : Box[Transaction] = for { 
				 api <- json.extractOpt[ApiPayment] ?~ "Can not decode json" ~> 400
				 tabId = new ObjectId(api.tabId);
				 tab <- Tab.find(tabId)
				 tx <- Transaction.createRecord.tabId(tabId).user( new ObjectId(api.userId) ).card(api.card).amount(api.amount).validateToBox
			} yield
				tx.saveAndNotifyPage(tab.pageId.get)
}