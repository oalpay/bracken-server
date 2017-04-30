package brackenapp.rest

import net.liftweb.http.LiftResponse
import net.liftweb.http.rest.{RestHelper,RestContinuation}
import net.liftweb.common.Logger
import net.liftweb.util.BasicTypesHelpers._
import net.liftweb.common._
import net.liftweb.common.Box._
import net.liftweb.http.LiftRules
import net.liftweb.json.Extraction 
import net.liftweb.json.JsonAST._
import net.liftweb.json.JsonDSL._
import net.liftweb.http.S
import brackenapp.model._
import brackenapp.comet._
import org.bson.types.ObjectId
import net.liftweb.util.BasicTypesHelpers._
import com.foursquare.rogue.LiftRogue._
import net.liftweb.http.auth.AuthRole




object GeoLocation{
	def unapply(loc: String): Option[(Double,Double)] = loc.split(",") match{
		case Array(AsDouble(lat),AsDouble(lng)) => Some(lat,lng)
		case _ => None
	}
}

object Security {

}

object BrackenService extends RestHelper with Logger {

	serve {
		case "api" :: "page" :: Page(p) :: Nil JsonGet req => {
			p.asJValue
		}
		case "api" :: "page" :: id :: "content" :: Nil JsonGet req => {
			Content.where(_.pageId eqs new ObjectId(id)).orderAsc(_.order).fetch().map(_.asJValue) : JArray
		}
		case "api" :: "page" :: id :: "content" :: Nil JsonPut json -> _ => {
			val newContent = for {
				JObject(content) <- json
				JField("name", JString(name)) <- content
				JField("category", JString(category)) <- content
				JField("price", JDouble(price)) <-  content
			} yield Content.createRecord.pageId(new ObjectId(id)).name(name).category(category).price(price).save.asJValue
			if (newContent.isEmpty) Failure( "can not create content" ) : Box[JValue] else Full(newContent.head)
    	}
		case "api" :: "comment" :: Nil JsonGet req => {
			for { 
				userId <- S.param("user") ?~ "userId is missing"
				pageId <- S.param("page") ?~ "pageId is missing"
			} yield Comment.findLastByUser(userId,pageId).map(_.asJValue) : JArray
		}
		case "api" :: "comment" :: Nil JsonPut json -> _ => {
			for { 
				apiComment <- json.extractOpt[ApiComment] ?~ "Can not decode json" ~> 400
				comment <- Comment.createFromApi(apiComment)
			} yield comment.save.asJValue
    	}
	}
	
	serve {
		case "api" :: "content" :: Content(c) :: Nil JsonDelete req => {
			c.delete_!
			c.asJValue
    	}
	}
	
	serve {
		case "api" :: "tab" :: Nil JsonGet req => {
			(for( tabs <- Tab.findAllFromApi(S.param("page"),S.param("table"),S.param("status")) ) yield tabs.map(_.asJValue)) : Box[JArray]
		}
		
		case "api" :: "tab" :: Nil JsonPut json -> _ => {
			Tab.createFromApi(json).map(_.asJValue)
		}
	}
	
	serve {
		case "api" :: "user" :: Nil JsonGet req => {
			for{ 
				email <- S.param("email") ?~ "email is missing"
				password <- S.param("password") ?~ "password is missing"
				user <- User.where( _.email eqs email ).and(_.password eqs password ).fetch().headOption ?~ "user and password does not match"
			} yield user.asJValue
		}
		
		case "api" :: "user" :: Nil JsonPut json -> _ => {
			User.createFromApi(json).map(_.asJValue)
		}
		
		case "api" :: "user" :: User(user) :: "card" :: Nil JsonPost json -> _ => {
			user.setCardFromApi(json)
			user.update
			user.asJValue
		}
	}
	
	case class TabUpdate(name:String, payload:JValue)
	
	def waitForChanges(tab:Tab) = {
		RestContinuation.async {
			satisfyRequest => {
				 def callback(change:Any) {
					 val update = change match {
						 case balance:TabBalanceChanged => TabUpdate("balance", balance.newBalance : JValue)
						 case list:TabCartChangedList => TabUpdate("carts",Cart.where(_.id in list.carts.map(_.cartId)).fetch.map(_.asJValue) : JArray)
						 case status:TabChanged => TabUpdate("checkout", Nil)
					 	} 	
					 satisfyRequest(Extraction.decompose(update :: Nil))
				 }
				 OrderServer.findOrCreatePage(tab.pageId.get.toString) ! WaitTabChange(tab.id.get,callback)
			}
		} : JArray
	}
	serve {
		case "api" :: "tab" :: "stream" :: Nil JsonGet req => {
			//implicit val formats = Cart.formats
			for{
				tabIdStr <- S.param("tab") ?~ "tabId is missing" ~> 400
				tabId = new ObjectId(tabIdStr)
				tab <- Tab.find( tabId ) ?~ "tab not  found" ~> 400
				AsLong(lastUpdatedAt) <- S.param("lastUpdatedAt") ?~ "lastUpdatedAt not  found" ~> 400
			} yield {
				var tabUpdate:List[TabUpdate] = List()
				//first check if previous updates exists for carts
				val carts = Cart.where(_.tabId eqs tabId ).and(_.lastUpdatedAt gt lastUpdatedAt).fetch()
				if (!carts.isEmpty){
					 tabUpdate = TabUpdate("carts",carts.map(_.asJValue):JArray) :: tabUpdate
				}
				// then check if previous updates exists for trancaction
				if(Transaction.where(_.tabId eqs tabId).and(_.createdAt gt lastUpdatedAt).exists){
					tabUpdate = TabUpdate("balance", Tab.totalPayment(tabId):JValue ) :: tabUpdate
				}
				if(tabUpdate.isEmpty) waitForChanges(tab)
				else Extraction.decompose(tabUpdate)	
			 } : JValue
		}
		
		case "api" :: "cart" :: Nil JsonGet req => {
			(for( carts <- Cart.findAllFromApi(S.param("tab")) ) yield carts.map(_.asJValue)) : Box[JArray]
		}
		
		case "api" :: "cart" :: Nil JsonPut json -> _ => {
			Cart.createFromApi(json).map(_.asJValue)
		}
		
		case "api" :: "cart" :: Cart(cart) :: Nil JsonPost json -> _ => {
			Cart.updateFromApi(cart,json).map(_.asJValue)
		}
	
	}
	
	// payment
	serve {
		case "api" :: "payment" :: Nil JsonPut json -> _ => {
			Transaction.createFromApi( json ).map( _.asJValue)
		}
		case "api" :: "payment" :: Tab(tab) :: "sum" :: Nil JsonGet req => {
			("sum" -> Tab.totalPayment(tab.id.get)) : JObject
		}
	}
}