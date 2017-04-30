package brackenapp
package comet

import net.liftweb._
import http._
import actor._
import util._
import Helpers._
import common._
import brackenapp.model.{Page => PageModel, Cart,Tab,Transaction}
import mongodb.BsonDSL._
import scala.xml._
import xml.Text
import net.liftweb.http.js.JE._
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsExp
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.jquery.JqJE._
import net.liftweb.http.js.jquery.JqJsCmds
import net.liftweb.http.js.jquery.JqJsCmds.JqSetHtml
import config.Site
import scala.collection._
import com.foursquare.rogue.LiftRogue._
import org.bson.types.ObjectId

case class TabBalanceChanged(tabId:ObjectId,newBalance:Double)

case class TabChanged(tabId:ObjectId,newStatus:Int)

case class TabCartChangedList(tabId:ObjectId, carts:List[CartChanged]) 

case class CartChanged(tabId:ObjectId,cartId:ObjectId, newStaus:Int) 

case class CartChangedList(carts:List[CartChanged])

case class WaitTabChange(tabId:ObjectId, callback: (Any) => Unit)

object OrderServer extends Logger{
    private var pages = Map[String,OrderServer]()
     
    def findOrCreatePage(pageId:String) = synchronized {
        pages.get(pageId) getOrElse {
          val orderServer = new OrderServer()
          pages += (pageId -> orderServer)
          orderServer
        }
    }
     
    def findPage(pageId:String) = {
        pages.get(pageId)
    }
     
}
 
class OrderServer extends LiftActor with ListenerManager with Logger{
    private var apiListeners = List[WaitTabChange]()
    private var msg:Any = _
     
    def createUpdate = msg
 
    def updateTabListeners() = {
        apiListeners = apiListeners.flatMap( w => {
			msg match {
		        case cartsList:CartChangedList => {
		            cartsList.carts.filter( w.tabId == _.tabId ) match {
		                case Nil => List(w)
		                case list=> w.callback( TabCartChangedList(w.tabId,list)); Nil
		            }
				}
				case balance:TabBalanceChanged => {
					if( w.tabId == balance.tabId ){
						w.callback(balance)
						Nil
						} else List(w)
				}
				case status:TabChanged => {
					if( w.tabId == status.tabId ){
						w.callback(status)
						Nil
						} else List(w)
				}
			}
        })
    }
 
    override def lowPriority = {
        case carts:CartChangedList => msg = carts; updateTabListeners(); updateListeners()
		case balance:TabBalanceChanged => msg = balance; updateTabListeners(); updateListeners()
		case status:TabChanged => msg = status; updateTabListeners(); updateListeners()
        case w:WaitTabChange => apiListeners = apiListeners:+w
    }
}

class TabsActor extends CometActor with CometListener with Logger {
	private val page = Site.currentPage.get

	def registerWith = OrderServer.findOrCreatePage(page.id.get.toString)
	
	private lazy val cartHtml = ".cart ^^" #> "**" apply defaultHtml
	
	private lazy val tabHtml = ".tab ^^" #> "**" apply defaultHtml
	
	private var renderedTabs = mutable.Set[ObjectId]()

	override def lowPriority = {
		case CartChangedList(carts) => {
			var updates = updateTabsHeader &
				carts.groupBy(_.tabId).map{case (tabId,carts) => (tabId, updateOwe(tabId))}.values.reduce( _ & _) &
				Cart.where(_.id in carts.map(_.cartId) ).fetch().map( c =>
					if(c.status.get == Cart.Received) addCart(c) else updateCart(c))
			partialUpdate( updates )}
		case change:TabBalanceChanged => { 
			partialUpdate( 
				updateOwe(change.tabId) & 
				Jq("#tab"+change.tabId+" .dept")  ~> JsFunc("html", amountFormat(change.newBalance) ) )
		}
		case change:TabChanged => {
			if(change.newStatus == Tab.Checkout){
				renderedTabs -= change.tabId
				var updates = updateTabsHeader & JqId("tabsContainer")  ~> JsFunc("masonry",Str("remove"),JqId("tab"+change.tabId))
				partialUpdate(updates)
			}
		}
	}
	
	def updateTabsHeader = {
		val (r,p) = getReceivedAndProcesingTotal( getTabCartsMap.values.flatten )
		SetHtml("cartReceivedTotal", Text(r.toString) ) & 
			SetHtml("cartProccesingTotal", Text(p.toString) )
	}
	
	def updateOwe(tabId:ObjectId): JsCmd = Jq("#tab"+tabId+" .owe")  ~> JsFunc("html", Tab.find(tabId).map( 
			t => amountFormat(t.totalAmount) ).openOr("0") :String ) 
	
	def timeago(cartId:String) = Jq("#"+cartId+" a") ~> JsFunc("timeago")
	
	def checkoutTab(tabId:ObjectId)() {
		Tab.find(tabId).map( _.status(Tab.Checkout).updateAndNotify )
	}
	
	def updateCart(cart:Cart) : JsCmd = Replace(cart.id.get.toString, renderCart(cart,false)(cartHtml)) & 
		Call("window.cartUpdated",Str(cart.id.get.toString))
	
	def addCart(cart:Cart) : JsCmd ={
		if(renderedTabs.contains(cart.tabId.get)){
			JqId("cartsTab" + cart.tabId.get) ~> JqPrepend(renderCart(cart,true)(cartHtml)) &
				JqId("tabsContainer")  ~> JsFunc("masonry",Str("layout")) & timeago(cart.id.get.toString)
		}
		else
			JqId("tabsContainer") ~> JqAppend(renderTab(Tab.find(cart.tabId.get).get,List(cart))(tabHtml)) &
				JqId("tabsContainer")  ~> JsFunc("masonry",Str("appended"),JqId("tab"+cart.tabId.get)) & timeago(cart.id.get.toString) &
					Call("window.tabAdded",Str(cart.tabId.get.toString))
	}
	
	def updateCartPressed(cart:Cart,status:Int)() = {
		cart.status(status).update
		Jq("#"+cart.id.get.toString+" button") ~> JqAttr("disabled","true")
	}

	def renderCart(cart:Cart,collapsed:Boolean) = {
		".cart [id]" #> cart.id &
		".cart [data-status]" #> cart.status &
		".cart [class+]" #> (cart.status.get match {
			case Cart.Received => "panel-danger cart-received"
			case Cart.Processing => "panel-warning cart-processing"
			case Cart.Delivered => "panel-success cart-delivered"
			case Cart.Canceled => "panel-info cart-canceled"
			case _ => "panel-info" }) &
		".accordion-toggle [data-parent+]" #> name andThen
		".accordion-toggle [href+]" #> cart.id &
		".panel-collapse [id+]" #> cart.id &
		".panel-collapse [class+]" #> (if(collapsed) "collapsed" else "in") &
		".date [title]" #> cart.createdAt.asHtml &
		".item *" #> cart.items.get.map{ item =>
			".quantity *" #> item.quantity &
			".item-name *" #> item.name } &	
			".process-cart-button" #> (cart.status.get match {
				case Cart.Received => "button [onClick]" #> SHtml.ajaxInvoke(updateCartPressed(cart,Cart.Processing))
				case _ => "*" #> (None: Option[NodeSeq]) }) &
			".deliver-cart-button" #> (cart.status.get match {
				case Cart.Processing => "button [onClick]" #> SHtml.ajaxInvoke(updateCartPressed(cart,Cart.Delivered))
				case _ => "*" #> (None: Option[NodeSeq]) }) &
			".cancel-cart-button" #> (cart.status.get match {
				case Cart.Canceled | Cart.Delivered => "*" #> (None: Option[NodeSeq])
				case _ => "button [onClick]" #> SHtml.ajaxInvoke(updateCartPressed(cart,Cart.Canceled)) })
	}
	
	def amountFormat(amount:Double) =  "%.2f".format(amount)
	
	def renderBalance(tab:Tab) = {
		".owe *" #> amountFormat(tab.totalAmount) &
		".dept *" #> amountFormat(Tab.totalPayment(tab.id.get)) &
		".currency *" #> page.currency.get
	}
  
	def renderTab(tab:Tab,carts:Iterable[Cart]) ={
		val id = tab.id.get
		renderedTabs += id
		".tab [id]" #> ("tab" + id) &
		".tab-name *+" #> tab.tableId.get &
		".tab-balance *" #> renderBalance(tab) &
		".checkout-tab-button [onClick]" #>  SHtml.ajaxInvoke( checkoutTab(id) ) &
		".panel-group [id+]" #> id.toString &
		".cart" #> carts.map { renderCart(_,true) }
	}
	
	def getReceivedAndProcesingTotal(carts:Iterable[Cart]) = {
		carts.foldLeft((0,0)) { ( t, cart) => 
			if (cart.status.get == Cart.Received) 
				(t._1+1,t._2)
			else if (cart.status.get == Cart.Processing)
				(t._1,t._2+1)
			else	
				(t._1,t._2)
		 }
	}
	
	def renderTabsHeader(carts:Iterable[Cart]) ={
		val (r,p) = getReceivedAndProcesingTotal(carts)
		"#cartReceivedTotal *" #> r &
		"#cartProccesingTotal *" #> p
	}
	
	def getTabCartsMap = {
		val tabs = Tab.where( _.pageId eqs page.id.get).and(_.status eqs Tab.Open).fetch.map{ t => (t.id.get, t) }.toMap
		val carts = Cart.where( _.tabId in tabs.keys ).fetch
		carts.groupBy( _.tabId.get ).map { case (tabId,carts) => (tabs(tabId), carts ) } 
	}

	def render = {
		renderedTabs.clear()
		val tabs = getTabCartsMap
		".tabs-header" #> renderTabsHeader(tabs.values.flatten) &
		".tab" #>  tabs.map { case (tab,carts) => (tab, renderTab(tab ,carts )) }.values 
	}
}