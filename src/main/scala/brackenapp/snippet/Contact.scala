package brackenapp
package snippet

import net.liftweb._
import http._
import common._
import util.Helpers._
import scala.xml.NodeSeq
import net.liftweb.util.Mailer
import Mailer._

/**
 * Declare the fields on the screen
 */
object Contact extends Logger {

   // associate behavior with each HTML element
   def render ={
	var email = ""
	var message = ""
    def process() = {
 	   Mailer.sendMail(From("bilgi@brackenapp.com"), Subject(email), PlainMailBodyType(message),To("osman.alpay@gmail.com"))
	   S.notice(S ? "Gönderildi...")
    }
     "name=email" #> SHtml.text(email, email = _) &
     "name=message" #> SHtml.textarea(message, message = _) &
     "type=submit" #> SHtml.onSubmitUnit(process)
 	} 
 }