package brackenapp.model

import brackenapp.MongoBaseSpec

/**
 * Created with IntelliJ IDEA.
 * User: osmanalpay
 * Date: 2/23/13
 * Time: 5:47 PM
 * To change this template use File | Settings | File Templates.
 */
class PageSpec extends MongoBaseSpec {
  "Inventory" should {
    "create, validate, save, and retrieve properly" in {
      var inventory = Inventory.createRecord

      val errs = inventory.validate
      if (errs.length > 0) {
        fail("Validation error: "+errs.mkString(", "))
      }
      inventory.save(true)

      val inventoryFromDb = Inventory.find(inventory.id.is)
      inventoryFromDb.isDefined should equal (true)
      inventoryFromDb.map(u => u.id.is should equal (inventory.id.is))
    }
  }

}
