package cracking

object Btree {
  def from(valuesToAdd: List[Int]): Btree = {
    val btree = new Btree()
    valuesToAdd.foreach(v => btree.insert(v))
    btree
  }
}

class Btree() {
  var rootNode: Node = _

  def printByOrder(godMod: Boolean): String = {
    rootNode.printByOrder(godMod)
  }

  def insert(valuesToAdd: List[Int]): Btree = {
    valuesToAdd.foreach(v => insert(v))
    this
  }

  def insert(valueToAdd: Int): Btree = {
    if (rootNode == null) {
      rootNode = new Node(valueToAdd)
    } else {
      rootNode.insert(valueToAdd)
    }
    this
  }

  def computeOrderNumber(): Unit = {
    if(rootNode != null){
      var minValue = rootNode.retrieveNodePreviousAncestor()
      minValue.setOrderPositionAndSoOn(0)
    }
  }

  def balanceSpaces() : Unit ={
    if(rootNode != null){
      rootNode.balanceSpaces()
    }
  }

  def has(valueToFind: Int): Boolean = {
    rootNode.isPresent(valueToFind)
  }



}
