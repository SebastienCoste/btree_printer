package cracking

import scala.collection.mutable
import scala.collection.mutable.MutableList

object Btree {
  def from(valuesToAdd: List[Int]): Btree = {
    val btree = new Btree()
    valuesToAdd.foreach(v => btree.insert(v))
    btree
  }
}

class Btree() {
  var rootNode: Node = null

  def printByLine(godMod: Boolean): String = {
    rootNode.printByLine(godMod)
  }

  def printBySpaces(godMod: Boolean): String = {
    rootNode.printBySpaces(godMod)
  }

  def printByConcat(godMod: Boolean): String = {
    rootNode.printByConcat(godMod)
  }

  def printByOrder(godMod: Boolean): String = {
    rootNode.printByOrder(godMod)
  }

  def insert(valuesToAdd: List[Int]): Btree = {
    valuesToAdd.foreach(v => insert(v))
    return this
  }

  def insert(valueToAdd: Int): Btree = {
    if (rootNode == null) {
      rootNode = new Node(valueToAdd)
    } else {
      rootNode.insert(valueToAdd)
    }
    return this
  }

  def computeOrderNumber(): Unit = {
    if(rootNode != null){
      var minValue = rootNode.getNodePreviousAncestor()
      minValue.setOrderPositionAndSoOn(0)
    }
  }

  def balanceSpaces() : Unit ={
    if(rootNode != null){
      rootNode.balanceSpaces()
    }
  }

  def has(valueToFind: Int): Boolean = {
    return rootNode.isPresent(valueToFind)
  }























}


case class Spaces(var l:Int, var r:Int)

