package cracking

import scala.collection.mutable

class Node(val value: Int) {


  var leftOffset, rightOffset, leftMargin, orderPosition, leftMarginBalanced : Int = 0

  var isLeft : Boolean = false

  var nodeLeft, nodeRight, nodeUp, nodePrevious, nodeNext: Option[Node] = None

  def balanceSpaces() : Unit = {
    if (nodeLeft.isDefined){
      nodeLeft.get.balanceSpaces()
    }

    if (nodeRight.isDefined){
      nodeRight.get.balanceSpaces()
    }

    if (nodeLeft.isDefined && nodeRight.isDefined){
      var toLeft = this.orderPosition - nodeLeft.get.orderPosition
      var toRight = nodeRight.get.orderPosition - this.orderPosition

      if (toLeft > toRight){
        this.nodeNext.get.moveOrderThisAndNext(toLeft-toRight)
      } else if (toRight > toLeft){
        this.moveOrderThisAndNext(toRight-toLeft)
      }
    }
  }

  def moveOrderThisAndNext(offset: Int) : Unit = {
    this.orderPosition += offset
    if (nodeNext.isDefined){
      nodeNext.get.moveOrderThisAndNext(offset)
    }
  }

  def computeOffsets(_leftMargin: Int): Unit = {
    if (nodeLeft.isDefined) {
      nodeLeft.get.computeOffsets(_leftMargin)
      leftOffset = nodeLeft.get.leftOffset + nodeLeft.get.rightOffset + 1
    }

    this.leftMargin = this.leftOffset + _leftMargin

    if (nodeRight.isDefined) {
      nodeRight.get.computeOffsets(this.leftMargin + 1)
      rightOffset = nodeRight.get.leftOffset + nodeRight.get.rightOffset + 1
    }

    if(nodeLeft.isDefined && nodeRight.isDefined){
      this.leftMarginBalanced = nodeLeft.get.leftMarginBalanced + math.max(this.leftMargin - nodeLeft.get.leftMargin, nodeRight.get.leftMargin - this.leftMargin)
    }else if(nodeLeft.isDefined){
      this.leftMarginBalanced = nodeLeft.get.leftMarginBalanced + (leftMargin - nodeLeft.get.leftMargin)
    }else if(nodeRight.isDefined){
      this.leftMarginBalanced = nodeRight.get.leftMarginBalanced + (leftMargin - nodeRight.get.leftMargin)
    }else{
      this.leftMarginBalanced = leftMargin
    }
  }

  def retrieveNodePreviousAncestor():Node = {
    if (nodePrevious.isDefined){
      return nodePrevious.get.retrieveNodePreviousAncestor()
    }
    this
  }

  def setOrderPositionAndSoOn(currentPosition: Int) : Unit = {
    this.orderPosition = currentPosition
    if(nodeNext.isDefined){
      nodeNext.get.setOrderPositionAndSoOn(currentPosition+2)
    }
  }

  def insert(valueToAdd: Int): Unit = {
    if (valueToAdd != value) {
      if (valueToAdd < value) {
        if (nodeLeft.isEmpty) {
          this.createNodeLeft(valueToAdd)
        } else {
          nodeLeft.get.insert(valueToAdd)
        }
      } else {
        if (nodeRight.isEmpty) {
          this.createNodeRight(valueToAdd)
        } else {
          nodeRight.get.insert(valueToAdd)
        }
      }
    }
  }

  private def createNodeRight(valueToAdd: Int) : Unit= {
    nodeRight = Some(new Node(valueToAdd))
    nodeRight.get.nodeUp = Some(this)
    nodeRight.get.nodePrevious = Some(this)
    if (this.nodeNext.isDefined) {
      nodeRight.get.nodeNext = this.nodeNext
      this.nodeNext.get.nodePrevious = nodeRight
      this.nodeNext = nodeRight
    } else {
      this.nodeNext = nodeRight
    }
  }

  private def createNodeLeft(valueToAdd: Int) : Unit= {
    nodeLeft = Some(new Node(valueToAdd))
    nodeLeft.get.isLeft = true
    nodeLeft.get.nodeUp = Some(this)
    nodeLeft.get.nodeNext = Some(this)
    if (this.nodePrevious.isDefined) {
      nodeLeft.get.nodePrevious = this.nodePrevious
      this.nodePrevious.get.nodeNext = nodeLeft
      this.nodePrevious = nodeLeft
    } else {
      this.nodePrevious = nodeLeft
    }
  }

  def isPresent(valueToFind: Int): Boolean = {
    if (value.equals(valueToFind)) return true
    if (valueToFind < value && nodeLeft.isDefined) return nodeLeft.get.isPresent(value)
    if (valueToFind > value && nodeRight.isDefined) return nodeRight.get.isPresent(value)
    false
  }


  def dispatchByLine(lineLevel: Int, matrix: mutable.MutableList[mutable.MutableList[Node]]): mutable.MutableList[mutable.MutableList[Node]] = {
    def defaultLine() = {
      var lineMatrixTemp: mutable.MutableList[Node] = mutable.MutableList()
      matrix += lineMatrixTemp
      lineMatrixTemp
    }

    var lineMatrix: mutable.MutableList[Node] = matrix.get(lineLevel).getOrElse(defaultLine())
    lineMatrix += this
    nodeLeft.map(nl => nl.dispatchByLine(lineLevel + 1, matrix))
    nodeRight.map(nr => nr.dispatchByLine(lineLevel + 1, matrix))
    matrix
  }


  def printByOrder(godMod: Boolean): String = {
    var result = ""
    var matrix = dispatchByLine(0, mutable.MutableList())
    for (lm <- matrix) {
      var margeToSupress = 0
      var margePipeToSupress = 0
      var pipeLine = ""
      var resultLine = ""
      for (n <- lm) {
        resultLine += getSpaces(n.orderPosition - margeToSupress) + n.value
        if (godMod) {
          result += ": [" + n.orderPosition + "]"
        }
        margeToSupress = n.orderPosition + 1
        if (n.value > 9){
          margeToSupress +=1
        }
        var pipePosition = 0
        if (n.nodeUp.isDefined){
          if (n.isLeft){
            pipePosition = ((n.nodeUp.get.orderPosition - n.orderPosition) / 2) + n.orderPosition
            pipeLine += getSpaces(pipePosition - margePipeToSupress) + "/"
          } else {
            pipePosition = ((n.orderPosition - n.nodeUp.get.orderPosition ) / 2) + n.nodeUp.get.orderPosition
            pipeLine += getSpaces(pipePosition - margePipeToSupress) + "\\"
          }
          margePipeToSupress = pipePosition + 1
          if (n.value > 9){
            margePipeToSupress +=1
          }
        }
      }

      result += pipeLine + "\n" + resultLine + "\n"
    }
    result
  }


  private def getSpaces(spacesCount: Int): String = {
    var spacesToPrint = ""
    for (spaces <- 0 until spacesCount) {
      spacesToPrint += " "
    }
    spacesToPrint
  }
}
