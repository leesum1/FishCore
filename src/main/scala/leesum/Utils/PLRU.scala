package leesum.Utils

import chisel3._
import chisel3.util.{BitPat, ListLookup, Lookup, isPow2, log2Ceil}
import leesum.GenVerilogHelper
class TreeNode {
  var left: Option[TreeNode] = None
  var right: Option[TreeNode] = None
  var value: Option[Int] = None
}

object TreeNode {
  def apply(value: Int): TreeNode = {
    val node = new TreeNode
    node.value = Some(value)
    node
  }
}

class CompleteBinaryTree {
  var root: Option[TreeNode] = None
  val queue = scala.collection.mutable.Queue[TreeNode]()
  def add(value: Int): Unit = {
    val node = TreeNode(value)
    if (root.isEmpty) {
      root = Some(node)
    } else {
      val treeNode = queue.head
      if (treeNode.left.isEmpty) {
        treeNode.left = Some(node)
      } else {
        treeNode.right = Some(node)
        queue.dequeue()
      }
    }
    queue.enqueue(node)

  }
  def find_paths(): List[List[Int]] = {
    def find_paths_recursive(
        node: Option[TreeNode],
        path: List[Int],
        all_paths: scala.collection.mutable.ListBuffer[List[Int]]
    ): Unit = {
      node match {
        case Some(n) =>
          // Append the current node's value to the path
          val newPath = path :+ n.value.get
          // If it's a leaf node, append the path to paths list
          (n.left, n.right) match {
            case (None, None) => all_paths += newPath
            case _            =>
              // Continue the search on left and right children
              find_paths_recursive(n.left, newPath, all_paths)
              find_paths_recursive(n.right, newPath, all_paths)
          }
        case None => // Nothing to do for None
      }
    }

    val all_paths = scala.collection.mutable.ListBuffer[List[Int]]()
    find_paths_recursive(root, List.empty[Int], all_paths)
    all_paths.toList
  }
  // Helper function to print a tree node with appropriate indentation
  private def printNode(node: Option[TreeNode], indent: String): Unit = {
    node match {
      case Some(n) =>
        printNode(n.right, indent + "   ")
        println(indent + n.value.get)
        printNode(n.left, indent + "   ")
      case None =>
    }
  }

  def printTree(): Unit = {
    printNode(root, "")
  }
}

object CompleteBinaryTree extends App {
  val tree = new CompleteBinaryTree
  for (j <- 0 until 7) {
    tree.add(j)
  }
  for (j <- 0 until 8) {
    tree.add(j)
  }

  tree.printTree()
  println()

  val paths = tree.find_paths()

  paths.foreach(println)
}

class PLRU(nums: Int) extends Module {
  require(nums > 1, "nums must be greater than 1")
  require(isPow2(nums), "nums must be power of 2")

  val io = IO(new Bundle {
    val update_valid = Input(Bool())
    val update_data = Input(UInt(log2Ceil(nums).W))
    val out = Output(UInt(log2Ceil(nums).W))
  })

//  val age_tree = RegInit(VecInit(Seq.fill(nums - 1)(false.B)))
  val age_tree = Seq.tabulate(log2Ceil(nums)) { i =>
    val lvl_nums = math.pow(2, i).toInt
    RegInit(VecInit(Seq.fill(lvl_nums)(false.B)))
  }

  val help_tree = new CompleteBinaryTree
  for (i <- 0 until 2 * nums - 1) {
    help_tree.add(i)
  }

  help_tree.printTree()

  val paths = help_tree.find_paths()

  def find_direction(path: List[Int]) = {
    path
      .sliding(2)
      .map { case List(p, c) =>
        if ((p + 1) * 2 == c) {
          "right"
        } else if ((p + 1) * 2 - 1 == c) {
          "left"
        } else {
          assert(false, "error")
          "error"
        }
      }
      .toList
  }

  def gen_bit_part(path: List[Int], direction: List[String]) = {
    require(
      path.length == direction.length + 1,
      "path.length must be equal to direction.length + 1"
    )

    var bits_string = ""
    for (i <- 0 until nums - 1) {
      path.zipWithIndex.find(_._1 == i) match {
        case Some((_, idx)) =>
          if (direction(idx) == "left") {
            bits_string += "0"
          } else {
            bits_string += "1"
          }
        case None =>
          bits_string += "?"
      }
    }
    (
      BitPat("b" + bits_string.reverse),
      (path.last - (nums - 1)).U(log2Ceil(nums).W)
    )
  }

  paths.foreach(println)
  val directions = paths.map(find_direction)
  directions.foreach(println)

  val bit_parts = paths
    .zip(directions)
    .map { case (path, direction) =>
      gen_bit_part(path, direction)
    }

  bit_parts.foreach(println)

//  // use decoder
//  val out1 = DecoderHelper(
//    VecInit(age_tree.flatten).asUInt,
//    0.U(log2Ceil(nums).W),
//    bit_parts
//  )

  // use lookup
  val out2 = Lookup(
    VecInit(age_tree.flatten).asUInt,
    0.U(log2Ceil(nums).W),
    bit_parts
  )

  io.out := out2

  // --------------------------------
  // update
  // --------------------------------

  for (lvl <- 0 until log2Ceil(nums)) {

    val data_msb = io.update_data.getWidth - 1

    // from high to low
    val lvl_age_val = io.update_data(data_msb - lvl)

    val age_idx = if (lvl == 0) {
      0.U(log2Ceil(nums).W)
    } else {
      VecInit(io.update_data.asBools.takeRight(lvl)).asUInt
    }

    when(io.update_valid) {
      age_tree(lvl)(age_idx) := !lvl_age_val
    }
  }

}

object PLRU {
  def apply(nums: Int, en: Bool, in: UInt): UInt = {
    require(nums > 1, "nums must be greater than 1")
    require(isPow2(nums), "nums must be power of 2")
    require(in.getWidth == log2Ceil(nums), "in width must be log2Ceil(nums)")

    val plru = Module(new PLRU(nums))
    plru.io.update_valid := en
    plru.io.update_data := in
    plru.io.out
  }
}

object gen_PLRU_verilog extends App {
  GenVerilogHelper(new PLRU(8))
}
