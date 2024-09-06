package leesum.Utils

import chisel3._
import chisel3.util.{BitPat, Lookup, isPow2, log2Ceil}
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

class PLRU(nums: Int, use_decoder: Boolean = false) extends Module {
  require(nums > 1, "nums must be greater than 1")
  require(isPow2(nums), "nums must be power of 2")

  val io = IO(new Bundle {
    val update_valid = Input(Bool())
    val update_data = Input(UInt(log2Ceil(nums).W))
    val out = Output(UInt(log2Ceil(nums).W))
  })

  // tree-based pseudo-LRU
  // age_tree(0) is the root                 0
  // age_tree(1) is the first level        1   2
  // age_tree(2) is the second level      3 4 5 6
  // ....
  val age_tree = Seq.tabulate(log2Ceil(nums)) { i =>
    val lvl_nums = math.pow(2, i).toInt
    RegInit(VecInit(Seq.fill(lvl_nums)(false.B)))
  }

  val helper_tree = new CompleteBinaryTree
  for (i <- 0 until 2 * nums - 1) {
    helper_tree.add(i)
  }

  val paths_to_leaf = helper_tree.find_paths()

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

  val directions = paths_to_leaf.map(find_direction)

  val bit_parts = paths_to_leaf
    .zip(directions)
    .map { case (path, direction) =>
      gen_bit_part(path, direction)
    }

  bit_parts.foreach(println)

//  // use decoder
//  val decoder_out = DecoderHelper(
//    VecInit(age_tree.flatten).asUInt,
//    0.U(log2Ceil(nums).W),
//    bit_parts
//  )

  // use lookup
  val lookup_out = Lookup(
    VecInit(age_tree.flatten).asUInt,
    0.U(log2Ceil(nums).W),
    bit_parts
  )

  io.out := lookup_out

  // --------------------------------
  // update
  // --------------------------------

  for (lvl <- 0 until log2Ceil(nums)) {

    // from hi to lo in io.update_data, each bit represents a node in the tree
    val new_age_node_val = !VecInit(io.update_data.asBools).reverse(lvl)

    // find the offset of age node in each level
    val age_offset = if (lvl == 0) {
      0.U(log2Ceil(nums).W)
    } else {
      VecInit(io.update_data.asBools.takeRight(lvl)).asUInt
    }
    // update the age node in each level
    when(io.update_valid) {
      age_tree(lvl)(age_offset) := new_age_node_val
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
