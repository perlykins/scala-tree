import scala.collection.mutable.{ArrayBuffer, Stack}

trait TreeSvc:
    def build(source: String): Option[Node]
    def find(label: String, tree: Node): Option[Node]
    def print(tree: Node): Unit

case class Node(label: String, children: ArrayBuffer[Node])

object TreeService extends TreeSvc:

    override def build(source: String): Option[Node] = {
        val data = read(source)
            
        val root = Node("root", ArrayBuffer())

        val stack = Stack[(Int, Node)]()
        stack.push((0, root))

        for (line <- data.tail) {
            val node = Node(line.last, ArrayBuffer[Node]())

            val level = getLevel(line)

            stack.popWhile((l, _) => l >= level)
            
            val (topLevel, top) = stack.top
            top.children.append(node)
            stack.update(0, (topLevel, top))

            stack.push((level, node))
        }

        Some(root)
    }

    def read(source: String): Seq[Seq[String]] = {
        val bufferedSource = io.Source.fromFile(source)
        val lines = for (line <- bufferedSource.getLines.toSeq) yield {
            line.split(";").map(_.trim).toSeq
        }
        bufferedSource.close

        lines
    }

    def getLevel(line: Seq[String]): Int =
        line.takeWhile(_.length() == 0).size + 1

    override def find(label: String, tree: Node): Option[Node] = {
        val stack = Stack[Node]()
        stack.push(tree)

        while (!stack.isEmpty) {
            val node = stack.pop()
            if (node.label == label) {
                return Some(node)
            } else {
                stack.pushAll(node.children)
            }
        }
        None
    }

    override def print(tree: Node): Unit = {
        val stack = Stack[Node]()
        stack.push(tree)

        while (!stack.isEmpty) {
            val node = stack.pop()
            println(node.label)
            stack.pushAll(node.children)
        }

    }
