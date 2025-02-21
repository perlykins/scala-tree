import scala.collection.mutable.{ArrayBuffer, Stack}

case class Node(label: String, level: Int, children: ArrayBuffer[Node])

trait TreeSvc:
    def build(source: String): Option[Node]
    def find(label: String, tree: Node): Option[Node]
    def print(tree: Node): Unit

object TreeService extends TreeSvc:

    override def build(source: String): Option[Node] = {
        val data = read(source)
            
        val root = Node("root", 0, ArrayBuffer())

        val stack = Stack[Node]()
        stack.push(root)

        for (line <- data.tail) {
            val level = getLevel(line)

            val node = Node(line.last, level, ArrayBuffer[Node]())
            stack.popWhile(n => n.level >= level)
            
            val top = stack.top
            top.children.append(node)
            stack.update(0, top)

            stack.push(node)
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

        val baseLevel = tree.level
        while (!stack.isEmpty) {
            val node = stack.pop()
            for (i <- 1 to (node.level - baseLevel))
                printf("  ")
            println(node.label)

            stack.pushAll(node.children)
        }
    }
