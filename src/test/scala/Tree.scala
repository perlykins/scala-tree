class Tree extends munit.FunSuite {

  import java.nio.file.Paths
  val path = Paths.get("src/resources/test.csv").toAbsolutePath().toString()

  test("TreeService.build cannot build tree from file that doesn't exist") {
    val tree = TreeService.build("/some/file")
    assert(tree.isEmpty)
  }

  test("TreeService.build builds a tree") {
    val tree = TreeService.build(path)
    assert(!tree.isEmpty)
  }

  test("TreeService.build correctly sets a tree root") {
    TreeService.build(path) match 
      case Some(t) => assertEquals(t.label, "root")
  }

  test("TreeService.build creates a tree root with the correct number of children") {
    TreeService.build(path) match 
      case Some(t) => {
        assertEquals(
          t.children.head.label,
          "AGRICULTURE, FORESTRY AND FISHING"
        )
        assertEquals(t.children.size, 1)
      }
  }

  test("TreeService.find yields None when a node cannot be found") {
    TreeService.build(path) match 
      case Some(t) => {
        val node = TreeService.find("i don't exist", t)
        assert(node.isEmpty)
      } 
  }

  test("TreeService.find finds leaf nodes") {
    TreeService.build(path) match 
      case Some(t) => {
        val node = TreeService.find("Growing of fibre crops", t)
        assert(!node.isEmpty)
        assert(node.get.children.isEmpty)
      } 
  }

  test("TreeService.find finds subtrees that aren't leaves") {
    TreeService.build(path) match 
      case Some(t) => {
        val node = TreeService.find("Growing of non-perennial crops", t)
        assert(!node.isEmpty)
        assertEquals(node.get.children.size, 6)
      } 
  }
}
