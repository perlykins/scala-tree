@main def main(): Unit = {
  val tree = TreeService.build("/Users/cathleenperlman/Downloads/input.csv")

  val subTree = tree match 
    case Some(t) => TreeService.find("Growing of non-perennial crops", t)
    case None => None
  
  subTree match 
    case Some(t) => TreeService.print(t)
    case None => println("node not found")

  

}
