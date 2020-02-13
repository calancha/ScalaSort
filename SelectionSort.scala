// Implement selection sort algorithm
// The number of elements of the output must equal the input

object SelectionSort extends App {

  def getLists(sorted: List[Int], unsorted: List[Int]): (List[Int], List[Int]) = {
    unsorted.isEmpty match {
      // Default case: just return the input
      case true => (sorted, unsorted)
      case false =>
        // Remove the minimum element (min) from unsorted and append it to sorted
        val min = unsorted.min
        val unsortedSplit = unsorted.span(_ != min)
        getLists(sorted :+ min, unsortedSplit._1 ::: unsortedSplit._2.tail)
    }
  }

  def selectionSort(list: List[Int]): List[Int] = {
    var (sorted, unsorted) = getLists(List[Int](), list)
    sorted
  }



  // *** Tests ***
  val lists = List(
    List(8,8,4,9,7,2,1,0,4,1,8,4,7,9,1,5,8,8,7,7),
    List(8,8,6,5,0,8,8,5,5,3,1,5,7,0,1,9,6,2,8,0),
    List(7,9,2,8,8,8,0,1,4,0,1,9,9,0,1,6,0,1,3,1),
    List(0,0,1,1,4,9,4,7,8,3,6,9,4,8,4,1,8,8,7,4),
    List(3,2,6,1,6,4,4,4,4,8,0,6,7,6,5,0,3,9,6,5),
    List(7,8,2,8,3,9,0,1,3,9,6,4,3,0,5,4,8,5,1,9),
    List(3,6,1,5,3,2,5,4,6,4,1,7,1,8,2,7,7,5,8,4),
    List(1,2,9,6,7,7,3,0,8,4,3,5,5,3,9,2,5,6,0,9),
    List(9,0,7,2,5,9,4,2,0,3,2,2,3,7,9,1,5,5,3,9),
    List(2,5,7,3,5,3,5,2,4,8,8,8,0,2,3,6,2,0,8,5)
  )

  val results = List(
    List(0, 1, 1, 1, 2, 4, 4, 4, 5, 7, 7, 7, 7, 8, 8, 8, 8, 8, 9, 9),
    List(0, 0, 0, 1, 1, 2, 3, 5, 5, 5, 5, 6, 6, 7, 8, 8, 8, 8, 8, 9),
    List(0, 0, 0, 0, 1, 1, 1, 1, 1, 2, 3, 4, 6, 7, 8, 8, 8, 9, 9, 9),
    List(0, 0, 1, 1, 1, 3, 4, 4, 4, 4, 4, 6, 7, 7, 8, 8, 8, 8, 9, 9),
    List(0, 0, 1, 2, 3, 3, 4, 4, 4, 4, 5, 5, 6, 6, 6, 6, 6, 7, 8, 9),
    List(0, 0, 1, 1, 2, 3, 3, 3, 4, 4, 5, 5, 6, 7, 8, 8, 8, 9, 9, 9),
    List(1, 1, 1, 2, 2, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 7, 7, 7, 8, 8),
    List(0, 0, 1, 2, 2, 3, 3, 3, 4, 5, 5, 5, 6, 6, 7, 7, 8, 9, 9, 9),
    List(0, 0, 1, 2, 2, 2, 2, 3, 3, 3, 4, 5, 5, 5, 7, 7, 9, 9, 9, 9),
    List(0, 0, 2, 2, 2, 2, 3, 3, 3, 4, 5, 5, 5, 5, 6, 7, 8, 8, 8, 8)
  )

  val testResults = (lists zip results).map (pair => selectionSort(pair._1) == pair._2)
  if (testResults.forall(identity)) {
    println("Al results OK ")
  }
  else
  {
    println("Some results fail")
  }
}

