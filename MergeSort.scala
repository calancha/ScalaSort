// Implement qsort algorithm
// The number of elements of the output must equal the input

object MergeSort extends App {

  def merge(left: List[Int], right: List[Int]): List[Int] = {
    var result = List[Int]()

    var cleft = left
    var cright = right
    while (cleft.nonEmpty && cright.nonEmpty)
      if (cleft.head <= cright.head) {
        result = result :+ cleft.head
        cleft = cleft.tail
      }
      else
      {
        result = result :+ cright.head
        cright = cright.tail
      }

    // Either cleft or cright may have elements cleft; consume them.
    // (Only one of the following loops will actually be entered.)
    while (cleft.nonEmpty) {
      result = result :+ cleft.head
      cleft = cleft.tail
    }
    while (cright.nonEmpty) {
      result = result :+ cright.head
      cright = cright.tail
    }
    result
  }

  // Divide the list on smaller sublists; method merge merges the left and right sublists
  def mergesort(list: List[Int]): List[Int] = list.length match {
    // Base case. A list of zero or one elements is sorted, by definition.
    case 0 | 1 => list
    case _ =>
      // Recursive case. First, divide the list into equal-sized sublists
      // consisting of the first half and second half of the list.
      // This assumes lists start at index 0.
      var left = List[Int]()
      var right = List[Int]()
      var idx = 0
      for (elt <- list) {
        if (idx < list.length / 2) {
          left = left :+ elt
        }
        else
        {
          right = right :+ elt
        }
        idx += 1
      }
      // Recursively sort both sublists.
      left = mergesort(left)
      right = mergesort(right)
      // Then merge the now-sorted sublists.
      merge(left,right)
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

  var idx = 0
  val testResults = (lists zip results).map (pair => mergesort(pair._1) == pair._2)
  if (testResults.forall(identity)) {
    println("Al results OK ")
  }
  else
  {
    println("Some results fail")
  }
}
