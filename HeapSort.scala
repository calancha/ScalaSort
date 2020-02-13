// Implement heap algorithm
// The number of elements of the output must equal the input

object HeapSort extends App {

  // *** Steps ***
  // 1. Call the buildMaxHeap() function on the list. Also referred to as heapify(), this builds a heap from a list in O(n) operations.
  // 2. Swap the first element of the list with the final element. Decrease the considered range of the list by one.
  // 3. Call the siftDown() function on the list to sift the new first element to its appropriate index in the heap.
  // 4. Go to step (2) unless the considered range of the list is one element.
  //
  // We are using here built-in PriorityQueue, thus the heap adjust itself automatically
  // after we call heap.drop.
  // TODO: rewrite using a custom heap structure.
  def heapSort(list: List[Int], acc: List[Int]): List[Int] = {
    var result = acc
    var clist = list
    var heap = scala.collection.mutable.PriorityQueue.empty(Ordering[Int].reverse)
    for (elt <- list) { heap += elt }
    while (clist.nonEmpty) {
      var min = heap.head
      var clist_head = clist.head
      if (min == clist_head) {
        clist = clist.tail
      }
      else {
        var min_idx = clist.indexOf(min)
        clist = clist.slice(1, min_idx) ::: (clist.head :: clist.slice(min_idx + 1, clist.length))
      }
      result = result :+ min
      heap = heap.drop(1)
    }
    result
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

  val testResults = (lists zip results).map (pair => heapSort(pair._1, List[Int]()) == pair._2)
  if (testResults.forall(identity)) {
    println("Al results OK ")
  }
  else
  {
    println("Some results fail")
  }

  // println(heapSort(List(9,0,7,2,5,9,4,2,0,3,2,2,3,7,9,1,5,5,3,9), List[Int]()))
}
