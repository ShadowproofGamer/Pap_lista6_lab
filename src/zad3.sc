import scala.annotation.tailrec

//lista heterogeniczna
sealed trait AB[+T1,+T2]
case class P[+T1, +T2](t:String, e:T1) extends AB[T1,T2]
case class S[+T1, +T2](e:String) extends AB[T1,T2]

//drzewo binarne
sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Element(value: Int) extends BT[Int]
case class Node[+A](elem: '.', left: BT[A], right: BT[A]) extends BT[A]

/*
                  .
                /   \
               .     3
             /   \
            .     .
          /   \    \
         2     1    4
*/
val t = Node('.', Node('.', Node('.', Element(2), Element(1)), Node('.', Empty, Element(4))), Element(3))

def treeToList[A, B](input:BT[A]):List[AB[A, B]] = {
  @tailrec
  def treeToList_rec[A, B](tree:BT[A], queue:List[AB[A,B]|BT[A]], res:List[AB[A,B]]):List[AB[A, B]]={
    queue match
      case _ if tree==Node =>{
        tree match
          case Node('.', Element(x), Empty) => treeToList_rec(Empty, queue, (S("Node element") :: P("Element", x) :: res))
          case Node('.', Empty, Element(x)) => treeToList_rec(Empty, queue, S("Node element") :: P("Element", x) :: res)
          case Node('.', Element(x), Element(y)) => treeToList_rec(Empty, queue, S("Node element element") :: P("Element", y) :: P("Element", x) :: res)
          case Node('.', Node('.', x, y), Element(z)) => treeToList_rec(Node('.', x, y), P("Element", z) :: S("Node left element") :: queue, res)
          case Node('.', Element(z), Node('.', x, y)) => treeToList_rec(Node('.', x, y), P("Element", z) :: S("Node element right") :: queue, res)
          case Node('.', Node('.', x, y), Node('.', z, w)) => treeToList_rec(Node('.', x, y), Node('.', z, w) :: queue, res)
      }
      case List() if tree==Empty => res.reverse
      case List(Node('.', x, y),_) if x.isInstanceOf[BT[A]] => treeToList_rec(Node('.', x, y), queue.tail, res)
      case List(x, _) if tree==Empty => treeToList_rec(Empty, queue.tail, x::res)
  }
  treeToList_rec(input, List(), List())
}

treeToList(t);

/*
case Node('.', Node('.', x, y), Element(z)) => treeToList_rec(Node('.', x, y), P("Element", z) :: S("Node left element") :: queue, res)
Node(. . .),      [P("Element", 3) :: S("Node left element")]       []

case Node('.', Node('.', x, y), Node('.', z, w)) => treeToList_rec(Node('.', x, y), Node('.', z, w) :: queue, res)
Node(. 2 1),      [Node(. empty 4), P("Element", 3), S("Node left element")],     []

case Node('.', Element(x), Element(y)) => treeToList_rec(Empty, queue, S("Node element element") :: P("Element", y) :: P("Element", x) :: res)
(),      [Node(. empty 4), P("Element", 3), S("Node left element")],      [S("Node element element"),  P("Element", 1),  P("Element", 2)]

case List(Node('.', x:BT[A], y:BT[A]),_) => treeToList_rec(Node('.', x, y), queue.tail, res)
Node(. empty 4),      [P("Element", 3), S("Node left element")],      [S("Node element element"),  P("Element", 1),  P("Element", 2)]

case Node('.', Empty, Element(x)) => treeToList_rec(Empty, queue, S("Node element") :: P("Element", x) :: res)
(),      [P("Element", 3), S("Node left element")],      [S("Node element"),  P("Element", 4), S("Node element element"),  P("Element", 1),  P("Element", 2)]

case List(x:AB[A, B], _) if tree==Empty => treeToList_rec(Empty, queue.tail, x::res)
(),      [S("Node left element")],      [P("Element", 3), S("Node element"),  P("Element", 4), S("Node element element"),  P("Element", 1),  P("Element", 2)]

case List(x:AB[A, B], _) if tree==Empty => treeToList_rec(Empty, queue.tail, x::res)
(),      [],      [S("Node left element"), P("Element", 3), S("Node element"),  P("Element", 4), S("Node element element"),  P("Element", 1),  P("Element", 2)]

case List() if tree==Empty => res.reverse
[S("Node left element"), P("Element", 3), S("Node element"),  P("Element", 4), S("Node element element"),  P("Element", 1),  P("Element", 2)].reverse


*/