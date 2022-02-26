class User(val name: String, val id: Int) {

  def printUser: IO[Unit] = IO {
    println(name)
    println(id)
  }

  def withID(newID: Int): User = new User(name, newID)

}

/*   val initialFn: (Int => List[Int]) = x => List(x, x + 5, x + 10)

  val myComputation: ListArrow[Int, String] = new ListArrow(initialFn)
    .compose(x => List(x, x * 10, x * 100))
    .map(_.toString)

  //this is List("0", "0", "0", "5", "50", "500", "10", "100", "1000")
  //dom.window.alert(myComputation(0).toString)

  val myOtherComp = myComputation.contraMap((x: Double) => x.floor.toInt)
  //this will also be List(0, 0, 0, 5, 50, 500, 10, 100, 1000)
  //dom.window.alert(myOtherComp(0.999).toString)*/

class ListArrow[-A, +B](val arrow: (A => List[B])) {

  def printUser(user: User): IO[Unit] = IO {
    println(user.name)
    println(user.id)
  }

  def apply(a: A): List[B] = arrow(a)

  def compose[C](next_arrow: B => List[C]): ListArrow[A, C] = {
    new ListArrow(a => arrow(a).flatMap(next_arrow))
  }

  def map[C](f: B => C): ListArrow[A, C] = {
    new ListArrow(a => arrow(a).map(f))
  }

  def flatMap[C]: ((B => List[C]) => ListArrow[A, C]) = compose

  def contraMap[PreA](g: PreA => A): ListArrow[PreA, B] = {
    new ListArrow(preA => arrow(g(preA)))
  }

}
