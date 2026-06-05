effect ndet
  ctl choose(options : list<a>) : a
  ctl guard(cond : bool) : ()

fun find-matching(list1 : list<int>, list2 : list<int>) : ndet int
  val choice1 = choose(list1)
  val choice2 = choose(list2)
  guard(choice1 == choice2)
  choice1

fun all-results(action : () -> ndet a) : list<a>
  with handler
    ctl choose(options)
      options.flatmap fn(x) { resume(x) }
    ctl guard(cond)
      if cond then resume(()) else []
  [action()]

fun first-result(action : () -> ndet a) : maybe<a>
  with handler
    ctl choose(options)
      options.foreach-while fn(x)
        resume(x)
    ctl guard(cond)
      if cond then resume(()) else Nothing
  Just(action())

fun main()
  val all = all-results {
    find-matching([1,2,3,4,5], [3,5,7])
  }
  println("All results: " ++ all.show)

  val first = first-result {
    find-matching([1,2,3,4,5], [3,5,7])
  }
  println("First result: " ++ first.show)
