
// Advanced Scala Tutorial

// Higher-order Functions
def applyFunction(x: Int, f: Int => Int): Int = f(x)

val square = (x: Int) => x * x
println(s"Square of 5: ${applyFunction(5, square)}")

// For-Comprehensions
val numbers = List(1, 2, 3, 4)
val result = for {
  n <- numbers
  if n % 2 == 0
} yield n * 2

println(s"Even numbers doubled: $result")

// Concurrency (Future)
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

val futureResult = Future {
  Thread.sleep(1000)
  "Hello from Future!"
}

futureResult.onComplete {
  case scala.util.Success(value) => println(value)
  case scala.util.Failure(exception) => println(s"An error occurred: $exception")
}

// Tail Recursion
def factorial(n: Int): Int = {
  @annotation.tailrec
  def loop(n: Int, acc: Int): Int = {
    if (n <= 0) acc
    else loop(n - 1, n * acc)
  }
  loop(n, 1)
}

println(s"Factorial of 5: ${factorial(5)}")
