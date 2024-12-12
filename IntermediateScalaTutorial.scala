
// Intermediate Scala Tutorial

// Classes and Objects
class Person(val name: String, val age: Int) {
  def greet(): String = s"Hello, my name is $name and I am $age years old."
}

object PersonApp {
  def main(args: Array[String]): Unit = {
    val person = new Person("Bob", 30)
    println(person.greet())
  }
}

// Collections (List, Map, and Set)
val numbers = List(1, 2, 3, 4, 5)
println(s"Numbers List: $numbers")

val capitals = Map("USA" -> "Washington", "UK" -> "London", "India" -> "Delhi")
println(s"Capitals: $capitals")

val fruits = Set("Apple", "Banana", "Orange")
println(s"Fruits Set: $fruits")

// Pattern Matching
val day = 3
val dayName = day match {
  case 1 => "Monday"
  case 2 => "Tuesday"
  case 3 => "Wednesday"
  case 4 => "Thursday"
  case 5 => "Friday"
  case _ => "Weekend"
}

println(s"Day $day is $dayName")
