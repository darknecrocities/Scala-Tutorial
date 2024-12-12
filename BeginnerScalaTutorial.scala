
// Beginner Scala Tutorial

// Variables and Data Types
var name: String = "Scala Beginner"
val age: Int = 25

// Print statements
println(s"Hello, my name is $name and I am $age years old.")

// Conditionals
val result = if (age >= 18) "Adult" else "Minor"
println(s"You are an $result")

// Loops
println("Counting from 1 to 5:")
for (i <- 1 to 5) {
  println(i)
}

// Functions
def greet(name: String): String = {
  s"Hello, $name! Welcome to Scala."
}

println(greet("Alice"))
