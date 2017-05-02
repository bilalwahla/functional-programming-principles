import lecture3.Rational

val half = new Rational(1, 2)

def error(msg: String) = throw new Error(msg)

//error("test")

val x = null
val y: String = x

//val z: Int = null // only reference types and not the value types

if (true) 1 else false