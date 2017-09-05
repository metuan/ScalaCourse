package Polynomials

class Polynomials(val terms0: Map[Int, Double]) {

 def this(bindings: (Int, Double)*) = this(bindings.toMap)

 val terms: Map[Int, Double] = terms0 withDefaultValue 0.0

 def + (other: Polynomials) = new Polynomials((other.terms foldLeft terms)(addTerm))

 def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
   val (exp, coeff) = term
   terms + (exp -> (coeff + terms(exp)))
 }

 override def toString: String =
   (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff+"x^"+exp) mkString " + "
}
