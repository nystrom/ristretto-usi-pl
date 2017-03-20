package ristretto.main

////////////////////////////////////////////////////////////////
// Compiler error handling
////////////////////////////////////////////////////////////////
trait Errors {

  import org.bitbucket.inkytonik.kiama.util.Positions
  import ristretto.frontend.RistrettoSyntax.ASTNode

  object Posns extends Positions

  private var errorCount = 0

  type Position = Object

  def error(t: ASTNode, s: String) = {
    Posns.getStart(t) match {
      case Some(p) =>
        println(p.format + " " + s)
      case None =>
        println(s)
    }
    errorCount += 1
  }

  def error(s: String) = {
    println(s)
    errorCount += 1
  }

  def hasErrors = errorCount > 0
}
