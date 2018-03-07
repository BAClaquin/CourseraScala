package Week4

abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  def isZero = true

  def predecessor = throw new NoSuchElementException("Zero has no predecessor")

  def successor = new Succ(Zero)

  def +(that: Nat) = that

  def -(that: Nat) =
    if (that.isZero) Zero
    else throw new NoSuchElementException("Zero")
}

class Succ(n: Nat) extends Nat {
  def isZero = false

  def predecessor = n

  def successor = new Succ(n)

  def + (that: Nat) =
    if(that.isZero) this
    else new Succ(n.successor) + that.predecessor

  def -(that: Nat) =
    if(that.isZero) this
    else if(this.predecessor.isZero) Zero - that.predecessor
    else new Succ(n.predecessor) - that.predecessor
}