package name.reerink.anummer

import java.io._

object Compute {

	class Complex(real: Double, imaginary: Double) {
		def re = real
		def im = imaginary
		override def toString() = re + (if (im < 0) "" else "+") + im + "i]"
	}

	def timer(count: Long, delayMs: Long, callback: (Long) => Unit) {
		var i: Long = count
		while (i > 0) {
			callback(i)
			i -= 1
			Thread.sleep(delayMs)
		}
	}

	def timeFlies() {
		println("Tick")
	}

	def main(args: Array[String]): Unit = {
		println("Compute A-nummers")
		//timer(5, 1, (i) => println("Ready for takeoff: " + i + ", " + new Complex(i, 5 - i).toString()))

		//val env: Env = { case "x" => 3 }
		//val env2: Env = { case "x" => 6 }
		//val exp: Tree = Sum(Var("x"), Const(5))

		//println("exp " + exp + " for env: " + env2 + " = " + eval(exp, env2))
		println("range(1,10)=" + Range(1, 10))

		val a1 = new Anummer(100000000)
		println("Anummer (" + a1 + ") valid?=" + a1.isValid)
		val a2 = new Anummer(1000000000)
		println("Anummer (" + a2 + ") valid?=" + a2.isValid)
		val a3 = new Anummer(1010101024)
		println("Anummer (" + a3 + ") valid?=" + a3.isValid)
		val a4 = new Anummer(1010101025)
		println("Anummer (" + a4 + ") valid?=" + a4.isValid)
		val a5 = new Anummer(1010101026)
		println("Anummer (" + a5 + ") valid?=" + a5.isValid)

		val writer: PrintWriter = new PrintWriter(new BufferedWriter(new FileWriter("anummers.txt")));

		var found = 0
		var a: Long = 1000000000
		while (a <= 9999999999L) {
			val anr = new Anummer(a)
			if (anr.isValid) {
				if (found % 1000 == 0) {
					println("Found (" + found + ") A-nummers, current: " + anr)
				}
				writer.println(anr.toString())
				found += 1
			}
			a += 1
		}
		writer.close()
	}

	abstract class Tree
	case class Sum(l: Tree, r: Tree) extends Tree
	case class Var(v: String) extends Tree
	case class Const(c: Int) extends Tree

	type Env = String => Int

	def eval(t: Tree, e: Env): Int = t match {
		case Sum(l, r) => eval(l, e) + eval(r, e)
		case Var(x) => e(x)
		case Const(c) => c
	}
}
