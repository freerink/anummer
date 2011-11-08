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

		val anummers: List[Long] = List(100000000, 1000000000, 1010101024, 1010101025, 1010101026,
			1010000000, 1010100021, 1010101010, 1010101098, 1010101099, 1010101100, 1010991234)
		for (anummer <- anummers) {
			val a = new Anummer(anummer)
			println("Anummer (" + a + ") valid?=" + a.isValid + ", next = " + a.incr + " -> " + (anummer + a.incr))
		}

		//return

		val writer: PrintWriter = new PrintWriter(new BufferedWriter(new FileWriter("anummers.txt")));

		var found = 0
		var a: Long = 1000000000
		while (a < 9999999999L) {
			val anr = new Anummer(a)
			if (anr.isValid) {
				if (found % 1000 == 0 ) {
					println("Found (" + found + ") A-nummers, current: " + anr)
				}
				found += 1
				writer.println(anr.toString())
			} 
			a += anr.incr
		}
		writer.close()
	}

}
