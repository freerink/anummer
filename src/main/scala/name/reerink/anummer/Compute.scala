package name.reerink.anummer

import java.io._
import java.util.concurrent.CountDownLatch
import akka.actor.{ Actor, PoisonPill, ScalaActorRef }
import Actor._
import akka.routing.{ Routing, CyclicIterator }
import Routing._

sealed trait AnummerMessage
case class Calculate() extends AnummerMessage
case class Work(start: Long, end: Long) extends AnummerMessage
case class Done(found: Long) extends AnummerMessage
case class Result(anummers: List[Long]) extends AnummerMessage

class Worker extends Actor {
	def receive = {
		case Work(start, end) =>
			println("Work received start=" + start + ", end=" + end)
			//val anummers = List(start, end)
			self reply Done(calculateAnummers(self, start, end))
	}

	def calculateAnummers(me: ScalaActorRef, start: Long, end: Long): Long = {
		var a: Long = start
		var found = 0
		var anummers: List[Long] = List()
		while (a < end) {
			val anr = new Anummer(a)
			if (anr.isValid) {
				found += 1
				anummers = anummers ::: List(a)
				if (found % 1000 == 0) {
					//println("Found (" + found + ") A-nummers, current: " + a)
					me reply Result(anummers)
					anummers = List()
				}
			}
			a += anr.incr
		}
		me reply Result(anummers)
		found
	}
}

object RemoteWorkerServer {
	def run = {
		remote.start("localhost", 2552)
		remote.register("anummer-computer", actorOf[Worker])
	}
	
	def main(args: Array[String]) = run
}

object Hoi {
	def run = {
		println(remote.address.getHostName())
	}
	
	def main(args: Array[String]) = run
}

class Master(nrOfWorkers: Int, nrOfMessages: Int, start: Long,
	end: Long, latch: CountDownLatch) extends Actor {

	var nrOfResults: Int = 0

	var totalFound = 0

	val workers = Vector.fill(nrOfWorkers)(actorOf[Worker].start())

	val router = Routing.loadBalancerActor(CyclicIterator(workers)).start()

	val writer: PrintWriter = new PrintWriter(new BufferedWriter(new FileWriter("anummers.txt")));

	def write(xs: List[Long], writer: PrintWriter): List[Long] = xs match {
		case Nil => xs
		case x :: xs1 => {
			writer.println(x.toString())
			write(xs1, writer)
		}
	}

	def receive = {

		case Done(found) =>
			println("Done found: " + found)
			nrOfResults += 1
			if (nrOfResults == nrOfMessages) self.stop()

		case Result(anummers) =>
			totalFound += anummers.size
			print("\rTotal found: " + totalFound)
			write(anummers, writer)

		case Calculate() =>
			println("Master received Calculate")

			val incr = (end - start) / nrOfMessages

			var part = start
			for (i <- 0 until nrOfMessages) {
				router ! Work(part, part + incr)
				part += incr
			}

			router ! Broadcast(PoisonPill)

			router ! PoisonPill

	}

	override def postStop() {
		writer.close()
		latch.countDown()
	}
}

object Compute {

	def main(args: Array[String]): Unit = {
		println("Compute A-nummers")

		val latch = new CountDownLatch(1)
		val master = actorOf(new Master(Runtime.getRuntime().availableProcessors(), 10, 1000000000L, 9999999999L, latch)).start()

		// start the calculation
		master ! Calculate()

		// wait for master to shut down
		latch.await()

		return

		val anummers: List[Long] = List(100000000, 1000000000, 1010101024, 1010101025, 1010101026,
			1010000000, 1010100021, 1010101010, 1010101098, 1010101099, 1010101100, 1010991234)
		for (anummer <- anummers) {
			val a = new Anummer(anummer)
			println("Anummer (" + a + ") valid?=" + a.isValid + ", next = " + a.incr + " -> " + (anummer + a.incr))
		}

		val writer: PrintWriter = new PrintWriter(new BufferedWriter(new FileWriter("anummers.txt")));

		var found = 0
		var a: Long = 1000000000
		while (a < 9999999999L) {
			val anr = new Anummer(a)
			if (anr.isValid) {
				if (found % 1000 == 0) {
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
