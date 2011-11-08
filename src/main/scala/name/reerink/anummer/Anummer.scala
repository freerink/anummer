package name.reerink.anummer;

class Anummer(v: Long) {
	def value = v

	var incr: Long = 1

	def isValid: Boolean = {
		incr = 1
		// strValue bevat a0 ... a9
		val strValue = value.toString()
		/* 
		 * a0 > 0 en lengte 10 betekent strValue.length == 10 omdat we een Long doorgeven waarbij er 
		 * geen voorloop nullen mogelijk zijn
		 */
		if (strValue.length != 10) {
			return false
		}
		// opeenvolgende cijfers mogen niet gelijk aan elkaar zijn
		var a: Int = strValue(0) - '0'
		var sum: Long = a
		var multiplier = 1
		var sum2: Long = a
		for (i <- Range(1, 10)) {
			val b: Int = strValue(i) - '0'
			if (a == b) {
				if (a != 9 && b != 9) {
					// Bouw de volgende waarde door het eerste stuk te pakken van het origineel en de rest op 0 te zetten
					val nextStrValue = strValue.substring(0, i) + (b + 1) + "0000000000".substring(i + 1, 10)
					incr = nextStrValue.toLong - value
				}
				// println("nextValue = " + nextStrValue + ", incr = " + incr)
				return false
			}
			sum += b
			multiplier = (multiplier * 2)
			sum2 += (multiplier * b)
			// Shift
			a = b
		}
		//println("Sum = " + sum + " , sum2 = " + sum2)
		if (sum % 11 != 0 || sum2 % 11 != 0) {
			return false
		}
		true
	}

	override def toString() = { "" + value }

}
