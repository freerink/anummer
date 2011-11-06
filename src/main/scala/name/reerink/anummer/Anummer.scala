package name.reerink.anummer;

class Anummer(value: Long) {
	def isValid: Boolean = {
		// strValue bevat a0 ... a9
		val strValue = value.toString()
		/* 
		 * a0 > 0 en lengte 10 betekent strValue.length == 10 omdat we een Long doorgeven waarbij er 
		 * geen voorloop nullen mogelijk zijn
		 */
		if (strValue.length != 10) {
			return false
		}
		// opeenvolgende cijfers zijn niet gelijk aan elkaar
		var a: Int = strValue(0) - '0'
		var sum:Long = a
		var multiplier = 1
		var sum2:Long = a
		for (i <- Range(1, 10)) {
			val b: Int = strValue(i) - '0'
			if (a == b) {
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
