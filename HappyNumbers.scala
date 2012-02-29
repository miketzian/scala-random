object HappyNumbers {
	def main(args: Array[String]) {
		var hList = List[Int]()
		var t = System.nanoTime()
		for(i <- 0.until(1000)) {
			if(isHappy(i)) {
				hList = hList.::(i)
			}
		}
		println("Happy Numbers: ")
		println(hList.reverse)
	}
	def isHappy(num: Int):Boolean = {
			happyMatches(List(num))
	}
	def happyMatches(x: List[Int]):Boolean = {
		reduceNumber(x.apply(0)) match {
			case 1 => true
			case value:Int if(x.contains(value)) => false
			case value:Int => happyMatches(x.::(value))
			case _ => false
		}
	}
	def reduceNumber(number: Int):Int = {
		number.toString().toCharArray().foldLeft(0)(foldOp);
	}
	def sq(j: Int):Int = { 
		j * j
	}
	def foldOp(i: Int, c: Char):Int = {
		i + sq(Integer.parseInt(c.toString()))
	}
}