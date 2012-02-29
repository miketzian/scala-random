object HappyNumbers {
	def main(args: Array[String]) {
		var hList = List[Int]()
		for(i <- 0.until(100)) {
		  val happy = HappyNumberUtil.isHappy(i);
		  println(i + " " + happy)
		  if(happy) {
			  hList = hList.::(i)
		  }
		}
		println("Happy Numbers: ")
		println(hList)
	}
}
object HappyNumberUtil {
  
  def isHappy(num: Int):Boolean = {
    happyMatches(List(num))
  }
  
  def happyMatches(x: List[Int]):Boolean = {
    val lastNum = x.apply(0)
    val lastRed:Option[Int] = reduceNumber(lastNum)
    lastRed match {
      case Some(1) => true
      case Some(value) =>  {
        if(x.contains(value)) { 
          System.out.print(x)
          System.out.println(" .. Contained : " + value)
          false
        } else { 
	        value match { 
	          case 1 => true
	          case _ => {
	        	  //x.:: will add value ot the start of the list
	        	  happyMatches(x.::(value))
	          }
	        }
        }
      }
      case _ => false
    }
  }
  
  def reduceNumber(number: Int):Option[Int] = {
	val arr:Array[Char] = String.valueOf(number).toCharArray();
	return Some(arr.foldLeft(0)(foldOp))
  }
  
  def foldOp(i: Int, c: Char):Int = {
     val b = Integer.parseInt(String.valueOf(c))
     i + (b * b)
  }

}