object RangeChallenge {
  def sortFunc(a:Int, b:Int):Boolean = {
    a < b
  }
  def main(args: Array[String]) {
    val input = Seq(4, 1, 4, 2, 5, 8, 2, 6)
    println(input)
    val sorted = input.sortWith(sortFunc)
    println(sorted)
    val out = toSet2(sorted)
    println(out)
    val out2 = toSet3(sorted)
    println(out)
  }
  
  //matching on the list
  def toSet2(list: Seq[Int]):Seq[(Int,Int)] = {
	  def traverse(list: Seq[Int])(set: Seq[(Int,Int)]): Seq[(Int,Int)] = list match {
	    case hd :: tail if(set.size==0) => {
	      //size was 0, so add the first element
	    	traverse(tail)(set++Some((hd,hd)))
	    }
	    case hd :: tail if(set.last._2 == list.head) => {
	      //skip the element, it already exists...
	      traverse(tail)(set)
	    }
	    case hd :: tail => { 
	      //otherwise
	      set.last match {
	        //if check the last value if it should be incremented
	        case (_,y) if(y == hd-1) => {
	          traverse(list.tail)(set.init++Some((set.last._1,hd)))
	        }
	        //or just add a new one
	        case _ => {
	          traverse(list.tail)(set++Some((hd,hd)))
	        }
	      }
	    }
	    case Nil => set
	  }
	  traverse(list)(Seq[(Int,Int)]())
  }
  
  //matching on the set
  def toSet3(list: Seq[Int]):Seq[(Int,Int)] = {
	  def traverse(list: Seq[Int])(set: Seq[(Int,Int)]): Seq[(Int,Int)] = set match {
	    case x if(list.length == 0) => {
	      x
	    }
	    case x if(x.size == 0) => {
	      traverse(list.tail)(set++Some((list.head,list.head)))
	    }
	    case _ => {
	      set.last match {
	          case (_,y) if(y == list.head) => {
	        	  traverse(list.tail)(set)
	          }
		      case (x,y) if(y == list.head-1) => {
		    	  traverse(list.tail)(set.init++Some((x,list.head)))
		      }
		      case _ => {
		        traverse(list.tail)(set++Some((list.head,list.head))) 
		      }
	      }
	    }
	  }
	  traverse(list)(Seq[(Int,Int)]())
  }
}