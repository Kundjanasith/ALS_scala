import scala.io.Source
import scala.collection.mutable.ListBuffer

val filename = "data/rat.csv"
var ratings = new ListBuffer[Rating]()
class Rating( uid: Int, mid: Int, rat: Double ){
    var user_id: Int = uid
    var movie_id: Int = mid
    var rate: Double = rat
    def getUserId(): Int = uid
    def getMovieId(): Int = uid
    def getRate( uid: Int, mid: Int ){
        var get_rate: Double = 0
    }
    override def toString: String =
        s" user_id: $user_id , movie_id: $movie_id, rate: $rate"
    
}

var numLine: Int = 0
for (line <- Source.fromFile(filename).getLines()) {
  numLine += 1
  if(numLine>1) {
    val uid = line.split(",")(0).toInt
    val mid = line.split(",")(1).toInt
    val rat = line.split(",")(2).toDouble
    var temp = new Rating(uid,mid,rat)
    println(temp.getUserId())
    ratings += temp
    
  }
}
val ratingList = ratings.toList
println(ratingList)
