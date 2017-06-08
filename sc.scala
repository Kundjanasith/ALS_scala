import scala.io.Source
import scala.collection.mutable.ListBuffer

val filename = "data/rat.csv"
val users = new ListBuffer[Int]()
val movies = new ListBuffer[Int]()
// Define rating
class Rating( uid: Int, mid: Int, rat: Double ){
    var user_id: Int = uid
    var movie_id: Int = mid
    var rate: Double = rat
    users += uid
    movies += mid
    def getUserId(): Int = user_id
    def getMovieId(): Int = movie_id
    def getRate(): Double = rate
    override def toString: String = s" user_id: $user_id , movie_id: $movie_id, rate: $rate"  
}
// Read file assign rating
var ratings = new ListBuffer[Rating]()
var ratingArr = ratings.to[Array]
var numLine: Int = 0
for (line <- Source.fromFile(filename).getLines()) {
  numLine += 1
  if(numLine>1) {
    val uid = line.split(",")(0).toInt 
    val mid = line.split(",")(1).toInt
    val rat = line.split(",")(2).toDouble
    var temp = new Rating(uid,mid,rat)
    ratings += temp
  }
}
// check Pivot
var ratingArr = ratings.to[Array]
def createPivotQ( uid: Int, mid: Int ): Double = {
  var res: Double = 0.0 
  for( r <- ratingArr ){
    if(r.getUserId()==uid&&r.getMovieId()==mid){
      res = r.getRate()
    }
  }
  return res
}
def createPivotW( uid: Int, mid: Int ): Double = {
  var res: Double = 0.0 
  for( r <- ratingArr ){
    if(r.getUserId()==uid&&r.getMovieId()==mid){
      res = 1.0
    }
  }
  return res
}
// Parameter
val lambda_ = 0.1
val n_factors = 100
val m = ( users.to[Array].reduceLeft(_ max _) - users.to[Array].reduceLeft(_ min _) ) + 1
val n = ( movies.to[Array].reduceLeft(_ max _) - movies.to[Array].reduceLeft(_ min _) ) + 1
val n_iterations = 20
// Matrix X
var matrixX = new ListBuffer[ListBuffer[Double]]()
for( i <- 1 to m ){
  var lineArr = new ListBuffer[Double]()
  for( j <- 1 to n_factors ){
    lineArr += ( Math.random * 5 )
  }
  matrixX += lineArr
}
// Matrix Y
var matrixY = new ListBuffer[ListBuffer[Double]]()
for( i <- 1 to n_factors ){
  var lineArr = new ListBuffer[Double]()
  for( j <- 1 to n ){
    lineArr += ( Math.random * 5 )
  }
  matrixY += lineArr
}
// Get error 
