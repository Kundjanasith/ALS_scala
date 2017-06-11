import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import scala.collection.mutable.ListBuffer

// val conf = new SparkConf().setAppName("Movie Rating")
// val sc = new SparkContext(conf)
def print_readable( arr: ListBuffer[Array[Double]] ){
    print("[")
    for( i <- 0 to arr.length - 1 ){
        print("(")
        for( j <- 0 to arr.apply(0).length - 1 ){
            print( arr.apply(i).apply(j) + ":" )
        }
        print(")")
    }
    println("]")
}
val filename = "data/rat.csv"
val rawData = sc.textFile(filename)
var rawRatings = rawData.map(_.split(",").take(3))
val users = new ListBuffer[Int]()
val movies = new ListBuffer[Int]()


case class Rating( uid: Int, mid: Int, rat: Double ){
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
val ratingList = new ListBuffer[Rating]()
case class RecommendRating( uid: Int, mid: Int, rat: Double ){
    ratingList += new Rating(uid,mid,rat)
}
// var rat = new ListBuffer[Rating]()
val rawRating = rawRatings.mapPartitionsWithIndex { (idx, iter) => if (idx == 0) iter.drop(1) else iter }
val ratings = rawRating.map { case Array(user, movie,rating) =>  RecommendRating( user.toInt, movie.toInt, rating.toDouble ) }


