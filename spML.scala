import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
val rawData = sc.textFile("")
val raRatings = rawData.map(_.split("\t").take(3))
import org.apache.spark.mllib.recommendation.Rating
val ratings = rawRatings.map { case Array(user, movie, rating) =>Rating(user.toInt, movie.toInt, rating.toDouble) }
import org.apache.spark.mllib.recommendation.ALS
val model = ALS.train(ratings, 50, 10, 0.01)
val movies = sc.textFile("")
val titles = movies.movies.map(line => line.split(",").take(2)).map(array =>(array(0).toInt,array(1))).collectAsMap()

val moviesForUser = ratings.keyBy(_.user).lookup(100)
moviesForUser.sortBy(-_.rating).take(10).map(rating => (titles(rating.product), rating.rating)).foreach(println)

val topKRecs = model.recommendProducts(789,10)
topKRecs.map(rating => (titles(rating.product), rating.rating)).foreach(println)

val actualRating = moviesForUser.take(1)(0)
val predictedRating = model.predict(789,actualRating.product)
val squaredError = math.pow(predictedRating - actualRating.rating, 2.0)
val usersProducts = ratings.map{ case Rating(user, product, rating) => (user, product)}
val predictions = model.predict(usersProducts).map{ case Rating(user, product, rating) => ((user, product), rating)}
val ratingsAndPredictions = ratings.map{ case Rating(user, product, rating) => ((user, product), rating) }.join(predictions)
val MSE = ratingsAndPredictions.map{ case ((user, product), (actual, predicted)) => math.pow((actual - predicted), 2) }.reduce(_ + _) / ratingsAndPredictions.count
println("Mean Squared Error = "+MSE)