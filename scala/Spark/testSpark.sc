import org.apache.spark.{SparkContext, SparkConf}
val a = 2
val conf = new SparkConf().setMaster("local").setAppName("spark-play")
val sc = new SparkContext(conf)
