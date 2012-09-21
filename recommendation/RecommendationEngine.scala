package recommendation
import scala.collection.mutable.Map
import scala.Math.sqrt
import scala.Math.pow
import del.icio.us.Delicious

object RecommendationEngine {
  
  type PrefsType = Map[String, Map[String, Double]]
  
  private val critics = Map(
     "Lisa Rose"-> 
  		Map("Lady in the Water"-> 2.5, "Snakes on a Plane"-> 3.5, "Just My Luck"-> 3.0, "Superman Returns"-> 3.5, "You, Me and Dupree"-> 2.5, "The Night Listener"-> 3.0), 
  	"Gene Seymour"-> 
  		Map("Lady in the Water"-> 3.0, "Snakes on a Plane"-> 3.5, "Just My Luck"-> 1.5, "Superman Returns"-> 5.0, "The Night Listener"-> 3.0, "You, Me and Dupree"-> 3.5), 
  	"Michael Phillips"-> 
  		Map("Lady in the Water"-> 2.5, "Snakes on a Plane"-> 3.0, "Superman Returns"-> 3.5, "The Night Listener"-> 4.0), "Claudia Pulg"-> Map("Snakes on a Plane"-> 3.5, "Just My Luck"-> 3.0, "The Night Listener"-> 4.5, "Superman Returns"-> 4.0, "You, Me and Dupree"-> 2.5), "Mick LaSalle"-> Map("Lady in the Water"-> 3.0, "Snakes on a Plane"-> 4.0, "Just My Luck"-> 2.0, "Superman Returns"-> 3.0, "The Night Listener"-> 3.0, "You, Me and Dupree"-> 2.0), "Jack Matthews"-> Map("Lady in the Water"-> 3.0, "Snakes on a Plane"-> 4.0, "The Night Listener"-> 3.0, "Superman Returns"-> 5.0, "You, Me and Dupree"-> 3.5), 
  	"Toby"-> 
  		Map("Snakes on a Plane"->4.5,"You, Me and Dupree"->1.0,"Superman Returns"->4.0)
  )
  
  def sum(args: Iterator[Double]) = args.reduceLeft(_+_)
  
  def sum(args: Iterable[Double]) = args.reduceLeft(_+_)
  
  def sim_pearson(prefs: PrefsType, p1: String, p2: String) : Double =
  {
    
    val si = for (item1 <- prefs(p1); item2 <- prefs(p2) if item1._1 == item2._1)
    yield item1._1
    
    // Find the number of common elements
    val n = si.size
    
    // If no ratings in common return 0
    if (n == 0)
      return 0
      
    // Add up all preferences
    val sum2 = sum(for ( it <- si) yield (prefs(p2)(it)))
    val sum1 = sum(for ( it <- si) yield (prefs(p1)(it)))
    
    // Add up all squares
    val sum1sq = sum(for (it <- si) yield (pow(prefs(p1)(it), 2)))
    val sum2sq = sum(for (it <- si) yield (pow(prefs(p2)(it), 2)))
    
    // Sum of all products
    
    val pSum = sum(for (it <- si) yield (prefs(p1)(it) * prefs(p2)(it)))
    
    val num = pSum-(sum1*sum2/n)

    val den = sqrt((sum1sq-pow(sum1,2)/n)*(sum2sq-pow(sum2,2)/n))
    
    if (den == 0)
      return 0
    val r = num/den
    return r
  }
  
  def transformPrefs(prefs: PrefsType) : PrefsType =
  {

    val result = Map[String, Map[String, Double]]()
   
    
    for ((person, items) <- prefs){
      for ((item, score) <- items) {
        if (!result.contains(item)) {
          result.put(item, Map[String, Double]())
        }
        result(item).put(person, score)
      }
    }
        
    return result
  }
  
  
  def topMatches (prefs: PrefsType, person: String, n: Int, similarity: (PrefsType, String, String) => Double) : List[Tuple2[Double, String]] =
  {
    val scores = for (other <- prefs.keysIterator if other != person) yield (similarity(prefs, person, other), other)
    val scoresList = scores.toList.sort(_._1 > _._1)
    
    return scoresList.slice(0,n)
  }
  
  def main(args: Array[String]): Unit = {

    val movies = transformPrefs(critics)
    for ((movie,score) <- topMatches(movies, "Superman Returns", n=5, similarity = sim_pearson))
      println(movie, score)

  }
  
}

class RecommendationEngine {
  val name = ""
}

