package recommendation

import scala.util.matching.Regex
import scala.collection.mutable.Map

object WordCounter {

  def main(args: Array[String]): Unit = {
    val w = new WordCounter
    println(w.cleanAndCount("<h>Hello World</h>"))
  }
  

}

class WordCounter {
  
  type WordCount = Map[String, Int]
  
  def cleanAndCount(text: String) : WordCount = {
    return countWords(splitWords(removeHtml(text)))
  }
  
  def removeHtml(text: String) : String = {
    val tag_regex = """<[^>]+>""".r
    val tags_removed = tag_regex.replaceAllIn(text, "")
    return tags_removed
  }
  
  def splitWords(text: String) : List[String] = {
    val non_alpha = """[^A-Z^a-z]+""".r
    val lowercase_text = text.toLowerCase()
    val splitWords = non_alpha.split(lowercase_text).toList
    return splitWords
  }
  
  def countWords(wordlist: List[String]) : WordCount = {
    val word_count = Map[String, Int]()
    
    for (word <- wordlist) {
      if (!word_count.contains(word))
        word_count += (word -> 0)
      word_count(word) = word_count(word) + 1
    }
    
    return word_count
  }
}