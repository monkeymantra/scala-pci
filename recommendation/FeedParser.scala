package recommendation
import com.sun.syndication.io._
import com.sun.syndication.feed.synd._
import java.net.URL
import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer
import scala.collection.JavaConversions._
import scala.io.Source

object FeedParser {
  
  
  def main(args: Array[String]): Unit = {
    val parser = new FeedParser
    
    val feeds = parser.getEntryForUrl("http://hbl.fi/rss.xml")
    
    for (entry <- feeds) println(entry.getDescription())
    
    val multiFeeds = parser.getEntriesFromFile("/Users/grice/Documents/workspace/ScalaTest/src/recommendation/feedlist.txt")
    multiFeeds.foreach(x => println(x.toList))
  }
}

class FeedParser {
  
  val testUrls = List("http://hbl.fi/rss.xml")
  
  type UrlFeeds = Map[String, Iterator[SyndEntryImpl]]
  
  def getEntriesFromFile(file: String) : Iterator[Iterator[SyndEntryImpl]] = {
    
    try {
      val feedList = Source.fromFile(file).getLines()
      return feedList.map(x => getEntryForUrl(x))
    }
    catch {
      case e => { throw new RuntimeException(e) }
    }

  }
  
  def getEntryForUrl(url: String) : Iterator[SyndEntryImpl] = {
    
    try {
      val sfi = new SyndFeedInput();
      
      val feed = sfi.build(new XmlReader(new URL(url)))
        
      val entries = feed.getEntries()
      val entriesIterator = entries.iterator()
      
      val result = for (entry <- entriesIterator) yield entry.asInstanceOf[SyndEntryImpl]
      
      return result
    }
    
    catch {
      case e => return Iterator[SyndEntryImpl]()
    }
    
  }
  
  def getEntriesForUrls(urls: List[String]) : UrlFeeds =  
  {
    
    val results = Map[String, Iterator[SyndEntryImpl]]()
    
    try {
      val sfi = new SyndFeedInput();
    
      urls.foreach(url => {
        val feed = sfi.build(new XmlReader(new URL(url)))
        
        val entries = feed.getEntries()
        val entriesIterator = entries.iterator()
      
        val entryResults = for (entry <- entriesIterator)
          yield entry.asInstanceOf[SyndEntryImpl];
        
        results += (url -> entryResults)
      })
    }
    catch {
      case e => throw new RuntimeException(e)
    }
    return results
  }
  
}