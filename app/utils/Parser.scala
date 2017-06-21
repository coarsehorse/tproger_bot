package utils

import scala.util.{Try, Success, Failure}
import scala.collection.JavaConverters._
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import java.net.URLDecoder

object Parser {
  /**
    * Parse trproger.ru for specific tag
    * @param tag tag text e.g. 'java'
    */
  def searchFor(tag: String): List[(String, List[String])] = {
    val url = "https://tproger.ru/tag/" + tag + "/"
    var rootSearchPage = new Document("stub")

    Try({
      Jsoup.connect(url)
        .userAgent("Mozilla")
        .maxBodySize(4096000)
        .get()
    }) match {
      case Success(doc) =>
        rootSearchPage = doc
      case Failure(f) =>
        println(f)
      // Actions for aborting process
    }

    // Find links to articles
    val moretag = rootSearchPage.getElementsByClass("moretag").asScala
    val hrefs = moretag map (_.attr("href"))

    def makeListHrefTags(hrefs_n: List[String]): List[(String, List[String])] = {
      hrefs_n match {
        case Nil => List()
        case x :: xs =>
          (x, getTagsByURL(x)) :: makeListHrefTags(xs)
      }
    }

    makeListHrefTags(hrefs.toList)
  }

  /**
    * Get hashtags from article URL
    * @param article_url search here for tags, article url
    * @return list of tags
    */
  def getTagsByURL(article_url: String): List[String] = {
    var article = new Document("stub")
    Try({
      Jsoup.connect(article_url)
        .userAgent("Mozilla")
        .maxBodySize(4096000)
        .get()
    }) match {
      case Success(doc) =>
        article = doc
      case Failure(f) =>
        println(f)
      // Actions for aborting process
    }

    val tag_regex = """(https://tproger.ru/tag/)(.+)/""".r

    val blocksWithLinks = article.select("[rel='tag']").asScala
    val originalLinks = blocksWithLinks.map(_.attr("href")).toList // Cyrillic symbols encoded. It looks ugly
    val linksWithTags = originalLinks map { link => // Cyrillic symbols decoded. Looks much better
      val found = tag_regex.findAllIn(link)
      val n = found.next
      val encoded = found.group(2)
      val decoded = URLDecoder.decode(encoded, "UTF-8")

      found.group(1) + decoded + "/"
    }

    def getTags(links: List[String]): List[String] = links match {
      case Nil => List()
      case x :: xs =>
        val found = tag_regex.findAllIn(x)
        val n = found.next

        found.group(2) :: getTags(xs)
    }

    getTags(linksWithTags)
  }
}