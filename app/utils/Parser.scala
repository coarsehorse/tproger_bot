import scala.util.{Try, Success, Failure}
import scala.collection.JavaConverters._
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import java.net.URLDecoder

object Parser {
  // Parse trproger for specified tag
  def searchFor(tag: String): List[(String, List[String])] = {
    // Request
    val url = "https://tproger.ru/tag/" + tag + "/"

    var rootSearchPage = new Document("stub")

    Try({
      Jsoup.connect(url)
        .userAgent("Mozilla")
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

  // Get hashtags from article URL
  def getTagsByURL(article_url: String): List[String] = {
    val article = Jsoup.connect(article_url)
      .userAgent("Mozilla")
      .get()

    val tagsClass = article.getElementsByClass("entry-meta clearfix").asScala
    val ul = tagsClass(0).getElementsByTag("ul").asScala
    val li = ul(0).getElementsByTag("li").asScala
    val a = li(0).getElementsByTag("a").asScala

    def normalizeURL(link: String): String = {
      val regex = """(https://tproger.ru/tag/)(.+)/""".r
      val found = regex.findAllIn(link)
      val n = found.next
      val encoded = found.group(2)
      val decoded = URLDecoder.decode(encoded, "UTF-8")

      found.group(1) + decoded + "/"
    }

    val linksWithTags = a.map(_.attr("href")).toList.map(lnk =>
      normalizeURL(lnk))

    def getTags(links: List[String]): List[String] = {
      def tag(link: String): String = {
        val tag_reg = """https://tproger.ru/tag/([^/]+)/""".r
        val found = tag_reg.findAllIn(link)
        val n = found.next

        found.group(1)
      }

      links match {
        case Nil => List()
        case x :: xs =>
          tag(x) :: getTags(xs)
      }
    }

    val tags = getTags(linksWithTags)

    tags
  }
}