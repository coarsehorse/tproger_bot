import scala.slick.driver.PostgresDriver.simple._
import scala.util.{Try, Failure, Success}

object DB {
  class Tag_db(tag: Tag) extends Table[(Int, String)](tag, "tag") {
    def tag_id = column[Int]("tag_id")
    def tag_text = column[String]("tag_text")
    def * = (tag_id, tag_text)
  }

  class Article_db(tag: Tag) extends Table[(Int, String)](tag, "article") {
    def article_id = column[Int]("article_id")
    def article_url = column[String]("article_url")
    def * = (article_id, article_url)
  }

  class TagArticle_db(tag: Tag) extends Table[(Int, Int)](tag, "tag_article") {
     def tag_id = column[Int]("tag_id")
     def article_id = column[Int]("article_id")
     def * = (tag_id, article_id)
  }

  // DRY
  val connectionUrl = "jdbc:postgresql://localhost:5432/tproger_bot?user=postgres&password=root"
  val tag = TableQuery[Tag_db]
  val article = TableQuery[Article_db]
  val tag_article = TableQuery[TagArticle_db]

  def addNewTagArticle(ins_tag: String, ins_url: String): Unit = {
    Database.forURL(connectionUrl, driver = "org.postgresql.Driver") withSession {
      implicit session =>
        // Error is normal - if tag is exists
        Try(tag.map(c => c.tag_text) += (ins_tag))
        val last_tag_q = tag filter (_.tag_text === ins_tag) map (_.tag_id)
        val last_tag_id = last_tag_q.list.head

        // Error is normal - if article is exists
        Try(article.map(c => c.article_url) += (ins_url))
        val last_article_q = article filter (_.article_url === ins_url) map (_.article_id)
        val last_article_id = last_article_q.list.head

        // Error is normal - if pair tag + article is exists
        Try(tag_article.map(c => (c.tag_id, c.article_id)) += (last_tag_id, last_article_id))
    }
  }

  def getArticlesByTag(u_tag: String): List[String] = {
    Database.forURL(connectionUrl, driver = "org.postgresql.Driver") withSession {
      implicit session =>
        val req = for {
          t <- tag if t.tag_text === u_tag
          t_a <- tag_article if t_a.tag_id === t.tag_id
          a <- article if a.article_id === t_a.article_id
        } yield (a.article_url)
        val urls = req.list

        urls
    }
  }
} 