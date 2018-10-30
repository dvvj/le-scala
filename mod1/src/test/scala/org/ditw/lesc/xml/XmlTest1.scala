package org.ditw.lesc.xml

import com.lucidchart.open.xtract.{XmlReader, __}
import com.lucidchart.open.xtract.XmlReader._
import cats.syntax.all._

import scala.xml.XML

object XmlTest1 extends App {

  val testStr =
    """<blog type="technical">
      |    <head>
      |        <title>Introducing Xtract</title>
      |        <subtitle>A new xml deserialization library for scala</subtitle>
      |        <author first="Thayne"
      |                last="McCombs"
      |                department="engineering"
      |                canContact="false" />
      |    </head>
      |    <body>
      |        <!-- first section is intro, so no title -->
      |        <section>
      |            <p>This is the introduction to the blog.</p>
      |        </section>
      |        <section title="Section 1">
      |            <p>First Paragraph</p>
      |            <p>Second Paragraph</p>
      |            <p>Third Paragraph</p>
      |        </section>
      |        <ignoredtag>
      |            This isn't in an expected tag, so it is ignored.
      |        </ignoredtag>
      |    </body>
      |</blog>
    """.stripMargin

  object BlogType extends Enumeration {
    val tech = Value("technical")
    val marketing = Value("marketing")
    val product = Value("product")
    val business = Value("business")
  }

  object Departments extends Enumeration {
    val engineering = Value("engineering")
    val sales = Value("sales")
    val marketing = Value("marketing")
    val exec = Value("executive")
    val support = Value("support")
  }

  case class AuthorInfo(
                         name: String,
                         email: Option[String],
                         department: Departments.Value,
                         canContact: Boolean
                       )

  object AuthorInfo {
    def validateEmail(email:String):Boolean = email contains("@")
    private val nameReader:XmlReader[String] = {
      for {
        first <- attribute[String]("first")
        last <- attribute[String]("last")
      } yield {
        first + " " + last
      }
    }
    implicit val reader:XmlReader[AuthorInfo] = (
      nameReader,
      attribute[String]("email").filter(s => validateEmail(s)).optional,
      attribute("department")(enum(Departments)),
      attribute[Boolean]("canContact")
    ).mapN(apply)
  }
  case class Section(title: Option[String], paragraphs: Seq[String])
  case class Content(sections: Seq[Section])
  case class Blog(
                   title: String,
                   subtitle: Option[String],
                   author: AuthorInfo,
                   blogType: BlogType.Value,
                   content: Content
                 )

  object Content {
    implicit val reader:XmlReader[Content] = (
      (__ \ "section").read(seq[Section].map(apply))
    )
  }

  object Section {
    implicit val reader:XmlReader[Section] = (
      attribute[String]("title").optional,
      (__ \ "p").read(seq[String])
    ).mapN(apply)
  }

  object Blog {
    implicit val reader:XmlReader[Blog] = (
      (__ \ "head" \ "title").read[String],
      (__ \ "head" \ "subtitle").read[String].optional,
      (__ \ "head" \ "author").read[AuthorInfo],
      attribute("type")(enum(BlogType)).default(BlogType.tech),
      (__ \ "body").read[Content]
    ).mapN(apply)
  }

  val xml = XML.loadString(testStr)
  val parsed = XmlReader.of[Blog].read(xml)
  println(parsed)
}
