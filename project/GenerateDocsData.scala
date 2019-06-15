package scala.build

trait SettingsDescriptorModel {
  case class Section(category: String, description: String, options: List[ScalacOption])
  case class ScalacOption(
    option: String,
    schema: Schema,
    description: String,
    abbreviations: Seq[String] = Seq.empty,
    deprecated: Option[String] = None,
    note: Option[String] = None
  )
  case class Schema(
    `type`: String,
    arg: Option[String] = None,
    multiple: Option[Boolean] = None,
    default: Option[Any] = None,
    choices: Seq[Choice] = Seq.empty,
    min: Option[Any] = None,
    max: Option[Any] = None
  )
  case class Choice(choice: String, description: Option[String] = None, deprecated: Option[String] = None)

  object Fixup {
    private[this] val quoted  = """`([^`']+)'""".r
    private[this] val htmlTag = """<([^>]+)>""".r

    def markdownifyBackquote(s: String): String = quoted.replaceAllIn(s, "`$1`")
    def dehtmlfy(s: String): String = htmlTag.replaceAllIn(s, "$1")
  }
}
trait Emitter { _: SettingsDescriptorModel =>
  private var indent = 0
  def indented(body: => Unit): Unit = { indent += 1; body; indent -= 1 }
  implicit class SB(val sc: StringContext) {
    def sb(args: Any*)(implicit sb: StringBuilder): Unit = {
      import StringContext.{checkLengths, processEscapes}
      checkLengths(args, sc.parts)
      val pi = sc.parts.iterator
      val ai = args.iterator
      sb.append(processEscapes(pi.next()))
      while (ai.hasNext) {
        sb.append(ai.next())
        sb.append(process(pi.next()))
      }
    }
  }
  implicit class Times(val n: Int) extends AnyVal {
    def times(body: => Unit): Unit = {
      def loop(val i: Int): Unit = if (i < n) { body ; loop(i + 1) }
      loop(0)
    }
  }
  def element(tag: String, value: String = "", head: Boolean = false)(implicit sb: StringBuilder): Unit = {
    indent.times(sb.append("  "))
    if (head) sb"- " else sb"  "
    if (tag.nonEmpty) sb"$tag:"
    if (value.nonEmpty) {
      if (tag.nonEmpty) sb" "
      sb""""$value""""
    }
    sb"\n"
  }
  def maybe[A](tag: String, value: Option[A])(implicit sb: StringBuilder): Unit = value.foreach(v => element(tag, v.toString))
  def maybes[A](tag: String, value: Seq[A], handlers: (A => Unit)*)(implicit sb: StringBuilder): Unit =
    if (value.nonEmpty) {
      element(tag)
      value.foreach(v => handlers.foreach(h => h(v)))
    }
  def emit(section: Section)(implicit sb: StringBuilder): Unit = {
    val Section(title, text, options) = section
    element("category", title, head = true)
    element("description", text)
    element("options")
    indented {
      options.foreach {
        case ScalacOption(option, schema, description, abbreviations, deprecated, note) =>
          element("option", option, head = true)
          element("schema")
          indented {
            import schema._
            element("type", `type`)
            maybe("arg", arg)
            maybe("multiple", multiple)
            maybe("default", default)
            maybes("choices", choices,
              (c: Choice) => indented(element("choice", c.choice, head = true)),
              (c: Choice) => indented(maybe("description", c.description)))
            maybe("min", min)
            maybe("max", max)
          }
          element("description", description)
          maybes("abbreviations", abbreviations, (x: String) => indented(element("", x, head = true)))
      }
    }
  }
}
/** Externalize a descriptor of ScalaSettings in YAML format.
 */
class SettingsDescriptor extends SettingsDescriptorModel with Emitter {
  import scala.tools.nsc.Settings
  val settings = new Settings(_ => ???)
  import settings._
  import Fixup._
  // Pasted from ./src/compiler/scala/tools/nsc/settings/AbsSettings.scala to avoid bootstrap error.
  // Extra categories are supported.
  implicit class UpgradedTests(s: Setting) {
    import s.{name, deprecationMessage}
    def isAdvanced_?   = name.startsWith("-X") && name != "-X"
    def isPrivate_?    = name.startsWith("-Y") && name != "-Y" && !isPreso_?
    def isVerbose_?    = name.startsWith("-V") && name != "-V"
    def isWarning_?    = name match {
                           case "-W" | "-Werror" => false
                           case "-Xlint" => true
                           case _  => name.startsWith("-W")
                         }
    def isPreso_?      = name.startsWith("-Ypresentation")
    def isStandard_?   = !isAdvanced_? && !isPrivate_? && !isWarning_? && !isVerbose_? && !isPreso_?
    def isDeprecated_? = deprecationMessage.isDefined
  }
  val sections: List[(String, Setting => Boolean, String)] = List(
    ("Standard Settings", _.isStandard_?,
      "A set of standard options that are supported on the current development environment and will be supported in future releases."),
    //("JVM Settings", _.isWarning_?, ""),
    //("Plugin Settings", _.isWarning_?, ""),
    ("Advanced Settings", _.isAdvanced_?, ""),
    ("Verbose Settings", _.isVerbose_?, ""),
    ("Private Settings", _.isPrivate_?, ""),
    ("Warning Settings", _.isWarning_?, ""),
    ("IDE-specific Settings", _.isPreso_?, ""),
  )
  def mergeChoice(labels: Seq[String], descriptions: Seq[String]): Seq[Choice] =
    labels.zipAll(descriptions, "", "").map {
      case (choice, d) => Choice(
        choice,
        description = Option(d).map(markdownifyBackquote).map(dehtmlfy).filter(_.nonEmpty),
        // FIXME
        deprecated = Some("EXPLAIN_ALTERNATIVE").filter(_ => d.toLowerCase.contains("deprecated"))
      )
    }
  def schema(s: Setting): Schema = s match {
    case b: BooleanSetting       => Schema("Boolean")
    case i: IntSetting           => Schema("Int", default = Some(i.default), min = i.range.map(_._1), max = i.range.map(_._2))
    case c: ChoiceSetting        =>
      val choices = mergeChoice(c.choices, c.choicesHelp)
      Schema("Choice", arg = Some(c.helpArg).map(dehtmlfy), default = Option(c.default), choices = choices)
    case mc: MultiChoiceSetting[_] =>
      val choices = mergeChoice(mc.choices, mc.descriptions)
      Schema("Choice", multiple = Some(true), arg = Some(mc.helpArg).map(dehtmlfy), choices = choices)
    case ps: PhasesSetting       => Schema("Phases", default = Option(ps.default))
    case px: PrefixSetting       => Schema("Prefix")
    case sv: ScalaVersionSetting => Schema("ScalaVersion", arg = Some(sv.arg).map(dehtmlfy), default = Some(sv.initial.unparse))
    case pathStr: PathSetting    => Schema("Path", arg = Some(pathStr.arg), default = Some(pathStr.default))
    case str: StringSetting      => Schema("String", arg = Some(str.arg).map(dehtmlfy), default = Some(str.default))
    case ms: MultiStringSetting  => Schema("String", multiple = Some(true), arg = Some(ms.arg).map(dehtmlfy))
  }
  def option(s: Setting): ScalacOption =
    ScalacOption(
      option = s.name,
      schema = schema(s),
      description = dehtmlfy(markdownifyBackquote(s.helpDescription)),
      abbreviations = s.abbreviations,
      //TODO
      deprecated = Some("EXPLAIN_ALTERNATIVE").filter(_ => s.helpDescription.toLowerCase.contains("deprecated"))
    )
  def descriptor: String = {
    val grouped = sections.map {
      case (title, predicate, text) =>
        val options = visibleSettings.filter(predicate).map(option).toList.sortBy(_.option)
        Section(title, text, options)
    }
    implicit val sb = new StringBuilder
    grouped.foreach(emit)
    sb.toString()
  }
}

object GenerateDocsData {
  import java.io.File
  import java.nio.charset.StandardCharsets.UTF_8

  // output a descriptor of compiler options for docs.scala-lang/_data
  def run(outDir: File) {
    val file = new File(outDir, "compiler-options.yml")
    val res = new SettingsDescriptor().descriptor
    sbt.IO.write(file, res, UTF_8, false)
  }
}
