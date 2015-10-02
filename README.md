# xml_proc
scala based xml streaming processor

example usage:
<pre>
<code>
  // must make reader implicit
  val file = new File("example.html")
  implicit val reader = new XMLEventReader(Source.fromFile(file))
  
  // start at top of html file
  until("html") {
    // processing head block
    case (end @ "head", _, _) => until(end) {
      case ("title", title, _) => println(s"title is $title")
      case ("style", style, _) => println(s"a style block: $style");
    }
    
    // processing "body" sibling
    case (end @ "body") => until(end) {
      case (end @ "table", _, attrs) =>
        // grab attribute data from table before proceeding
        println(s"""table height is ${attrs \ "height"}""")
        
        // finish parsing through nested table elements
        until(end) {
          // process each row
          case (end @ "tr", _, _) => until(end) {
            // process each data element of row
            case ("td", data, _) => println(s"table data is $data")
          }
        }
    }
  }
</code>
</pre>
