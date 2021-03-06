use Data.RoseTree;
use Data.RoseTree.{RoseTree};
use Text/HtmlDoc.{Html};
use Text/HtmlDoc.{writeHtml, concat, html, head, meta, title, body, paragraph, text, link, div, toString, 
    lang_, charset_, class_, rel_, href_, type_, media_};
use Text/HtmlDoc.{<&>};
use Text/HtmlDoc/Tree.{LabelPrinter, collapsibleTree2};

def main(): Result[Unit, System.Error] & Impure = 
    use Result.{flatMap};
    use Data/RoseTree.{drawTree};
    let tree1 = toRoseTree(flixBasicDb());
    Console.printLine(drawTree(tree1)); 
    let htmltree = paragraph(Nil, collapsibleTree2(nodePrinter(), leafPrinter(), tree1));
    let doc = makeDoc(htmltree);
    writeHtml(160, "E://coding/flix/flix-htmldoc/output/dirtree.html", Text/Charset.utf_8(), doc)


enum FileObject {
    case Directory(String, List[FileObject])
    case File(String)
}    

def flixBasicDb(): FileObject = 
    Directory("flix-basicdb", 
          Directory("src", 
               Directory("Internal", 
                       File("PreparedStatement.flix")
                    :: File("Prim.flix")
                    :: File("ResultSet.flix")
                    :: Nil)
            :: File("DbMonad.flix")
            :: File("PreparedStatementSetter.flix")
            :: File("RowEvaluator.flix")
            :: Nil)
        :: Directory("test", 
               File("Test01.flix")
            :: Nil)
        :: File("LICENSE")
        :: File("README.md")
        :: Nil)

def toRoseTree(x: FileObject): RoseTree[String] = toRoseTreeHelper(x, k -> k)

def toRoseTreeHelper(x: FileObject, k: RoseTree[String] -> RoseTree[String]): RoseTree[String] = match x { 
    case File(name) => k(RoseTree(name, Nil))
    case Directory(name, kids) => toRoseTreeListHelper(kids, vs -> k(RoseTree(name, vs)))
}

def toRoseTreeListHelper(xs: List[FileObject], k: List[RoseTree[String]] -> RoseTree[String]): RoseTree[String] = match xs { 
    case Nil => k(Nil)
    case x :: rs => 
        toRoseTreeHelper(x, 
            v1 -> toRoseTreeListHelper(rs,  
            vs -> k(v1 :: vs)
            ))
}            


def makeDoc(content: Html): Html = 
    html(lang_("en") :: Nil, 
            head(Nil, 
                    meta(charset_("utf-8") :: Nil)
                <&> title(Nil, text("Tree Demo"))
                <&> link(rel_("stylesheet") :: href_("tree.css") :: type_("text/css") :: media_("all") :: Nil)
            )
        <&> body(Nil, content)
    )

def leafPrinter(): LabelPrinter[String] = 
    { makeAttrs = _ -> class_("treeleaf") :: Nil
    , printNode = s -> div(class_("box") :: Nil, div(Nil, text(s)))
    }

def nodePrinter(): LabelPrinter[String] = 
    { makeAttrs = _ -> Nil
    , printNode = s -> div(class_("box") :: Nil, div(Nil, text(s)))
    }