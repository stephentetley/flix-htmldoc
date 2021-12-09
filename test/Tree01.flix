use Data.{RoseTree, RoseForest};
use Data.RoseTree.{Node};


use Text/HtmlDoc.{Html};
use Text/HtmlDoc.{writeHtml, html, head, meta, title, body, text, toString, lang_, charset_};
use Text/HtmlDoc.{<&>};

def leaf(s: String): RoseTree[String] = Node(s, Nil)
def node(s: String, kids: RoseForest[String]): RoseTree[String] = Node(s, kids)

def tree01(): RoseTree[String] = 
    node("root", leaf("item1") :: node("item2", leaf("item2.1") :: leaf("item2.2") :: Nil) :: Nil)

def makeDoc(content: Html): Html = 
    html(lang_("en") :: Nil, 
            head(Nil, 
                    meta(charset_("utf-8") :: Nil)
                <&> title(Nil, text("Tree Demo"))
            )
        <&> body(Nil, content)
    )

def main(): Result[Unit, System.Error] & Impure =
    let printer = { makeAttrs= _ -> Nil, printNode = s -> text(s) };
    let tree = Text/HtmlDoc/Tree.collapsibleTree(printer,  tree01());
    let doc = makeDoc(tree);
    writeHtml(160, "e:/coding/flix/flix-htmldoc/output/Tree.html", Text/Charset.utf_8(), doc)
    
