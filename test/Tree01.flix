use Data.{RoseTree, RoseForest};
use Data.RoseTree.{RoseTree};
use Data/RoseTree.{Forest};

use Text/HtmlDoc.{Html};
use Text/HtmlDoc.{html, head, meta, title, body, text, toString, lang_, charset_};
use Text/HtmlDoc.{<&>};

def leaf(s: String): RoseTree[String] = RoseTree(s, Nil)
def node(s: String, kids: RoseForest[String]): RoseTree[String] = RoseTree(s, kids)

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

def main(): Unit & Impure =
    let printer = { makeAttrs= _ -> Nil, printNode = s -> text(s) };
    let tree = Text/HtmlDoc/Tree.collapsibleTree(printer,  tree01());
    toString(makeDoc(tree)) |> Console.printLine
    
