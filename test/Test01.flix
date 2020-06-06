use Text/HtmlDoc.{Html};
use Text/HtmlDoc.{html, text, toString};


def doc1(): Html = html(Nil, text("blank document"))



@test
def test01(): String = 
    doc1() |> toString

@test
def test02(): String = 
    use Text/HtmlDoc.{tr, colspan_};
    tr(colspan_("2") :: Nil, text("...")) |> toString


def runTests(): Unit & Impure =
    test01() |> Console.printLine;
    test02() |> Console.printLine


