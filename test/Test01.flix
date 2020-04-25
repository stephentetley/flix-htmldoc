use Text/HtmlDoc.{Html};
use Text/HtmlDoc.{html, text, toString};


def doc1(): Html = html(Nil, text("blank document"))



@test
def test01(): String = 
    doc1() |> toString