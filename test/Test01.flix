
mod Test01 {

    use Text.HtmlDoc.{Html};
    use Text.HtmlDoc.{html, text, toString};
    use Assert.{assertEq};


    def doc1(): Html = html(Nil, text("blank document"))



    @Test
    def test01(): Unit \ Assert =
        assertEq(expected = "<html>blank document</html>", doc1() |> toString)

    @Test
    def test02(): Unit \ Assert =
        use Text.HtmlDoc.{tr, colspan_};
        assertEq(expected = "<tr colspan=\"2\">...</tr>", tr(colspan_(2) :: Nil, text("...")) |> toString)

    @Test
    def test03(): Unit \ Assert =
        use Text.HtmlDoc.{br, empty};
        assertEq(expected = "<br></br>", br(Nil, empty()) |> toString)

    @Test
    def test04(): Unit \ Assert =
        use Text.HtmlDoc.{br0};
        assertEq(expected = "<br />", br0(Nil) |> toString)


    @Test
    def test05(): Unit \ Assert =
        use Text.HtmlDoc.{<&>, link0};
        assertEq(expected = "<link /><link />", (link0(Nil) <&> link0(Nil)) |> toString)


    @Test
    def test06(): Unit \ Assert =
        use Text.HtmlDoc.{<&&>, link0};
        assertEq(expected = "<link /> <link />", (link0(Nil) <&&> link0(Nil)) |> toString)

    @Test
    def test07(): Unit \ Assert =
        use Text.HtmlDoc.{anchor, HtmlAttr, text};
        assertEq(expected = "<a href=\"https://example.com\">Click me!</a>", anchor(List#{HtmlAttr.HtmlAttr("href", "https://example.com")}, text("Click me!")) |> toString)

    @Test
    def test08(): Unit \ Assert =
        use Text.HtmlDoc.{anchor, HtmlAttr, text};
        assertEq(expected = "<a download>Click me!</a>", anchor(List#{HtmlAttr.HtmlAttrBoolean("download")}, text("Click me!")) |> toString)

    @Test
    def test09(): Unit \ Assert =
        use Text.HtmlDoc.{anchor, HtmlAttr, text};
        assertEq(expected = "<a href=\"https://example.com\" download>Click me!</a>", anchor(List#{HtmlAttr.HtmlAttr("href", "https://example.com"), HtmlAttr.HtmlAttrBoolean("download")}, text("Click me!")) |> toString)

    @Test
    def test10(): Unit \ Assert =
        use Text.HtmlDoc.{anchor, HtmlAttr, text};
        assertEq(expected = "<a download href=\"https://example.com\">Click me!</a>", anchor(List#{HtmlAttr.HtmlAttrBoolean("download"), HtmlAttr.HtmlAttr("href", "https://example.com")}, text("Click me!")) |> toString)
}
