
mod Test01 {

    use Text.HtmlDoc.{Html};
    use Text.HtmlDoc.{html, text, toString};


    def doc1(): Html = html(Nil, text("blank document"))



    @test
    def test01(): String = 
        doc1() |> toString

    @test
    def test02(): String = 
        use Text.HtmlDoc.{tr, colspan_};
        tr(colspan_(2) :: Nil, text("...")) |> toString

    @test
    def test03(): Unit \ IO = 
        use Text.HtmlDoc.{br, empty};
        br(Nil, empty()) |> toString |> println

    @test
    def test04(): Unit \ IO = 
        use Text.HtmlDoc.{br0};
        br0(Nil) |> toString |> println


    @test
    def test05(): Unit \ IO = 
        use Text.HtmlDoc.{<&>, link0};
        (link0(Nil) <&> link0(Nil)) |> toString |> println


    @test
    def test06(): Unit \ IO = 
        use Text.HtmlDoc.{<&&>, link0};
        (link0(Nil) <&&> link0(Nil)) |> toString |> println

    @test
    def test07(): Unit \ IO =
        use Text.HtmlDoc.{anchor, HtmlAttr, text};
        anchor(List#{HtmlAttr.HtmlAttr("href", "https://example.com")}, text("Click me!")) |> toString |> println

    @test
    def test08(): Unit \ IO =
        use Text.HtmlDoc.{anchor, HtmlAttr, text};
        anchor(List#{HtmlAttr.HtmlAttrBoolean("download")}, text("Click me!")) |> toString |> println

    @test
    def test09(): Unit \ IO =
        use Text.HtmlDoc.{anchor, HtmlAttr, text};
        anchor(List#{HtmlAttr.HtmlAttr("href", "https://example.com"), HtmlAttr.HtmlAttrBoolean("download")}, text("Click me!")) |> toString |> println

    @test
    def test10(): Unit \ IO =
        use Text.HtmlDoc.{anchor, HtmlAttr, text};
        anchor(List#{HtmlAttr.HtmlAttrBoolean("download"), HtmlAttr.HtmlAttr("href", "https://example.com")}, text("Click me!")) |> toString |> println
}
