
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

}
