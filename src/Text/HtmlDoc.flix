/*
 * Copyright 2020 Stephen Tetley
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

    
mod Text.HtmlDoc {
    
    use Text.PrettyPrint.{Doc, render, 
        hcat, besideSpace, beside, encloseSep, 
        dquotes, group, hang}
    use Text.PrettyPrint.{text => ppText, char => ppChar, space => ppSpace, empty => ppEmpty}
    use Text.PrettyPrint.{<>, <<>>, <&&>, <!>}

    use Text.HtmlDoc.HtmlElement
    use Text.HtmlDoc.HtmlElement.{
        HtmlText, HtmlTag, HtmlTagNoBody,HtmlComment
    }
    use Text.HtmlDoc.HtmlAttr
    use Text.HtmlDoc.HtmlAttr.{
        HtmlAttr, HtmlAttrBoolean
    }
    use Text.HtmlDoc.Html
    use Text.HtmlDoc.Html.{Html}

    /// Convention use X suffix to avoid nameclashes e.g. objectX


    /// TODO - probably shouldn't be exposed...
    /// Do we need a `Raw` element for `script` and `svg` content?
    pub enum HtmlElement {
        case HtmlText(String)
        case HtmlTag(String, List[HtmlAttr], Html)
        case HtmlTagNoBody(String, List[HtmlAttr])
        case HtmlComment(String)
    }

    pub enum HtmlAttr {
        case HtmlAttr(String, String)
        case HtmlAttrBoolean(String)
    }


    pub enum Html(List[HtmlElement] -> List[HtmlElement])


    /// Potentially we need a "no-pretty" version for speed.


    ///
    /// Render the Html document `doc` to a String with a fixed width of 120 characters.
    ///
    pub def toString(doc: Html): String = outputHtml(120, doc)

    pub def outputHtml(width: Int32, x: Html): String = 
        render(width, ppHtml(x, identity))

    ///
    /// Output the Html document `doc` to the file `path`.
    ///
    pub def writeHtml!(width: Int32, path: String, doc: Html): Result[String, Bool] \ IO = 
        use Result.flatMap;
        let str = render(width, ppDocType() <&&> ppHtml(doc, identity));
        Files.write(path, str)


    def ppHtml(x: Html, k: Doc -> Doc): Doc = 
        let Html(f) = x;
        ppHtmlElements(f(Nil), xs -> k(hcat(xs)))

    
    def ppHtmlElement(x: HtmlElement, k: Doc -> Doc): Doc = match x { 
        case HtmlText(s)                => k(ppText(s))
        case HtmlComment(s)             => k(ppText("<!--") <<>> ppText(s) <<>> ppText("-->"))
        case HtmlTag(name, attrs, body) => 
            ppHtml(body, v1 -> k(hang(2, ppOpeningTag(name, attrs) <> v1) <> ppClosingTag(name)))
        case HtmlTagNoBody(name, attrs) => k(ppOpeningTag(name, attrs))
    }

    def ppHtmlElements(xs: List[HtmlElement], k: List[Doc] -> Doc): Doc = match xs { 
        case Nil     => k(Nil)
        case x :: rs => ppHtmlElement(x, v1 -> ppHtmlElements(rs, vs -> k(v1 :: vs)))
    }

    def ppOpeningTag(name: String, attrs: List[HtmlAttr]): Doc = match attrs {
        case Nil => ppText("<" + name + ">")
        case _   => group(encloseSep(ppText("<" + name), ppChar('>'), ppEmpty(), List.map(ppAttr, attrs)))
    }

    def ppClosingTag(name: String): Doc = ppText("</" + name + ">")

    // TODO - check for double quotes in value, if present use single quotes... 
    def ppAttr (attr: HtmlAttr): Doc = match attr {
        case HtmlAttr(name, value)  => group(ppText(name) <> ppChar('=') <> dquotes(ppText(value)))
        case HtmlAttrBoolean(name)  => ppText(name)
    }

    def ppDocType(): Doc = ppText("<!DOCTYPE html>")

    ///
    /// Returns string `s` with all occurences of HTML special characters replaced.
    ///
    def escapeHtml(s: String): String =
        region rc {
            let sb = StringBuilder.new(rc);
            String.foldLeft((_, acc) -> escapeHtmlChar(sb, acc), (), s);
            StringBuilder.toString(sb)
        }

    def escapeHtmlChar(sb: StringBuilder[r], c: Char): Unit \ r = match c { 
        case '<'        => StringBuilder.appendString!("&lt;", sb)
        case '>'        => StringBuilder.appendString!("&gt;", sb)
        case '&'        => StringBuilder.appendString!("&amp;", sb)
        case '"'        => StringBuilder.appendString!("&quot;", sb)
        case '\u00A0'   => StringBuilder.appendString!("&nbsp;", sb)
        case _          => StringBuilder.append!(c, sb)
    }


    def makeHtml(x: HtmlElement): Html = 
        Html(xs -> x :: xs)

    pub def text(s: String): Html = makeHtml(HtmlText(escapeHtml(s)))

    pub def append(x: Html, y: Html): Html = match (x, y) {
        case (Html(k1), Html(k2)) => Html(xs -> k1(k2(xs)))
    }

    ///
    /// Concantenate two HTML fragements.
    ///
    pub def <&>(x: Html, y: Html): Html = append(x, y)

    ///
    /// Concantenate two HTML fragements with a space in between.
    ///
    pub def <&&>(x: Html, y: Html): Html = append(x, append(text(" "), y))

    pub def concat(xs: List[Html]): Html = match xs { 
        case Nil     => Html(ks -> ks)
        case x :: rs => concatHelper(rs, x)
    }

    pub def concatHelper(xs: List[Html], ac: Html): Html = match xs {
        case Nil     => ac
        case x :: rs => concatHelper(rs, ac <&> x)
    }


    pub def empty(): Html = 
        Html(identity)

    ///
    /// Tags
    /// See: https://developer.mozilla.org/en-US/docs/Web/HTML/Element
    ///

    pub def html(attrs: List[HtmlAttr], content: Html): Html        = makeHtml(HtmlTag("html", attrs, content))

    ///
    /// Document metadata
    ///

    pub def base(attrs: List[HtmlAttr], content: Html): Html        = makeHtml(HtmlTag("base", attrs, content))    
    pub def head(attrs: List[HtmlAttr], content: Html): Html        = makeHtml(HtmlTag("head", attrs, content))
    pub def link(attrs: List[HtmlAttr]): Html                       = makeHtml(HtmlTagNoBody("link", attrs))
    pub def meta(attrs: List[HtmlAttr]): Html                       = makeHtml(HtmlTagNoBody("meta", attrs)) 

    pub def style(attrs: List[HtmlAttr], content: String): Html = 
        let content1 = makeHtml(HtmlText(content));
        makeHtml(HtmlTag("style", attrs, content1))
    
    pub def title(attrs: List[HtmlAttr], content: Html): Html       = makeHtml(HtmlTag("title", attrs, content))

    ///
    /// Sectioning root
    ///
    pub def body(attrs: List[HtmlAttr], content: Html): Html        = makeHtml(HtmlTag("body", attrs, content))

    ///
    /// Content Sectioning
    ///

    pub def address(attrs: List[HtmlAttr], content: Html): Html     = makeHtml(HtmlTag("address", attrs, content))
    pub def article(attrs: List[HtmlAttr], content: Html): Html     = makeHtml(HtmlTag("article", attrs, content))
    pub def aside(attrs: List[HtmlAttr], content: Html): Html       = makeHtml(HtmlTag("aside", attrs, content))
    pub def footer(attrs: List[HtmlAttr], content: Html): Html      = makeHtml(HtmlTag("footer", attrs, content))
    pub def header(attrs: List[HtmlAttr], content: Html): Html      = makeHtml(HtmlTag("header", attrs, content))

    pub def h1(attrs: List[HtmlAttr], content: Html): Html          = makeHtml(HtmlTag("h1", attrs, content))
    pub def h2(attrs: List[HtmlAttr], content: Html): Html          = makeHtml(HtmlTag("h2", attrs, content))
    pub def h3(attrs: List[HtmlAttr], content: Html): Html          = makeHtml(HtmlTag("h3", attrs, content))
    pub def h4(attrs: List[HtmlAttr], content: Html): Html          = makeHtml(HtmlTag("h4", attrs, content))
    pub def h5(attrs: List[HtmlAttr], content: Html): Html          = makeHtml(HtmlTag("h5", attrs, content))
    pub def h6(attrs: List[HtmlAttr], content: Html): Html          = makeHtml(HtmlTag("h6", attrs, content))

    pub def hgroup(attrs: List[HtmlAttr], content: Html): Html      = makeHtml(HtmlTag("hgroup", attrs, content))

    pub def mainHtml(attrs: List[HtmlAttr], content: Html): Html    = makeHtml(HtmlTag("main", attrs, content))

    pub def nav(attrs: List[HtmlAttr], content: Html): Html         = makeHtml(HtmlTag("nav", attrs, content))
    pub def section(attrs: List[HtmlAttr], content: Html): Html     = makeHtml(HtmlTag("section", attrs, content))

    ///
    /// Text content
    ///
    pub def blockquote(attrs: List[HtmlAttr], content: Html): Html  = makeHtml(HtmlTag("blockquote", attrs, content))
    pub def dd(attrs: List[HtmlAttr], content: Html): Html          = makeHtml(HtmlTag("dd", attrs, content))
    pub def div(attrs: List[HtmlAttr], content: Html): Html         = makeHtml(HtmlTag("div", attrs, content))
    pub def dl(attrs: List[HtmlAttr], content: Html): Html          = makeHtml(HtmlTag("dl", attrs, content))
    pub def dt(attrs: List[HtmlAttr], content: Html): Html          = makeHtml(HtmlTag("dt", attrs, content))
    pub def figcaption(attrs: List[HtmlAttr], content: Html): Html  = makeHtml(HtmlTag("figcaption", attrs, content))
    pub def figure(attrs: List[HtmlAttr], content: Html): Html      = makeHtml(HtmlTag("figure", attrs, content))
    pub def hr(attrs: List[HtmlAttr], content: Html): Html          = makeHtml(HtmlTag("hr", attrs, content))
    pub def li(attrs: List[HtmlAttr], content: Html): Html          = makeHtml(HtmlTag("li", attrs, content))
    pub def ol(attrs: List[HtmlAttr], content: Html): Html          = makeHtml(HtmlTag("ol", attrs, content))
    pub def paragraph(attrs: List[HtmlAttr], content: Html): Html   = makeHtml(HtmlTag("p", attrs, content))
    pub def pre(attrs: List[HtmlAttr], content: Html): Html         = makeHtml(HtmlTag("pre", attrs, content))
    pub def ul(attrs: List[HtmlAttr], content: Html): Html          = makeHtml(HtmlTag("ul", attrs, content))

    ///
    /// Inline text semantics
    ///
    pub def anchor(attrs: List[HtmlAttr], content: Html): Html      = makeHtml(HtmlTag("a", attrs, content))
    pub def abbr(attrs: List[HtmlAttr], content: Html): Html        = makeHtml(HtmlTag("abbr", attrs, content))
    pub def bold(attrs: List[HtmlAttr], content: Html): Html        = makeHtml(HtmlTag("b", attrs, content))
    pub def bdi(attrs: List[HtmlAttr], content: Html): Html         = makeHtml(HtmlTag("bdo", attrs, content))
    pub def bdo(attrs: List[HtmlAttr], content: Html): Html         = makeHtml(HtmlTag("bdo", attrs, content))
    pub def br(attrs: List[HtmlAttr], content: Html): Html          = makeHtml(HtmlTag("br", attrs, content))
    pub def cite(attrs: List[HtmlAttr], content: Html): Html        = makeHtml(HtmlTag("cite", attrs, content))
    pub def code(attrs: List[HtmlAttr], content: Html): Html        = makeHtml(HtmlTag("code", attrs, content))
    pub def data(attrs: List[HtmlAttr], content: Html): Html        = makeHtml(HtmlTag("data", attrs, content))
    pub def dfn(attrs: List[HtmlAttr], content: Html): Html         = makeHtml(HtmlTag("dfn", attrs, content))
    pub def em(attrs: List[HtmlAttr], content: Html): Html          = makeHtml(HtmlTag("em", attrs, content))
    pub def italic(attrs: List[HtmlAttr], content: Html): Html      = makeHtml(HtmlTag("i", attrs, content))
    pub def kbd(attrs: List[HtmlAttr], content: Html): Html         = makeHtml(HtmlTag("kbd", attrs, content))
    pub def mark(attrs: List[HtmlAttr], content: Html): Html        = makeHtml(HtmlTag("mark", attrs, content))
    pub def quotation(attrs: List[HtmlAttr], content: Html): Html       = makeHtml(HtmlTag("q", attrs, content))
    pub def rb(attrs: List[HtmlAttr], content: Html): Html          = makeHtml(HtmlTag("rt", attrs, content))
    pub def rp(attrs: List[HtmlAttr], content: Html): Html          = makeHtml(HtmlTag("rp", attrs, content))
    pub def rt(attrs: List[HtmlAttr], content: Html): Html          = makeHtml(HtmlTag("rt", attrs, content))
    pub def rtc(attrs: List[HtmlAttr], content: Html): Html         = makeHtml(HtmlTag("rtc", attrs, content))
    pub def ruby(attrs: List[HtmlAttr], content: Html): Html        = makeHtml(HtmlTag("ruby", attrs, content))
    pub def strikethrough(attrs: List[HtmlAttr], content: Html): Html       = makeHtml(HtmlTag("s", attrs, content))
    pub def samp(attrs: List[HtmlAttr], content: Html): Html        = makeHtml(HtmlTag("samp", attrs, content))
    pub def small(attrs: List[HtmlAttr], content: Html): Html       = makeHtml(HtmlTag("small", attrs, content))
    pub def span(attrs: List[HtmlAttr], content: Html): Html        = makeHtml(HtmlTag("span", attrs, content))
    pub def strong(attrs: List[HtmlAttr], content: Html): Html      = makeHtml(HtmlTag("strong", attrs, content))
    pub def sub(attrs: List[HtmlAttr], content: Html): Html         = makeHtml(HtmlTag("sub", attrs, content))
    pub def sup(attrs: List[HtmlAttr], content: Html): Html         = makeHtml(HtmlTag("sup", attrs, content))
    pub def time(attrs: List[HtmlAttr], content: Html): Html        = makeHtml(HtmlTag("time", attrs, content))
    pub def unarticulated(attrs: List[HtmlAttr], content: Html): Html = makeHtml(HtmlTag("u", attrs, content))
    pub def var(attrs: List[HtmlAttr], content: Html): Html         = makeHtml(HtmlTag("var", attrs, content))
    pub def wbr(attrs: List[HtmlAttr], content: Html): Html         = makeHtml(HtmlTag("wbr", attrs, content))
    
    ///
    /// Image and multimedia
    ///
    
    pub def area(attrs: List[HtmlAttr], content: Html): Html        = makeHtml(HtmlTag("area", attrs, content))
    pub def audio(attrs: List[HtmlAttr], content: Html): Html       = makeHtml(HtmlTag("audio", attrs, content))
    pub def img(attrs: List[HtmlAttr], content: Html): Html         = makeHtml(HtmlTag("img", attrs, content))
    pub def map(attrs: List[HtmlAttr], content: Html): Html         = makeHtml(HtmlTag("map", attrs, content))
    
    pub def svg(attrs: List[HtmlAttr], body: String): Html = 
        let content = makeHtml(HtmlText(body));
        makeHtml(HtmlTag("svg", attrs, content))

    pub def track(attrs: List[HtmlAttr], content: Html): Html       = makeHtml(HtmlTag("track", attrs, content))
    pub def video(attrs: List[HtmlAttr], content: Html): Html       = makeHtml(HtmlTag("video", attrs, content))

    ///
    /// Embdedded content
    ///

    pub def embed(attrs: List[HtmlAttr], content: Html): Html       = makeHtml(HtmlTag("embed", attrs, content))
    pub def iframe(attrs: List[HtmlAttr], content: Html): Html      = makeHtml(HtmlTag("iframe", attrs, content))
    pub def objectX(attrs: List[HtmlAttr], content: Html): Html     = makeHtml(HtmlTag("object", attrs, content))
    pub def param(attrs: List[HtmlAttr], content: Html): Html       = makeHtml(HtmlTag("param", attrs, content))
    pub def picture(attrs: List[HtmlAttr], content: Html): Html     = makeHtml(HtmlTag("picture", attrs, content))
    pub def source(attrs: List[HtmlAttr], content: Html): Html      = makeHtml(HtmlTag("source", attrs, content))

    ///
    /// Scripting
    ///

    pub def canvas(attrs: List[HtmlAttr], fallback: String): Html = 
        let fallback1 = makeHtml(HtmlText(fallback));
        makeHtml(HtmlTag("script", attrs, fallback1))

    pub def script(attrs: List[HtmlAttr], content: String): Html = 
        let content1 = makeHtml(HtmlText(content));
        makeHtml(HtmlTag("script", attrs, content1))

    pub def noscript(attrs: List[HtmlAttr], content: Html): Html    = makeHtml(HtmlTag("noscript", attrs, content))
    

    ///
    /// Demarkating edits
    ///

    pub def del(attrs: List[HtmlAttr], content: Html): Html         = makeHtml(HtmlTag("del", attrs, content))
    pub def ins(attrs: List[HtmlAttr], content: Html): Html         = makeHtml(HtmlTag("ins", attrs, content))

    ///
    /// Table content
    ///
    pub def caption(attrs: List[HtmlAttr], content: Html): Html     = makeHtml(HtmlTag("caption", attrs, content))
    pub def col(attrs: List[HtmlAttr], content: Html): Html         = makeHtml(HtmlTag("col", attrs, content))
    pub def colgroup(attrs: List[HtmlAttr], content: Html): Html    = makeHtml(HtmlTag("colgroup", attrs, content))
    pub def table(attrs: List[HtmlAttr], content: Html): Html       = makeHtml(HtmlTag("table", attrs, content))
    pub def tbody(attrs: List[HtmlAttr], content: Html): Html       = makeHtml(HtmlTag("tbody", attrs, content))
        
    pub def td(attrs: List[HtmlAttr], content: Html): Html          = makeHtml(HtmlTag("td", attrs, content))
    pub def tfoot(attrs: List[HtmlAttr], content: Html): Html       = makeHtml(HtmlTag("tfoot", attrs, content))
    pub def th(attrs: List[HtmlAttr], content: Html): Html          = makeHtml(HtmlTag("th", attrs, content))
    pub def thead(attrs: List[HtmlAttr], content: Html): Html       = makeHtml(HtmlTag("thead", attrs, content))
    pub def tr(attrs: List[HtmlAttr], content: Html): Html          = makeHtml(HtmlTag("tr", attrs, content))
    

    ///
    /// Void elements
    ///

    pub def area0(attrs: List[HtmlAttr]): Html                      = makeHtml(HtmlTagNoBody("area /", attrs))
    pub def base0(attrs: List[HtmlAttr]): Html                      = makeHtml(HtmlTagNoBody("base /", attrs))
    pub def br0(attrs: List[HtmlAttr]): Html                        = makeHtml(HtmlTagNoBody("br /", attrs))
    pub def col0(attrs: List[HtmlAttr]): Html                       = makeHtml(HtmlTagNoBody("col /", attrs))
    pub def embed0(attrs: List[HtmlAttr]): Html                     = makeHtml(HtmlTagNoBody("embed /", attrs))
    pub def img0(attrs: List[HtmlAttr]): Html                       = makeHtml(HtmlTagNoBody("img /", attrs))
    pub def input0(attrs: List[HtmlAttr]): Html                     = makeHtml(HtmlTagNoBody("input /", attrs))
    pub def link0(attrs: List[HtmlAttr]): Html                      = makeHtml(HtmlTagNoBody("link /", attrs))
    pub def meta0(attrs: List[HtmlAttr]): Html                      = makeHtml(HtmlTagNoBody("meta /", attrs))
    pub def source0(attrs: List[HtmlAttr]): Html                    = makeHtml(HtmlTagNoBody("source /", attrs))
    pub def track0(attrs: List[HtmlAttr]): Html                     = makeHtml(HtmlTagNoBody("track /", attrs))
    pub def wbr0(attrs: List[HtmlAttr]): Html                       = makeHtml(HtmlTagNoBody("wbr /", attrs))

    ///
    /// Forms
    ///
    
    pub def button(attrs: List[HtmlAttr], content: Html): Html      = makeHtml(HtmlTag("button", attrs, content))
    pub def datalist(attrs: List[HtmlAttr], content: Html): Html    = makeHtml(HtmlTag("datalist", attrs, content))
    pub def fieldset(attrs: List[HtmlAttr], content: Html): Html    = makeHtml(HtmlTag("fieldset", attrs, content))
    pub def form(attrs: List[HtmlAttr], content: Html): Html        = makeHtml(HtmlTag("form", attrs, content))
    pub def input(attrs: List[HtmlAttr], content: Html): Html       = makeHtml(HtmlTag("input", attrs, content))
    pub def label(attrs: List[HtmlAttr], content: Html): Html       = makeHtml(HtmlTag("label", attrs, content))
    pub def legend(attrs: List[HtmlAttr], content: Html): Html      = makeHtml(HtmlTag("legend", attrs, content))
    pub def meter(attrs: List[HtmlAttr], content: Html): Html       = makeHtml(HtmlTag("meter", attrs, content))
    pub def optgroup(attrs: List[HtmlAttr], content: Html): Html    = makeHtml(HtmlTag("optgroup", attrs, content))
    pub def option(attrs: List[HtmlAttr], content: Html): Html      = makeHtml(HtmlTag("option", attrs, content))
    pub def output(attrs: List[HtmlAttr], content: Html): Html      = makeHtml(HtmlTag("output", attrs, content))
    pub def progress(attrs: List[HtmlAttr], content: Html): Html    = makeHtml(HtmlTag("progress", attrs, content))
    pub def selectHtml(attrs: List[HtmlAttr], content: Html): Html  = makeHtml(HtmlTag("select", attrs, content))
    pub def textarea(attrs: List[HtmlAttr], content: Html): Html    = makeHtml(HtmlTag("textarea", attrs, content))

    /// 
    /// Interactive elements
    ///
    pub def details(attrs: List[HtmlAttr], content: Html): Html     = makeHtml(HtmlTag("details", attrs, content))
    pub def dialog(attrs: List[HtmlAttr], content: Html): Html      = makeHtml(HtmlTag("dialog", attrs, content))
    pub def menu(attrs: List[HtmlAttr], content: Html): Html        = makeHtml(HtmlTag("menu", attrs, content))
    pub def summary(attrs: List[HtmlAttr], content: Html): Html     = makeHtml(HtmlTag("summary", attrs, content))


    /// 
    /// Web components
    ///
    pub def slot(attrs: List[HtmlAttr], content: Html): Html        = makeHtml(HtmlTag("slot", attrs, content))
    pub def template(attrs: List[HtmlAttr], content: Html): Html    = makeHtml(HtmlTag("template", attrs, content))
    

    def enumTrueOrFalse(x: Bool): String = if (x) "true" else "false"
    def enumYesOrNo(x: Bool): String = if (x) "yes" else "no"

    /// 
    /// Global attributes
    /// See: https://html.spec.whatwg.org/dev/dom.html#global-attributes
    ///

    pub def class_(s: String): HtmlAttr                 = HtmlAttr("class", s)
    pub def id_(s: String): HtmlAttr                    = HtmlAttr("id", s)
    pub def slot_(s: String): HtmlAttr                  = HtmlAttr("slot", s)

    pub def accesskey_(s: String): HtmlAttr             = HtmlAttr("accesskey", s)
    pub def autocapitalize_(s: String): HtmlAttr        = HtmlAttr("autocapitalize", s)
    pub def autofocus_(): HtmlAttr                      = HtmlAttrBoolean("autofocus")
    pub def contenteditable_(x: Bool): HtmlAttr         = HtmlAttr("contenteditable", enumTrueOrFalse(x))
    pub def dir_(s: String): HtmlAttr                   = HtmlAttr("dir", s)
    pub def draggable_(x: Bool): HtmlAttr               = HtmlAttr("draggable", enumTrueOrFalse(x))
    pub def enterkeyhit_(s: String): HtmlAttr           = HtmlAttr("enterkeyhit", s)
    pub def hidden_(): HtmlAttr                         = HtmlAttrBoolean("hidden")
    pub def inputmode_(s: String): HtmlAttr             = HtmlAttr("inputmode", s)
    pub def is_(s: String): HtmlAttr                    = HtmlAttr("is", s)
    pub def itemid_(s: String): HtmlAttr                = HtmlAttr("itemid", s)
    pub def itemprop_(s: String): HtmlAttr              = HtmlAttr("itemprop", s)
    pub def itemref_(s: String): HtmlAttr               = HtmlAttr("itemref", s)
    pub def itemscope_(): HtmlAttr                      = HtmlAttrBoolean("itemscope")
    pub def itemtype_(s: String): HtmlAttr              = HtmlAttr("itemtype", s)
    pub def lang_(s: String): HtmlAttr                  = HtmlAttr("lang", s)
    pub def nonce_(s: String): HtmlAttr                 = HtmlAttr("nonce", s)
    pub def spellcheck_(x: Bool): HtmlAttr              = HtmlAttr("spellcheck", enumTrueOrFalse(x))
    pub def style_(s: String): HtmlAttr                 = HtmlAttr("style", s)
    pub def tabindex_(i: Int32): HtmlAttr               = HtmlAttr("tabindex", Int32.toString(i))
    pub def title_(s: String): HtmlAttr                 = HtmlAttr("title", s)
    pub def translate_(x: Bool): HtmlAttr               = HtmlAttr("translate", enumYesOrNo(x))
    
    /// 
    /// Other attributes
    /// TODO - see https://html.spec.whatwg.org/multipage/indices.html#attributes-3

    pub def abbr_(s: String): HtmlAttr                  = HtmlAttr("abbr", s)
    pub def accept_(s: String): HtmlAttr                = HtmlAttr("accept", s)
    pub def accept_charset_(s: String): HtmlAttr        = HtmlAttr("accept-charset", s)
    pub def action_(s: String): HtmlAttr                = HtmlAttr("action", s)
    pub def align_(s: String): HtmlAttr                 = HtmlAttr("align", s)
    pub def allow_(s: String): HtmlAttr                 = HtmlAttr("allow", s)
    pub def allowfullscreen_(): HtmlAttr                = HtmlAttrBoolean("allowfullscreen")
    pub def allowpaymentrequest_(): HtmlAttr            = HtmlAttrBoolean("allowpaymentrequest")
    pub def alt_(s: String): HtmlAttr                   = HtmlAttr("alt", s)
    pub def as_(s: String): HtmlAttr                    = HtmlAttr("as", s)
    pub def async_(): HtmlAttr                          = HtmlAttrBoolean("async")
    pub def autocomplete_(s: String): HtmlAttr          = HtmlAttr("autocomplete", s)
    pub def autoplay_(): HtmlAttr                       = HtmlAttrBoolean("autoplay")
    pub def charset_(s: String): HtmlAttr               = HtmlAttr("charset", s)
    pub def checked_(): HtmlAttr                        = HtmlAttrBoolean("checked")
    pub def cite_(s: String): HtmlAttr                  = HtmlAttr("cite", s)
    pub def cols_(x: Int32): HtmlAttr                   = HtmlAttr("cols", Int32.toString(x))
    pub def colspan_(x: Int32): HtmlAttr                = HtmlAttr("colspan", Int32.toString(x))
    pub def content_(s: String): HtmlAttr               = HtmlAttr("content", s)
    pub def controls_(): HtmlAttr                       = HtmlAttrBoolean("controls")
    pub def crossorigin_(s: String): HtmlAttr           = HtmlAttr("crossorigin", s)
    pub def data_(s: String): HtmlAttr                  = HtmlAttr("data", s)
    pub def datetime_(s: String): HtmlAttr              = HtmlAttr("datetime", s)
    pub def decoding_(s: String): HtmlAttr              = HtmlAttr("decoding", s)
    pub def default_(): HtmlAttr                        = HtmlAttrBoolean("default")
    pub def defer_(): HtmlAttr                          = HtmlAttrBoolean("defer")
    pub def dirname_(s: String): HtmlAttr               = HtmlAttr("dirname", s)
    pub def disabled_(): HtmlAttr                       = HtmlAttrBoolean("disabled")
    pub def download_(s: String): HtmlAttr              = HtmlAttr("download", s)
    pub def enctype_(s: String): HtmlAttr               = HtmlAttr("enctype", s)
    pub def for_(s: String): HtmlAttr                   = HtmlAttr("for", s)
    pub def form_(s: String): HtmlAttr                  = HtmlAttr("form", s)
    pub def formaction_(s: String): HtmlAttr            = HtmlAttr("formaction", s)
    pub def formenctype_(s: String): HtmlAttr           = HtmlAttr("formenctype", s)
    pub def formmethod_(s: String): HtmlAttr            = HtmlAttr("formmethod", s)
    pub def formnovalidate_(s: String): HtmlAttr        = HtmlAttr("formnovalidate", s)
    pub def formtarget_(s: String): HtmlAttr            = HtmlAttr("formtarget", s)
    pub def headers_(s: String): HtmlAttr               = HtmlAttr("headers", s)
    pub def height_(s: String): HtmlAttr                = HtmlAttr("height", s)
    pub def high_(s: String): HtmlAttr                  = HtmlAttr("high", s)       // Float64?
    pub def href_(s: String): HtmlAttr                  = HtmlAttr("href", s)
    pub def hreflang_(s: String): HtmlAttr              = HtmlAttr("hreflang", s)
    pub def http_equiv_(s: String): HtmlAttr            = HtmlAttr("http-equiv", s)
    pub def imagesizes_(s: String): HtmlAttr            = HtmlAttr("imagesizes", s)
    pub def imagesrcset_(s: String): HtmlAttr           = HtmlAttr("imagesrcset", s)
    pub def integrity_(s: String): HtmlAttr             = HtmlAttr("integrity", s)
    pub def ismap_(): HtmlAttr                          = HtmlAttrBoolean("ismap")
    pub def kind_(s: String): HtmlAttr                  = HtmlAttr("kind", s)
    pub def label_(s: String): HtmlAttr                 = HtmlAttr("label", s)
    pub def list_(s: String): HtmlAttr                  = HtmlAttr("list", s)
    pub def loop_(): HtmlAttr                           = HtmlAttrBoolean("loop")
    pub def low_(s: String): HtmlAttr                   = HtmlAttr("low", s)       // Float64?
    pub def manifest_(s: String): HtmlAttr              = HtmlAttr("manifest", s)
    pub def max_(s: String): HtmlAttr                   = HtmlAttr("max", s)
    pub def maxlength_(x: Int32): HtmlAttr              = HtmlAttr("maxlength", Int32.toString(x))
    pub def media_(s: String): HtmlAttr                 = HtmlAttr("media", s)
    pub def method_(s: String): HtmlAttr                = HtmlAttr("method", s)
    pub def min_(s: String): HtmlAttr                   = HtmlAttr("min", s)
    pub def minlength_(x: Int32): HtmlAttr              = HtmlAttr("minlength", Int32.toString(x))
    pub def multiple_(): HtmlAttr                       = HtmlAttrBoolean("multiple")
    pub def muted_(): HtmlAttr                          = HtmlAttrBoolean("muted")
    pub def name_(s: String): HtmlAttr                  = HtmlAttr("name", s)
    pub def nomodule_(): HtmlAttr                       = HtmlAttrBoolean("nomodule")
    pub def novalidate_(): HtmlAttr                     = HtmlAttrBoolean("novalidate")
    pub def open_(): HtmlAttr                           = HtmlAttrBoolean("open")
    pub def optimum_(s: String): HtmlAttr               = HtmlAttr("optimum", s)       // Float64?
    pub def pattern_(s: String): HtmlAttr               = HtmlAttr("pattern", s)
    pub def ping_(s: String): HtmlAttr                  = HtmlAttr("ping", s)
    pub def placeholder_(s: String): HtmlAttr           = HtmlAttr("placeholder", s)
    pub def playsinline_(): HtmlAttr                    = HtmlAttrBoolean("playsinline")
    pub def poster_(s: String): HtmlAttr                = HtmlAttr("poster", s)
    pub def preload_(s: String): HtmlAttr               = HtmlAttr("preload", s)
    pub def readonly_(): HtmlAttr                       = HtmlAttrBoolean("readonly")
    pub def referrerpolicy_(s: String): HtmlAttr        = HtmlAttr("referrerpolicy", s)
    pub def rel_(s: String): HtmlAttr                   = HtmlAttr("rel", s)
    pub def required_(): HtmlAttr                       = HtmlAttrBoolean("required")
    pub def rows_(x: Int32): HtmlAttr                   = HtmlAttr("rows", Int32.toString(x))
    pub def rowspan_(x: Int32): HtmlAttr                = HtmlAttr("rowspan", Int32.toString(x))
    pub def sandbox_(s: String): HtmlAttr               = HtmlAttr("sandbox", s)
    pub def scope_(s: String): HtmlAttr                 = HtmlAttr("scope", s)
    pub def selected_(): HtmlAttr                       = HtmlAttrBoolean("selected")
    pub def shape_(s: String): HtmlAttr                 = HtmlAttr("shape", s)
    pub def size_(x: Int32): HtmlAttr                   = HtmlAttr("size", Int32.toString(x))
    pub def sizes_(s: String): HtmlAttr                 = HtmlAttr("sizes", s)
    pub def span_(x: Int32): HtmlAttr                   = HtmlAttr("span", Int32.toString(x))
    pub def src_(s: String): HtmlAttr                   = HtmlAttr("src", s)
    pub def srcdoc_(s: String): HtmlAttr                = HtmlAttr("srcdoc", s)
    pub def srclang_(s: String): HtmlAttr               = HtmlAttr("srclang", s)
    pub def srcset_(s: String): HtmlAttr                = HtmlAttr("srcset", s)
    pub def start_(x: Int32): HtmlAttr                  = HtmlAttr("start", Int32.toString(x))
    pub def step_(s: String): HtmlAttr                  = HtmlAttr("step", s)
    pub def target_(s: String): HtmlAttr                = HtmlAttr("target", s)
    pub def type_(s: String): HtmlAttr                  = HtmlAttr("type", s)
    pub def usemap_(s: String): HtmlAttr                = HtmlAttr("usemap", s)
    pub def value_(s: String): HtmlAttr                 = HtmlAttr("value", s)
    pub def width_(s: String): HtmlAttr                 = HtmlAttr("width", s)
    pub def wrap_(s: String): HtmlAttr                  = HtmlAttr("wrap", s)

}
