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

use Text/PrettyPrint.{Doc, prettyPrint, prettyPrint!, vcat, fillCat, besideSpace, beside, besideSoftbreak, encloseSep, dquotes, hang};
use Text/PrettyPrint.{text => ppText, char => ppChar, space => ppSpace};
use Text/PrettyPrint.{<>, <<>>, <!!>};
    
namespace Text/HtmlDoc {
    
    /// TODO - probably shouldn't be exposed...
    /// Do we need a `Raw` element for `script` and `svg` content?
    pub enum HtmlElement {
        case HtmlText(String),
        case HtmlTag(String, List[HtmlAttr], Html),
        case HtmlTagNoBody(String, List[HtmlAttr]),
        case HtmlComment(String)
    }

    pub enum HtmlAttr {
        case HtmlAttr(String, String)
        case HtmlAttrEmpty(String)
    }


    pub opaque type Html = List[HtmlElement] -> List[HtmlElement]


    /// Potentially we need a "no-pretty" version for speed.


    pub def toString(x: Html): String = outputHtml(80, x)

    pub def outputHtml(width: Int32, x: Html): String = 
        prettyPrint(width, ppHtml(x))

    pub def appendHtml!(sb: StringBuilder, x: Html, width: Int32): Unit & Impure = 
        prettyPrint!(sb, width, ppHtml(x))

    pub def writeHtml(width: Int32, path: String, cs: Text.Charset, x: Html): Result[Unit, System.Error] & Impure = 
        use Result.flatMap;
        let str = prettyPrint(width, ppDocType() <!!> ppHtml(x));
        let path1 = System/FilePath.new(path);
        System/File.writeFile(path1, cs, str)


    def ppHtml(x: Html): Doc = 
        let Html(f) = x;
        let xs = List.map(ppHtmlElement, f(Nil));
        vcat(xs)

    /// TODO - CPS?
    def ppHtmlElement(x: HtmlElement): Doc = match x { 
        case HtmlText(s)                => ppText(s)
        case HtmlComment(s)             => ppText("<!--") <<>> ppText(s) <<>> ppText("-->")
        case HtmlTag(name, attrs, body) => ppOpeningTag(name, attrs) <!!> hang(4, ppHtml(body)) <!!> ppClosingTag(name)
        case HtmlTagNoBody(name, attrs) => ppOpeningTag(name, attrs)
    }

    def ppOpeningTag(name: String, attrs: List[HtmlAttr]): Doc = match attrs {
        case Nil => ppText("<" + name + ">")
        case _ => encloseSep(ppText("<" + name + " "), ppChar('>'), ppSpace(), List.map(ppAttr, attrs))
    }

    def ppClosingTag(name: String): Doc = ppText("</" + name + ">")

    // TODO - check for double quotes, if present use single quotes... 
    def ppAttr (attr: HtmlAttr): Doc = match attr {
        case HtmlAttrEmpty(name)    => ppText(name)
        case HtmlAttr(name, value)  => ppText(name) <> ppChar('=') <> dquotes(ppText(value))
    }

    def ppDocType(): Doc = ppText("<!DOCTYPE html>")

    ///
    /// Returns string `s` with all occurences of HTML special characters replaced.
    ///
    def escapeHtml(s: String): String = escapeHtmlHelper(s) as & Pure

    def escapeHtmlHelper(s: String): String & Impure = 
        let sb = StringBuilder.new();
        String.foldLeft((_,c) -> escapeHtmlChar(sb,c), (), s);
        StringBuilder.toString(sb)


    def escapeHtmlChar(sb: StringBuilder, c: Char): Unit & Impure = match c { 
        case '<'        => StringBuilder.appendString!(sb, "&lt;")
        case '>'        => StringBuilder.appendString!(sb, "&gt;")
        case '&'        => StringBuilder.appendString!(sb, "&amp;")
        case '"'        => StringBuilder.appendString!(sb, "&quot;")
        case '\u00A0'   => StringBuilder.appendString!(sb, "&nbsp;")
        case _          => StringBuilder.appendChar!(sb, c)
    }


    def makeHtml(x: HtmlElement): Html = 
        Html(xs -> x :: xs)

    pub def text(s: String): Html = makeHtml(HtmlText(escapeHtml(s)))

    pub def append(x: Html, y: Html): Html = match (x, y) {
        case (Html(k1), Html(k2)) => Html(xs -> k1(k2(xs)))
    }

    pub def <&>(x: Html, y: Html): Html = append(x, y)

    pub def concat(xs: List[Html]): Html = match xs { 
        case Nil => Html(xs -> xs)
        case x :: rs => concatHelper(rs, x)
    }

    pub def concatHelper(xs: List[Html], ac: Html): Html = match xs {
        case Nil => ac
        case x :: rs => concatHelper(rs, ac <&> x)
    }


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
    pub def pHtml(attrs: List[HtmlAttr], content: Html): Html       = makeHtml(HtmlTag("p", attrs, content))
    pub def pre(attrs: List[HtmlAttr], content: Html): Html         = makeHtml(HtmlTag("pre", attrs, content))
    pub def ul(attrs: List[HtmlAttr], content: Html): Html          = makeHtml(HtmlTag("ul", attrs, content))

    ///
    /// Inline text semantics
    ///
    pub def aHtml(attrs: List[HtmlAttr], content: Html): Html       = makeHtml(HtmlTag("a", attrs, content))
    pub def abbr(attrs: List[HtmlAttr], content: Html): Html        = makeHtml(HtmlTag("abbr", attrs, content))
    pub def bold(attrs: List[HtmlAttr], content: Html): Html        = makeHtml(HtmlTag("bold", attrs, content))
    pub def bdi(attrs: List[HtmlAttr], content: Html): Html         = makeHtml(HtmlTag("bdo", attrs, content))
    pub def bdo(attrs: List[HtmlAttr], content: Html): Html         = makeHtml(HtmlTag("bdo", attrs, content))
    pub def br(attrs: List[HtmlAttr], content: Html): Html          = makeHtml(HtmlTag("br", attrs, content))
    pub def cite(attrs: List[HtmlAttr], content: Html): Html        = makeHtml(HtmlTag("cite", attrs, content))
    pub def code(attrs: List[HtmlAttr], content: Html): Html        = makeHtml(HtmlTag("code", attrs, content))
    pub def data(attrs: List[HtmlAttr], content: Html): Html        = makeHtml(HtmlTag("data", attrs, content))
    pub def dfn(attrs: List[HtmlAttr], content: Html): Html         = makeHtml(HtmlTag("dfn", attrs, content))
    pub def em(attrs: List[HtmlAttr], content: Html): Html          = makeHtml(HtmlTag("em", attrs, content))
    pub def iHtml(attrs: List[HtmlAttr], content: Html): Html       = makeHtml(HtmlTag("i", attrs, content))
    pub def kbd(attrs: List[HtmlAttr], content: Html): Html         = makeHtml(HtmlTag("kbd", attrs, content))
    pub def mark(attrs: List[HtmlAttr], content: Html): Html        = makeHtml(HtmlTag("mark", attrs, content))
    pub def qHtml(attrs: List[HtmlAttr], content: Html): Html       = makeHtml(HtmlTag("q", attrs, content))
    pub def rb(attrs: List[HtmlAttr], content: Html): Html          = makeHtml(HtmlTag("rt", attrs, content))
    pub def rp(attrs: List[HtmlAttr], content: Html): Html          = makeHtml(HtmlTag("rp", attrs, content))
    pub def rt(attrs: List[HtmlAttr], content: Html): Html          = makeHtml(HtmlTag("rt", attrs, content))
    pub def rtc(attrs: List[HtmlAttr], content: Html): Html         = makeHtml(HtmlTag("rtc", attrs, content))
    pub def ruby(attrs: List[HtmlAttr], content: Html): Html        = makeHtml(HtmlTag("ruby", attrs, content))
    pub def sHtml(attrs: List[HtmlAttr], content: Html): Html       = makeHtml(HtmlTag("s", attrs, content))
    pub def samp(attrs: List[HtmlAttr], content: Html): Html        = makeHtml(HtmlTag("samp", attrs, content))
    pub def small(attrs: List[HtmlAttr], content: Html): Html       = makeHtml(HtmlTag("small", attrs, content))
    pub def span(attrs: List[HtmlAttr], content: Html): Html        = makeHtml(HtmlTag("span", attrs, content))
    pub def strong(attrs: List[HtmlAttr], content: Html): Html      = makeHtml(HtmlTag("strong", attrs, content))
    pub def sub(attrs: List[HtmlAttr], content: Html): Html         = makeHtml(HtmlTag("sub", attrs, content))
    pub def sup(attrs: List[HtmlAttr], content: Html): Html         = makeHtml(HtmlTag("sup", attrs, content))
    pub def time(attrs: List[HtmlAttr], content: Html): Html        = makeHtml(HtmlTag("time", attrs, content))
    pub def uHtml(attrs: List[HtmlAttr], content: Html): Html       = makeHtml(HtmlTag("u", attrs, content))
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
    pub def object(attrs: List[HtmlAttr], content: Html): Html      = makeHtml(HtmlTag("object", attrs, content))
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


    pub def accesskey_(s: String): HtmlAttr             = HtmlAttr("accesskey", s)
    pub def autocapitalize_(s: String): HtmlAttr        = HtmlAttr("autocapitalize", s)
    pub def autofocus_(): HtmlAttr                      = HtmlAttrEmpty("autofocus")
    pub def class_(s: String): HtmlAttr                 = HtmlAttr("class", s)
    pub def contenteditable_(x: Bool): HtmlAttr         = HtmlAttr("contenteditable", enumTrueOrFalse(x))
    pub def dir_(s: String): HtmlAttr                   = HtmlAttr("dir", s)
    pub def draggable_(x: Bool): HtmlAttr               = HtmlAttr("draggable", enumTrueOrFalse(x))
    pub def enterkeyhit_(s: String): HtmlAttr           = HtmlAttr("enterkeyhit", s)
    pub def hidden_(): HtmlAttr                         = HtmlAttrEmpty("hidden")
    pub def id_(s: String): HtmlAttr                    = HtmlAttr("id", s)
    pub def inputmode_(s: String): HtmlAttr             = HtmlAttr("inputmode", s)
    pub def itemid_(s: String): HtmlAttr                = HtmlAttr("itemid", s)
    pub def itemprop_(s: String): HtmlAttr              = HtmlAttr("itemprop", s)
    pub def itemscope_(): HtmlAttr                      = HtmlAttrEmpty("itemscope")
    pub def lang_(s: String): HtmlAttr                  = HtmlAttr("lang", s)
    pub def spellcheck_(x: Bool): HtmlAttr              = HtmlAttr("spellcheck", enumTrueOrFalse(x))
    pub def style_(s: String): HtmlAttr                 = HtmlAttr("style", s)
    pub def tabindex_(i: Int32): HtmlAttr               = HtmlAttr("tabindex", Int32.toString(i))
    pub def title_(s: String): HtmlAttr                 = HtmlAttr("title", s)
    pub def translate_(x: Bool): HtmlAttr               = HtmlAttr("translate", enumYesOrNo(x))
    
    /// 
    /// Other attributes
    /// TODO - see https://html.spec.whatwg.org/multipage/indices.html#attributes-3

    pub def accept_(s: String): HtmlAttr                = HtmlAttr("accept", s)
    pub def accept_charset_(s: String): HtmlAttr        = HtmlAttr("accept-charset", s)
    pub def action_(s: String): HtmlAttr                = HtmlAttr("action", s)
    pub def align_(s: String): HtmlAttr                 = HtmlAttr("align", s)
    pub def allow_(s: String): HtmlAttr                 = HtmlAttr("allow", s)
    pub def alt_(s: String): HtmlAttr                   = HtmlAttr("alt", s)
    pub def async_(): HtmlAttr                          = HtmlAttrEmpty("async")
    pub def capture_(s: String): HtmlAttr               = HtmlAttr("capture", s)
    pub def charset_(s: String): HtmlAttr               = HtmlAttr("charset", s)
    
    pub def colspan_(s: String): HtmlAttr               = HtmlAttr("colspan", s)
    pub def data_(s: String): HtmlAttr                  = HtmlAttr("data", s)
    pub def disabled_(): HtmlAttr                       = HtmlAttrEmpty("disabled")
    pub def form_(s: String): HtmlAttr                  = HtmlAttr("form", s)
    pub def height_(s: String): HtmlAttr                = HtmlAttr("height", s)
    pub def href_(s: String): HtmlAttr                  = HtmlAttr("href", s)
    pub def open_(): HtmlAttr                           = HtmlAttrEmpty("open")
    pub def rel_(s: String): HtmlAttr                   = HtmlAttr("rel", s)
    pub def src_(s: String): HtmlAttr                   = HtmlAttr("src", s)
    pub def type_(s: String): HtmlAttr                  = HtmlAttr("type", s)
    pub def width_(s: String): HtmlAttr                 = HtmlAttr("width", s)


}