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

use Text/PrettyPrint.{Doc, prettyPrint, vcat, fillCat, besideSpace, beside, besideSoftline, encloseSep, dquotes, hang};
use Text/PrettyPrint.{text => ppText, char => ppChar, space => ppSpace};
    
namespace Text/HtmlDoc {
    
    /// TODO - probably shouldn't be exposed...
    pub enum HtmlElement {
        case HtmlText(String),
        case HtmlTag(String, List[HtmlAttr], Html),
        case HtmlComment(String)
    }

    pub enum HtmlAttr {
        case HtmlAttr(String, String)
        case HtmlAttrEmpty(String)
    }

    pub opaque type Html = List[HtmlElement] -> List[HtmlElement]



    pub def toString(x: Html): String = toStringWithWidth(x, 80)

    pub def toStringWithWidth(x: Html, width: Int32): String = 
        prettyPrint(width, ppHtml(x))

    def <>(d1: Doc, d2: Doc): Doc = beside(d1, d2)
    def <<>>(d1: Doc, d2: Doc): Doc = besideSpace(d1, d2)
    def <!>(d1: Doc, d2: Doc): Doc = besideSoftline(d1, d2)

    /// TODO - CPS
    def ppHtml(x: Html): Doc = 
        let Html(f) = x;
        vcat(List.map(ppHtmlElement, f(Nil)))

    def ppHtmlElement(x: HtmlElement): Doc = match x { 
        case HtmlText(s)                => ppText(s)
        case HtmlComment(s)             => ppText("<!--") <<>> ppText(s) <<>> ppText("-->")
        case HtmlTag(name, attrs, body) => ppOpenTag(name, attrs) <!> hang(4, ppHtml(body)) <!> ppCloseTag(name)
    }

    def ppOpenTag(name: String, attrs: List[HtmlAttr]): Doc = 
        encloseSep(ppText("<" + name), ppChar('>'), ppSpace(), List.map(ppAttr, attrs))

    def ppCloseTag(name: String): Doc = ppText("<" + name + ">")

    def ppAttr (attr: HtmlAttr): Doc = match attr {
        case HtmlAttrEmpty(name)    => ppText(name)
        case HtmlAttr(name, value)  => ppText(name) <> ppChar('=') <> dquotes(ppText(value))
    }


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

    pub def attr(name: String, value: String): HtmlAttr = HtmlAttr(name, value)

    pub def tag(name: String, attrs: List[HtmlAttr], content: Html): HtmlElement = 
        HtmlTag(name, attrs, content)

    pub def append(x: Html, y: Html): Html = match (x, y) {
        case (Html(k1), Html(k2)) => Html(xs -> k1(k2(xs)))
    }

    pub def html(attrs: List[HtmlAttr], content: Html): Html = makeHtml(tag("html", attrs, content))

    pub def body(attrs: List[HtmlAttr], content: Html): Html = makeHtml(tag("body", attrs, content))

    pub def h1(attrs: List[HtmlAttr], content: Html): Html = makeHtml(tag("h1", attrs, content))
    pub def h2(attrs: List[HtmlAttr], content: Html): Html = makeHtml(tag("h2", attrs, content))
    pub def h3(attrs: List[HtmlAttr], content: Html): Html = makeHtml(tag("h3", attrs, content))
    pub def h4(attrs: List[HtmlAttr], content: Html): Html = makeHtml(tag("h4", attrs, content))
    pub def h5(attrs: List[HtmlAttr], content: Html): Html = makeHtml(tag("h5", attrs, content))
    pub def h6(attrs: List[HtmlAttr], content: Html): Html = makeHtml(tag("h6", attrs, content))

    pub def table(attrs: List[HtmlAttr], content: Html):Html = makeHtml(tag("table", attrs, content))

    pub def tr(attrs: List[HtmlAttr], content: Html): Html = makeHtml(tag("tr", attrs, content))

    pub def td(attrs: List[HtmlAttr], content: Html): Html = makeHtml(tag("td", attrs, content))
 
    pub def ol(attrs: List[HtmlAttr], content: Html): Html = makeHtml(tag("ol", attrs, content))
    pub def ul(attrs: List[HtmlAttr], content: Html): Html = makeHtml(tag("ul", attrs, content))
    pub def li(attrs: List[HtmlAttr], content: Html): Html = makeHtml(tag("li", attrs, content))

    pub def anchor(attrs: List[HtmlAttr], content: Html): Html = makeHtml(tag("a", attrs, content))


    pub def href(s: String): HtmlAttr = attr("href", s)


}