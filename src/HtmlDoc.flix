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
    
namespace Text/HtmlDoc {
    
    /// TODO - probably shouldn't be exposed...
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


    /// Potentially we need a no-pretty version for speed.


    pub def toString(x: Html): String = outputHtml(80, x)

    pub def outputHtml(width: Int32, x: Html): String = 
        prettyPrint(width, ppHtml(x))

    pub def appendHtml!(sb: StringBuilder, x: Html, width: Int32): Unit & Impure = 
        prettyPrint!(sb, width, ppHtml(x))

    pub def writeHtml(width: Int32, path: String, cs: Text.Charset, x: Html): Result[Unit, String] & Impure = 
        use Result.flatMap;
        let str = prettyPrint(width, ppDocType() <!!> ppHtml(x));
        let path1 = System/FilePath.new(path);
        System/File.writeFile(path1, cs, str)

    def <>(d1: Doc, d2: Doc): Doc = beside(d1, d2)
    def <<>>(d1: Doc, d2: Doc): Doc = besideSpace(d1, d2)
    def <!!>(d1: Doc, d2: Doc): Doc = besideSoftbreak(d1, d2)

    /// TODO - CPS
    def ppHtml(x: Html): Doc = 
        let Html(f) = x;
        let xs = List.map(ppHtmlElement, f(Nil));
        vcat(xs)

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

    pub def html(attrs: List[HtmlAttr], content: Html): Html = makeHtml(HtmlTag("html", attrs, content))
    pub def head(attrs: List[HtmlAttr], content: Html): Html = makeHtml(HtmlTag("head", attrs, content))
    pub def body(attrs: List[HtmlAttr], content: Html): Html = makeHtml(HtmlTag("body", attrs, content))

    pub def title(attrs: List[HtmlAttr], content: Html): Html = makeHtml(HtmlTag("title", attrs, content))

    pub def h1(attrs: List[HtmlAttr], content: Html): Html = makeHtml(HtmlTag("h1", attrs, content))
    pub def h2(attrs: List[HtmlAttr], content: Html): Html = makeHtml(HtmlTag("h2", attrs, content))
    pub def h3(attrs: List[HtmlAttr], content: Html): Html = makeHtml(HtmlTag("h3", attrs, content))
    pub def h4(attrs: List[HtmlAttr], content: Html): Html = makeHtml(HtmlTag("h4", attrs, content))
    pub def h5(attrs: List[HtmlAttr], content: Html): Html = makeHtml(HtmlTag("h5", attrs, content))
    pub def h6(attrs: List[HtmlAttr], content: Html): Html = makeHtml(HtmlTag("h6", attrs, content))

    pub def link(attrs: List[HtmlAttr]): Html = makeHtml(HtmlTagNoBody("link", attrs))

    pub def table(attrs: List[HtmlAttr], content: Html): Html = makeHtml(HtmlTag("table", attrs, content))

    pub def tr(attrs: List[HtmlAttr], content: Html): Html = makeHtml(HtmlTag("tr", attrs, content))
    pub def td(attrs: List[HtmlAttr], content: Html): Html = makeHtml(HtmlTag("td", attrs, content))
    pub def th(attrs: List[HtmlAttr], content: Html): Html = makeHtml(HtmlTag("th", attrs, content))
 
    pub def ol(attrs: List[HtmlAttr], content: Html): Html = makeHtml(HtmlTag("ol", attrs, content))
    pub def ul(attrs: List[HtmlAttr], content: Html): Html = makeHtml(HtmlTag("ul", attrs, content))
    pub def li(attrs: List[HtmlAttr], content: Html): Html = makeHtml(HtmlTag("li", attrs, content))

    pub def anchor(attrs: List[HtmlAttr], content: Html): Html = makeHtml(HtmlTag("a", attrs, content))


    pub def href_(s: String): HtmlAttr = HtmlAttr("href", s)
    pub def type_(s: String): HtmlAttr = HtmlAttr("type", s)
    pub def colspan_(s: String): HtmlAttr = HtmlAttr("colspan", s)
    pub def rel_(s: String): HtmlAttr = HtmlAttr("rel", s)

}