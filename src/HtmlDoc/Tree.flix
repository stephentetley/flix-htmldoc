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

use Data.{RoseTree, RoseForest};
use Data.RoseTree.{RoseTree};
use Data/RoseTree.{Forest};
use Text/HtmlDoc.{Html, HtmlAttr, text, li, ul, details, summary, concat, open_};
use Text/HtmlDoc.{<&>};

namespace Text/HtmlDoc/Tree {

    type alias LabelPrinter[a] = { makeAttrs: a -> List[HtmlAttr], printNode: a -> Html}



    def collapsibleNode(printer: LabelPrinter[a], node: RoseTree[a], k: Html -> Html): Html = 
        let mkAttrs = printer.makeAttrs;
        let mkNode = printer.printNode;
        match node {
            case RoseTree(label, Nil) => { 
                let ans = li(mkAttrs(label), mkNode(label));
                k(ans)
            }
            case RoseTree(label, kids) => { 
                let ans = summary(mkAttrs(label), mkNode(label));
                collapsibleKids(printer, kids, vs -> k(li(Nil, details(open_() :: Nil, ans <&> ul(Nil, concat(vs))))))
            }
        }   

    def collapsibleKids(printer: LabelPrinter[a], nodes: RoseForest[a], k: List[Html] -> Html): Html = match nodes { 
        case Nil => k(Nil)
        case x :: rs => 
            collapsibleNode(printer, x, v1 -> collapsibleKids(printer, rs, vs -> k(v1 :: vs)))
    }
        


    pub def collapsibleTree(printer: LabelPrinter[a], node: RoseTree[a]): Html = 
        let mkAttrs = printer.makeAttrs;
        let mkNode = printer.printNode;
        match node {
            case RoseTree(label, kids) => { 
                let ans = summary(mkAttrs(label), mkNode(label));
                collapsibleKids(printer, kids, vs -> details(open_() :: Nil, ans <&> ul(Nil, concat(vs))))
            }
        }

    // TODO - printer for static tree that doesn't use `summary` and `details`.

}