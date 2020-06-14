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



    def collapsibleNode(printer: LabelPrinter[a], node: RoseTree[a]): Html = 
        let mkAttrs = printer.makeAttrs;
        let mkNode = printer.printNode;
        match node {
            case RoseTree(label, Nil) => { 
                let ans = ul(mkAttrs(label), mkNode(label));
                ans
            }
            case RoseTree(label, kids) => { 
                let ans = summary(mkAttrs(label), mkNode(label));
                let kids1 = collapsibleKids(printer, kids);
                details(open_() :: Nil, li(Nil, ans <&> kids1))
            }
        }   

    def collapsibleKids(printer: LabelPrinter[a], nodes: RoseForest[a]): Html = 
        let elems = List.map(collapsibleNode(printer), nodes);
        concat(elems)



    pub def collapsibleTree(printer: LabelPrinter[a], node: RoseTree[a]): Html = 
        collapsibleNode(printer, node)

}