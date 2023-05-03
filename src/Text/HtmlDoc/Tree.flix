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


mod Text.HtmlDoc.Tree {

    
    use RoseTree.{Tree, Forest}
    use RoseTree.Tree.{Node}
    use Text.HtmlDoc.{Html, HtmlAttr, text, li, ul, details, summary, concat, open_}
    use Text.HtmlDoc.{<&>}

    pub type alias LabelPrinter[a] = 
        { makeAttrs = a -> List[HtmlAttr]
        , printNode = a -> Html
        }


    pub def collapsibleTree(printer: LabelPrinter[a], node: Tree[a]): Html = 
        let mkAttrs = printer.makeAttrs;
        let mkNode = printer.printNode;
        match node {
            case Node(label, kids) => { 
                let ans = summary(mkAttrs(label), mkNode(label));
                collapsibleKids(printer, kids, vs -> details(open_() :: Nil, ans <&> ul(Nil, concat(vs))))
            }
        }

    def collapsibleNode(printer: LabelPrinter[a], node: Tree[a], k: Html -> Html): Html = 
        let mkAttrs = printer.makeAttrs;
        let mkNode = printer.printNode;
        match node {
            case Node(label, Nil) => { 
                let ans = li(mkAttrs(label), mkNode(label));
                k(ans)
            }
            case Node(label, kids) => { 
                let ans = summary(mkAttrs(label), mkNode(label));
                collapsibleKids(printer, kids, vs -> k(li(Nil, details(open_() :: Nil, ans <&> ul(Nil, concat(vs))))))
            }
        }   

    def collapsibleKids(printer: LabelPrinter[a], nodes: Forest[a], k: List[Html] -> Html): Html = match nodes { 
        case Nil => k(Nil)
        case x :: rs => 
            collapsibleNode(printer, x, v1 -> collapsibleKids(printer, rs, vs -> k(v1 :: vs)))
    }
        

    /// Version with custom printers for nodes with children and leaves (without children).

    pub def collapsibleTree2(nodePrinter: LabelPrinter[a], leafPrinter: LabelPrinter[a], node: Tree[a]): Html = 
        match node {
            case Node(label, kids) => { 
                let mkAttrs = nodePrinter.makeAttrs;
                let mkNode = nodePrinter.printNode;        
                let ans = summary(mkAttrs(label), mkNode(label));
                collapsibleKids2(nodePrinter, leafPrinter, kids, vs -> details(open_() :: Nil, ans <&> ul(Nil, concat(vs))))
            }
        }


    def collapsibleNode2(nodePrinter: LabelPrinter[a], leafPrinter: LabelPrinter[a], node: Tree[a], k: Html -> Html): Html = 
        match node {
            case Node(label, Nil) => { 
                let mkAttrs = leafPrinter.makeAttrs;
                let mkNode = leafPrinter.printNode;        
                let ans = li(mkAttrs(label), mkNode(label));
                k(ans)
            }
            case Node(label, kids) => { 
                let mkAttrs = nodePrinter.makeAttrs;
                let mkNode = nodePrinter.printNode;    
                let ans = summary(mkAttrs(label), mkNode(label));
                collapsibleKids2(nodePrinter, leafPrinter, kids, vs -> k(li(Nil, details(open_() :: Nil, ans <&> ul(Nil, concat(vs))))))
            }
        }   

    def collapsibleKids2(nodePrinter: LabelPrinter[a], leafPrinter: LabelPrinter[a], nodes: Forest[a], k: List[Html] -> Html): Html = match nodes { 
        case Nil => k(Nil)
        case x :: rs => 
            collapsibleNode2(nodePrinter, leafPrinter, x, v1 -> collapsibleKids2(nodePrinter, leafPrinter, rs, vs -> k(v1 :: vs)))
    }

    // TODO - printer for static tree that doesn't use `summary` and `details`.

}
