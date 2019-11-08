/*****************************************************************************
 * Copyright © 2018 Colin Bartolome
 * 
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy
 * of the License at
 * 
 *    http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations
 * under the License.
 *****************************************************************************/

import ceylon.markdown.parser {
    ListData,
    Node,
    NodeType,
    SourceLoc,
    SourcePos
}
import ceylon.markdown.renderer {
    RenderOptions
}
import ceylon.test {
    parameters,
    test
}

// These tests are still a bit naive and probably need some refactoring to make them more robust and
// test more situations.

{[Integer, String, String]*} testLinkHeadingsOptionParameters = {
    [1, "Test Heading", "test_heading"],
    [2, "Test Heading", "test_heading"],
    [3, "Test Heading", "test_heading"],
    [4, "Test Heading", "test_heading"],
    [5, "Test Heading", "test_heading"],
    [6, "Test Heading", "test_heading"]
};

{Boolean*} testSafeOptionDestinationParameters = { false, true };

{String*} testSoftBreakOptionParameters = {
    "ZZZ"
};

{[NodeType, Integer, Integer, Integer, Integer, Anything(Node)?]*} testSourcePosOptionParameters = {
    // The commented-out values were not implemented in commonmark.js, so they weren't implemented
    // here, either. If this changes in the future, we can uncomment those values.
    [NodeType.blockQuote, 1, 2, 3, 4, null],
    //[NodeType.code, 2, 3, 4, 5, null],
    [NodeType.codeBlock, 3, 4, 5, 6, null],
    //[NodeType.document, 4, 5, 6, 7, null],
    //[NodeType.emphasis, 5, 6, 7, 8, null],
    [NodeType.heading, 1, 7, 8, 9, (Node node) => node.level = 1],
    [NodeType.heading, 2, 7, 8, 9, (Node node) => node.level = 2],
    [NodeType.heading, 3, 7, 8, 9, (Node node) => node.level = 3],
    [NodeType.heading, 4, 7, 8, 9, (Node node) => node.level = 4],
    [NodeType.heading, 5, 7, 8, 9, (Node node) => node.level = 5],
    [NodeType.heading, 6, 7, 8, 9, (Node node) => node.level = 6],
    //[NodeType.htmlBlock, 7, 8, 9, 10, null],
    //[NodeType.htmlInline, 8, 9, 10, 11, null],
    //[NodeType.image, 9, 10, 11, 12, null],
    [NodeType.item, 10, 11, 12, 13, null],
    //[NodeType.lineBreak, 11, 12, 13, 14, null],
    [NodeType.link, 12, 13, 14, 15, null],
    [NodeType.list, 1, 14, 15, 16, (Node node) => node.listData = ListData { type = "bullet"; }],
    [NodeType.list, 2, 14, 15, 16, (Node node) => node.listData = ListData { type = "ordered"; }],
    [NodeType.paragraph, 14, 15, 16, 17, null],
    //[NodeType.softBreak, 15, 16, 17, 18, null],
    //[NodeType.strong, 16, 17, 18, 19, null],
    //[NodeType.text, 20, 21, 22, 23, null],
    [NodeType.thematicBreak, 21, 22, 23, 24, null]
};

shared abstract class RendererTests() {
    "This is very similar to [[testLanguageAttribute]], even going so far as to use the same test
     parameters. Both tests essentially test the same thing, except this one has the extra layer of
     making sure the renderer is playing along."
    test
    parameters (`value testLanguageAttributeParameters`)
    shared void testLanguageAttribute(String? defaultLanguage, String? explicitLanguage,
        String? expectedLanguage) {
        value options = RenderOptions {
            defaultLanguage = defaultLanguage;
        };
        value node = codeBlock(explicitLanguage);
        value expectedAttribute = expectedLanguageAttribute(expectedLanguage);
        
        verifyLanguageAttribute(options, node, expectedAttribute);
    }
    
    test
    parameters (`value testLinkHeadingsOptionParameters`)
    shared void testLinkHeadingsOption(Integer level, String headingText, String expectedId) {
        value options = RenderOptions {
            linkHeadings = true;
        };
        value node = Node(NodeType.heading);
        
        node.level = level;
        
        value text = Node(NodeType.text);
        
        text.literal = headingText;
        
        node.appendChild(text);
        
        verifyLinkHeadingsOption(options, node);
    }
    
    test
    parameters (`value testSafeOptionDestinationParameters`)
    shared void testSafeOptionDestination(Boolean image) {
        value options = RenderOptions {
            safe = true;
        };
        value node = Node(image then NodeType.image else NodeType.link);
        
        node.destination = "javascript:void";
        
        verifySafeOptionDestination(options, node, image);
    }
    
    test
    shared void testSafeOptionRawHtml() {
        value options = RenderOptions {
            safe = true;
        };
        value node = Node(NodeType.htmlInline);
        
        node.literal = "<html>";
        
        verifySafeOptionRawHtml(options, node);
    }
    
    test
    parameters (`value testSoftBreakOptionParameters`)
    shared void testSoftBreakOption(String softBreak) {
        value options = RenderOptions {
            softBreak = softBreak;
        };
        value node = Node(NodeType.softBreak);
        
        verifySoftBreakOption(options, node, softBreak);
    }
    
    test
    parameters (`value testSourcePosOptionParameters`)
    shared void testSourcePosOption(NodeType nodeType, Integer startLine, Integer startColumn,
        Integer endLine, Integer endColumn, Anything(Node)? decorator) {
        value options = RenderOptions {
            sourcePos = true;
        };
        value node = Node(nodeType,
            SourcePos(SourceLoc(startLine, startColumn), SourceLoc(endLine, endColumn)));
        
        if (exists decorator) {
            decorator(node);
        }
        
        verifySourcePosOption(options, node, startLine, startColumn, endLine, endColumn);
    }
    
    test
    shared void testSpecialLink() {
        value text = "text";
        value node = Node(NodeType.specialLink);
        
        node.literal = text;
        
        verifySpecialLink(node);
    }
    
    shared formal void verifyLanguageAttribute(RenderOptions options, Node node,
        String? expectedAttribute);
    
    shared formal void verifyLinkHeadingsOption(RenderOptions options, Node node);
    
    shared formal void verifySafeOptionDestination(RenderOptions options, Node node, Boolean image);
    
    shared formal void verifySafeOptionRawHtml(RenderOptions options, Node node);
    
    shared formal void verifySoftBreakOption(RenderOptions options, Node node, String softBreak);
    
    shared formal void verifySourcePosOption(RenderOptions options, Node node, Integer startLine,
        Integer startColumn, Integer endLine, Integer endColumn);
    
    shared formal void verifySpecialLink(Node node);
}
