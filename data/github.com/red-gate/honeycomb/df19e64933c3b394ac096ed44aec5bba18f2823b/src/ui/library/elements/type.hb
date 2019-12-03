{{#> layouts/ui-library title="Type"}}
    <h1>Type</h1>

    <div class="js-tabbed tabbed" data-tabs-pagination="true">
        <nav>
            <ul class="tabs tabs--left">
                <li><a href="#examples">Examples</a></li>
                <li><a href="#fundamentals">Fundamentals</a></li>
                <li><a href="#code">Code</a></li>
                <li><a href="#resources">Resources</a></li>
            </ul>
        </nav>

        <div class="js-tab tabbed__content" id="examples">
            <h2>Examples</h2>

            <div class="grid__row">                          
                <div class="grid__col grid__col--span-6-of-12">
                    <div class="padded-right--tight"> 
                        <h4 class="alpha">This is a heading 1</h4>
                        <p>Summary: This study examined the effects of enhanced layout (headers, indentation, and figure placement) on reading performance, comprehension, and satisfaction. Participants read text passages with and without enhanced layout. Results showed that reading speed and comprehension were not affected by layout, however, participants were more satisfied with the enhanced layout and reported it to be less fatiguing to read.</p>
                    </div>
                </div>

                <div class="grid__col grid__col--span-6-of-12">
                    <div class="padded-left--tight"> 
                        <h4 class="beta">This looks like a heading 2</h4>
                        <p>Summary: This study examined the effects of enhanced layout (headers, indentation, and figure placement) on reading performance, comprehension, and satisfaction. Participants read text passages with and without enhanced layout. Results showed that reading speed and comprehension were not affected by layout, however, participants were more satisfied with the enhanced layout and reported it to be less fatiguing to read.</p>
                    </div>
                </div>               
            </div>

            <hr class="spaced-v" />

            <h1>This is a level 1 heading (36px)</h1>
            <h2>This is a level 2 heading (24px)</h2>
            <h3>This is a level 3 heading (18px)</h3>
            <h4>This is a level 4 heading (16px)</h4>
            <h5>This is a level 5 heading (14px)</h5>
            <h6>This is a level 6 heading (12px)</h6>
            <p>This is paragraph text (16px)</p>
            <p><small>This is small paragraph text</small></p>
            <p><a href="#">This is a hyperlink</a></p>

            <h2>Apply header styles to other elements</h2>
            <p>You may want to style a level 2 heading like a level 1. You can do this by applying the following HTML classes:</p>
            <ul>
                <li><span class="alpha">alpha</span> - Applies level 1 heading style</li>
                <li><span class="beta">beta</span> - Applies level 2 heading style</li>
                <li><span class="gamma">gamma</span> - Applies level 3 heading style</li>
                <li><span class="delta">delta</span> - Applies level 4 heading style</li>
                <li><span class="epsilon">epsilon</span> - Applies level 5 heading style</li>
                <li><span class="zeta">zeta</span> - Applies level 6 heading style</li>
            </ul>
        </div>

        <div class="js-tab tabbed__content" id="fundamentals">
            <h2>Fundamentals</h2>
            <h3>Summary</h3>
            <ul class="">
                <li><a href="#default-roboto" class="js-scroll-to">Roboto is our default typeface</a></li>
                <li><a href="#clear-hierarchy" class="js-scroll-to">Create clear hierarchy</a></li>
                <li><a href="#line-length" class="js-scroll-to">Aim for a line-length of 50-80 characters</a></li>
                <li><a href="#uppercase" class="js-scroll-to">Use UPPERCASE sparingly</a></li>
                <li><a href="#contrast" class="js-scroll-to">Use enough contrast</a></li>
            </ul>

            <hr class="spaced-v" />

            <div class="grid__row">
                <h3 id="default-roboto">Roboto is our default typeface</h3>
                <div class="grid__col grid__col--span-6-of-12">
                    <div class="padded-right--tight"> 
                        <p class="icon--tick icon--3x color--green"></p>
                        <p class="alpha"> We use Roboto for both marketing and products</p>
                    </div>   
                </div>             

                <div class="grid__col grid__col--span-6-of-12">
                    <div class="padded-left--tight"> 
                        <p class="icon--flag icon--2x color--orange"></p>
                        <h4>Exceptions</h4>
                        <p><b>Segoe UI</b> is used when Roboto is not suitable. For example ReadyRoll uses Segoe UI because it sits within a Microsoft product.</p>
                        <p><b>Redgate display typeface</b> is only used for Redgate logos and some titles on marketing material.</p>
                    </div>
                </div>
            </div>

            <hr class="spaced-v" />

            <div class="grid__row">
                <h3 id="clear-hierarchy">Create clear hierarchy</h3>
                <div class="grid__col grid__col--span-6-of-12">
                    <div class="padded-right--tight"> 
                        <img class="js-svg spaced-bottom--tight" src="/assets/images/typography/good-hierarchy.svg" alt="UI with good typographical hierarchy" width="220" height="152">                   
                        <p class="icon--tick icon--3x color--green"></p>
                        <h4>Use size, weight, and spacing to create hierarchy</h4>
                        <p>Using the correct size, weight, and spacing of text will make it easier to scan and find information, saving users time, making it quicker to perform the correct action.</p>
                    </div>
                </div>

                <div class="grid__col grid__col--span-6-of-12">
                    <div class="padded-left--tight"> 
                        <img class="js-svg spaced-bottom--tight" src="/assets/images/typography/bad-hierarchy.svg" alt="UI with bad typographical hierarchy" width="220" height="152">
                        <p class="icon--cross icon--3x color--red"></p>
                        <h4>Don't hide information</h4>
                        <p>Not having the correct hierarchy has the same effect as hiding information. It could even cause misunderstandings that lead to wrong actions being taken.  </p>
                    </div>
                </div>               
            </div>

            <hr class="spaced-v" />

            <div class="grid__row">
                <h3 id="line-length">Aim for a line-length of 50-80 characters</h3>
                <div class="grid__col grid__col--span-6-of-12">
                    <div class="padded-right--tight"> 
                        <img class="js-svg spaced-bottom--tight" src="/assets/images/typography/ideal-line-length.svg" alt="UI with ideal line-length" width="220" height="152">                   
                        <p class="icon--tick icon--3x color--green"></p>
                        <h4>50-80 characters is the magic number</h4>
                        <p>Most users will feel more comfortable reading with a line-length of 50-80 characters, especially in long bodies of text. It could reduce fatigue, and increase retention rates.</p>
                    </div>
                </div>

                <div class="grid__col grid__col--span-6-of-12">
                    <div class="padded-left--tight"> 
                        <img class="js-svg spaced-bottom--tight" src="/assets/images/typography/long-line-length.svg" alt="UI with long line-length" width="220" height="152">
                        <p class="icon--cross icon--3x color--red"></p>
                        <h4>Avoid long and short line-lengths</h4>
                        <p>Line-lengths over 80 and under 50 characters can feel daunting</p>
                    </div>
                </div>               
            </div>

            <hr class="spaced-v" />

            <div class="grid__row">
                <h3 id="uppercase">Use UPPERCASE sparingly</h3>
                <div class="grid__col grid__col--span-6-of-12">
                    <div class="padded-right--tight"> 
                        <img class="js-svg spaced-bottom--tight" src="/assets/images/typography/ideal-line-length.svg" alt="UI with ideal line-length" width="220" height="152">                   
                        <p class="icon--tick icon--3x color--green"></p>
                        <h4>Using uppercase</h4>
                        <p>Uppercase text should be used sparingly. Only use uppercase text when it helps to scan infornation, for example tags and labels</p>
                    </div>
                </div>

                <div class="grid__col grid__col--span-6-of-12">
                    <div class="padded-left--tight"> 
                        <img class="js-svg spaced-bottom--tight" src="/assets/images/typography/long-line-length.svg" alt="UI with long line-length" width="220" height="152">
                        <p class="icon--cross icon--3x color--red"></p>
                        <h4>Never SHOUT</h4>
                        <p>Uppercase text could be percieved as shouting, and Redgate never shouts. Avoid using uppercase text when there's even the slightest chance of it being pecieved as shouting.  </p>
                    </div>
                </div>               
            </div>

            <hr class="spaced-v" />

            <div class="grid__row spaced-bottom">
                <h3 id="contrast">Use enough contrast</h3>
                <div class="grid__col grid__col--span-6-of-12">
                    <div class="padded-right--tight"> 
                        <img class="js-svg spaced-bottom--tight" src="/assets/images/typography/good-contrast.svg" alt="UI with good contrast" width="220" height="152">                   
                        <p class="icon--tick icon--3x color--green"></p>
                        <h4>Text contrast must meet standards</h4>
                        <p>Text contrast must meet the WCAG 2.0 level AA standards. Our products and services will then be accessible to more users.</p>
                    </div>
                </div>

                <div class="grid__col grid__col--span-6-of-12">
                    <div class="padded-left--tight"> 
                        <img class="js-svg spaced-bottom--tight" src="/assets/images/typography/bad-contrast.svg" alt="UI with bad contrast" width="220" height="152">
                        <p class="icon--cross icon--3x color--red"></p>
                        <h4>Low-contrast text is not the answer</h4>
                        <p>Not using enough contrast could make text invisible to some users. This leads to wrong actions being taken and could make usability impossible.  </p>
                    </div>
                </div>               
            </div>

        </div>

        <div class="js-tab tabbed__content" id="code">
            <h2>Code</h2>
            
            <h1>This is a level 1 heading</h1>
            {{#> code-sample }}
<h1>This is a level 1 heading</h1>
            {{/code-sample }}
            
            <h2>This is a level 2 heading</h2>
            {{#> code-sample }}
<h2>This is a level 2 heading</h2>
            {{/code-sample }}

            <h3>This is a level 3 heading</h3>
            {{#> code-sample }}
<h3>This is a level 3 heading</h3>
            {{/code-sample }}

            <h4>This is a level 4 heading</h4>
            {{#> code-sample }}
<h4>This is a level 4 heading</h4>
            {{/code-sample }}

            <h5>This is a level 5 heading</h5>
            {{#> code-sample }}
<h5>This is a level 5 heading</h5>
            {{/code-sample }}

            <p>This is paragraph text</p>
            {{#> code-sample }}
<p>This is paragraph text</p>
            {{/code-sample }}

            <p><small>This is small paragraph text</small></p>
            {{#> code-sample }}
<p><small>This is small paragraph text</small></p>
            {{/code-sample }}

            <a href="#">This is a hyperlink</a>
            {{#> code-sample }}
<a href="#">This is a hyperlink</a>
            {{/code-sample }}

            <p class="gamma">This is a paragraph that looks like a level 3 heading</p>
            {{#> code-sample }}
<p class="gamma">This is a paragraph that looks like a level 3 heading</p>
            {{/code-sample }}            
        </div>

        <div class="js-tab tabbed__content" id="resources">
            <h2>Resources</h2>

            <h3> Download fonts</h3>
            <p><a href="https://material.io/guidelines/resources/roboto-noto-fonts.html" target="_blank" class="icon--external icon--right">Roboto</a></p>
            
            <hr class="spaced-v" />
            
            <h3> Interesting reading</h3>
            <p><a href="http://usabilitynews.org/the-effects-of-line-length-on-children-and-adults-online-reading-performance/" target="_blank" class="icon--external icon--right">The effects of line length on children and adults</a></p>
            <p><a href="https://www.nngroup.com/articles/low-contrast/" target="_blank" class="icon--external icon--right">Low-Contrast Text Is Not the Answer</a></p>
        </div>
    </div>
{{/layouts/ui-library}}
