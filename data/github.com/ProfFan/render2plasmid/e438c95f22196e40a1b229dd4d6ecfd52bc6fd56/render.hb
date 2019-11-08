<html>
    <head>
        <script type="text/javascript" src="./js/angular.min.js"></script>
        <script type="text/javascript" src="./js/angularplasmid.complete.min.js"></script>
    </head>
    <body>
        <style>
            /* lato-300 - latin_latin-ext */
            @font-face {
              font-family: 'Lato';
              font-style: normal;
              font-weight: 300;
              src: local('Lato Light'), local('Lato-Light'),
                   url('./fonts/lato-v11-latin_latin-ext-300.woff2') format('woff2'), /* Chrome 26+, Opera 23+ */
                   url('./fonts/lato-v11-latin_latin-ext-300.woff') format('woff'); /* Chrome 6+, Firefox 3.6+, IE 9+, Safari 5.1+ */
            }
            /* lato-300italic - latin_latin-ext */
            @font-face {
              font-family: 'Lato';
              font-style: italic;
              font-weight: 300;
              src: local('Lato Light Italic'), local('Lato-LightItalic'),
                   url('./fonts/lato-v11-latin_latin-ext-300italic.woff2') format('woff2'), /* Chrome 26+, Opera 23+ */
                   url('./fonts/lato-v11-latin_latin-ext-300italic.woff') format('woff'); /* Chrome 6+, Firefox 3.6+, IE 9+, Safari 5.1+ */
            }
            /* lato-regular - latin_latin-ext */
            @font-face {
              font-family: 'Lato';
              font-style: normal;
              font-weight: 400;
              src: local('Lato Regular'), local('Lato-Regular'),
                   url('./fonts/lato-v11-latin_latin-ext-regular.woff2') format('woff2'), /* Chrome 26+, Opera 23+ */
                   url('./fonts/lato-v11-latin_latin-ext-regular.woff') format('woff'); /* Chrome 6+, Firefox 3.6+, IE 9+, Safari 5.1+ */
            }
            /* lato-700 - latin_latin-ext */
            @font-face {
              font-family: 'Lato';
              font-style: normal;
              font-weight: 700;
              src: local('Lato Bold'), local('Lato-Bold'),
                   url('./fonts/lato-v11-latin_latin-ext-700.woff2') format('woff2'), /* Chrome 26+, Opera 23+ */
                   url('./fonts/lato-v11-latin_latin-ext-700.woff') format('woff'); /* Chrome 6+, Firefox 3.6+, IE 9+, Safari 5.1+ */
            }
            /* lato-700italic - latin_latin-ext */
            @font-face {
              font-family: 'Lato';
              font-style: italic;
              font-weight: 700;
              src: local('Lato Bold Italic'), local('Lato-BoldItalic'),
                   url('./fonts/lato-v11-latin_latin-ext-700italic.woff2') format('woff2'), /* Chrome 26+, Opera 23+ */
                   url('./fonts/lato-v11-latin_latin-ext-700italic.woff') format('woff'); /* Chrome 6+, Firefox 3.6+, IE 9+, Safari 5.1+ */
            }
            /* lato-900 - latin_latin-ext */
            @font-face {
              font-family: 'Lato';
              font-style: normal;
              font-weight: 900;
              src: local('Lato Black'), local('Lato-Black'),
                   url('./fonts/lato-v11-latin_latin-ext-900.woff2') format('woff2'), /* Chrome 26+, Opera 23+ */
                   url('./fonts/lato-v11-latin_latin-ext-900.woff') format('woff'); /* Chrome 6+, Firefox 3.6+, IE 9+, Safari 5.1+ */
            }
            body {font-family: 'Lato';font-weight:400;}
            .boundary {stroke-dasharray:2,2;stroke-width:2px}
            .mdlabel {font-size:12px}
            .smlabel {font-size:8px}
            .white {fill:#fff}
            .red {fill:rgb(192,64,64)}
            .purple {fill:rgb(192,64,192)}
            .blue {fill:rgb(64,192,192)}
            .green {fill:rgb(64,192,64)}
            .gold {fill:rgb(192,128,64)}
        </style>
        <plasmid sequencelength="{{metadata.sequence_length}}" plasmidheight="375" plasmidwidth="375">
            <plasmidtrack trackstyle="fill:#ccc" width="5" radius="120"></plasmidtrack>
            <plasmidtrack trackstyle='fill:rgba(225,225,225,0.5);' radius='110'>
                <tracklabel labelstyle='font-size:20px;font-weight:400' text="{{metadata.locusname}}"></tracklabel>
                <tracklabel labelstyle='font-size:10px' vadjust="20" text="{{metadata.sequence_length}} bp"></tracklabel>
                
                <!-- <trackscale interval="{{math metadata.sequence_length "div" 200}}" style="stroke:#999;" vadjust="8"></trackscale>
                <trackscale interval="{{math metadata.sequence_length "div" 50}}" showlabels="1" style="stroke:#333;stroke-width:2px" ticksize="5" vadjust="8" labelstyle="font-size:9px;fill:#999;font-weight:300" labelvadjust="15"></trackscale>
                 -->
                <trackscale interval="{{math metadata.sequence_length "div" 100}}" style='stroke:#999' direction="in" ticksize="3"></trackscale>
                <trackscale interval="{{math metadata.sequence_length "div" 100}}" style='stroke:#999' ticksize="3"></trackscale>
                <trackscale interval="{{math metadata.sequence_length "div" 20}}" style="stroke:#f00" direction="in" showlabels="1" labelstyle="fill:#999;stroke:none;text-anchor:middle;alignment-baseline:middle;font-size:10px"></trackscale>
                <!-- PstI -->
                {{#each features}}
                    {{#ifequal feature "misc_feature"}}
                    <trackmarker start="{{location.[0].start}}" end="{{location.[0].end}}" markerstyle="fill:{{rcolor}}" arrowendlength="4" arrowstartlength="-4">
                        <markerlabel type="path" class="mdlabel white" text="{{label}}"></markerlabel>
                    </trackmarker>
                    <trackmarker start="{{location.[0].start}}" markerstyle="stroke:rgba(128,64,64,0.8)" class="boundary" wadjust="20">
                        <markerlabel class="smlabel red" text="{{location.[0].start}}" vadjust="30"></markerlabel>
                    </trackmarker>
                    <trackmarker start="{{location.[0].end}}" markerstyle="stroke:rgba(128,64,64,0.8)" class="boundary" wadjust="20">
                        <markerlabel class="smlabel red" text="{{location.[0].end}}" vadjust="30"></markerlabel>
                    </trackmarker>
                    <trackmarker start="{{location.[0].start}}" end="{{location.[0].end}}" markerstyle="fill:rgba(255,221,238,0.6)" wadjust="-5" vadjust="25"></trackmarker>
                    {{/ifequal}}
                {{/each}}
                <!-- AcolIII -->
                <!-- <trackmarker start="450" end="620" markerstyle='stroke:#000;fill:#f00;' arrowendlength="10" arrowendwidth="5" wadjust="10" vadjust="-5">
                    <markerlabel text="AcolIII" vadjust="40" hadjust="2" valign="outer" class="lglabel red" showline="1" linevadjust="-10" lineclass="labelline"></markerlabel>
                    <markerlabel type="path" text="400-620" class="smlabel white" vadjust="4"></markerlabel>
                </trackmarker> -->

                <!-- EcoRI -->
                <!-- <trackmarker start="630" end="720" markerstyle='stroke:#000;fill:#f00;' arrowendlength="10" arrowendwidth="5" wadjust="10" vadjust="-5">
                    <markerlabel text="EcoRI" vadjust="40" hadjust="-2" valign="outer" class="lglabel red" showline="1" linevadjust="-20" lineclass="labelline"></markerlabel>
                    <markerlabel type="path" text="630-720" class="smlabel white" vadjust="4"></markerlabel>
                </trackmarker> -->
                                
                <!-- ScaI -->
                <!-- <trackmarker start="760" end="880" markerstyle='stroke:#000;fill:#0f0;' arrowendlength="10" arrowendwidth="5" wadjust="10" vadjust="-5">
                    <markerlabel text="ScaI" vadjust="40" valign="outer" class="lglabel green" showline="1" linevadjust="-15" lineclass="labelline"></markerlabel>
                    <markerlabel type="path" text="760-880" class="smlabel black" vadjust="4"></markerlabel>
                </trackmarker> -->
                
                <!-- TaqI -->
                <!-- <trackmarker start="980" end="150" markerstyle='stroke:#000;fill:#00f;' wadjust="10" vadjust="-5">
                    <markerlabel text="TaqI" vadjust="40" hadjust="2" valign="outer" class="lglabel blue" showline="1" linevadjust="-10" linevadjust="-15" lineclass="labelline"></markerlabel>
                    <markerlabel type="path" text="920-150" class="smlabel white" vadjust="4"></markerlabel>
                </trackmarker> -->

                <!-- Block markers -->
                <!-- <trackmarker start="420" end="430" markerstyle='stroke:#000;fill:#666' wadjust="20" vadjust="-10">
                    <markerlabel text="A35" vadjust="-15" valign="inner" labelclass="mdlabel gray"></markerlabel>
                </trackmarker>
                <trackmarker start="355" end="365" markerstyle='stroke:#000;fill:#666' wadjust="20" vadjust="-10">
                    <markerlabel text="A36" vadjust="-15" valign="inner" labelclass="mdlabel gray"></markerlabel>
                </trackmarker>
                <trackmarker start="905" end="915" markerstyle='stroke:#000;fill:#666' wadjust="20" vadjust="-10">
                    <markerlabel text="A22" vadjust="-15" valign="inner" labelclass="mdlabel gray"></markerlabel>
                </trackmarker>
                <trackmarker start="170" end="180" markerstyle='stroke:#000;fill:#666' wadjust="20" vadjust="-10">
                    <markerlabel text="A14" vadjust="-15" valign="inner" labelclass="mdlabel gray"></markerlabel>
                </trackmarker> -->
            </plasmidtrack>
        </plasmid>
    </body>
</html>
