<patch-1.0>
   <obj type="ctrl/dial p" sha="501c30e07dedf3d701e8d0b33c3c234908c3388e" uuid="cc5d2846c3d50e425f450c4b9851371b54f4d674" name="Body/Noise Rate" x="658" y="0">
      <params>
         <frac32.u.map name="value" onParent="true" value="41.5"/>
      </params>
      <attribs/>
   </obj>
   <obj type="logic/latch" sha="efe28fa4b70e8abfb7de3dff6cad26b8c6a8c95e" uuid="14750683752bd43205826430adb7168dae3cc2c" name="latch_1" x="14" y="14">
      <params/>
      <attribs/>
   </obj>
   <obj type="math/*" sha="ec71f518aa0e133062e5a9d4828d3739865c99f" uuid="922423f2db9f222aa3e5ba095778288c446da47a" name="*_1" x="378" y="28">
      <params/>
      <attribs/>
   </obj>
   <obj type="logic/latch" sha="efe28fa4b70e8abfb7de3dff6cad26b8c6a8c95e" uuid="14750683752bd43205826430adb7168dae3cc2c" name="latch_2" x="14" y="70">
      <params/>
      <attribs/>
   </obj>
   <obj type="math/+" sha="49587c7bc7e1813d8a9a7a9be012580af00ea274" uuid="44553fdc8628c67ab535845ed1be304ad6c9553b" name="+_2" x="504" y="98">
      <params/>
      <attribs/>
   </obj>
   <comment type="patch/comment" x="658" y="98" text="This envelope is for the noise generator"/>
   <obj type="math/*" sha="ec71f518aa0e133062e5a9d4828d3739865c99f" uuid="922423f2db9f222aa3e5ba095778288c446da47a" name="*_4" x="378" y="112">
      <params/>
      <attribs/>
   </obj>
   <obj type="env/d m" sha="3f6e6c6081782177f0dc9dfe9e50a99b54fe41f6" uuid="85e82f54dfc28839d300cda777af8907ae2a28d0" name="Noise Decay" x="658" y="112">
      <params>
         <frac32.s.map name="d" onParent="true" value="-19.0"/>
      </params>
      <attribs/>
   </obj>
   <obj type="math/inv" sha="527f9ea38e810968a5d209b2913f846bcc5cbfea" uuid="565521d3699b36d8095aa1c79b9ad0046fb133ce" name="inv_1" x="210" y="126">
      <params/>
      <attribs/>
   </obj>
   <obj type="math/div 2" sha="2cb2778f41e0d462d7aed59480280459b497b573" uuid="f14884de1baf3e615e12ee162a96a013eca76789" name="div_1" x="280" y="126">
      <params/>
      <attribs/>
   </obj>
   <obj type="ctrl/dial p" sha="501c30e07dedf3d701e8d0b33c3c234908c3388e" uuid="cc5d2846c3d50e425f450c4b9851371b54f4d674" name="Noise Accent Amt" x="658" y="224">
      <params>
         <frac32.u.map name="value" onParent="true" value="12.5"/>
      </params>
      <attribs/>
   </obj>
   <comment type="patch/comment" x="224" y="238" text="These CV operations are for making different Accent/Ghost behaviours"/>
   <obj type="patch/inlet b" sha="17c8e188371661163bfa55cea9974eecb785fb06" uuid="3b0d3eacb5bb978cb05d1372aa2714d5a4790844" name="Trig In" x="14" y="252">
      <params/>
      <attribs/>
   </obj>
   <obj type="math/*" sha="ec71f518aa0e133062e5a9d4828d3739865c99f" uuid="922423f2db9f222aa3e5ba095778288c446da47a" name="*_5" x="392" y="252">
      <params/>
      <attribs/>
   </obj>
   <obj type="math/+" sha="49587c7bc7e1813d8a9a7a9be012580af00ea274" uuid="44553fdc8628c67ab535845ed1be304ad6c9553b" name="+_3" x="504" y="280">
      <params/>
      <attribs/>
   </obj>
   <obj type="patch/inlet b" sha="17c8e188371661163bfa55cea9974eecb785fb06" uuid="3b0d3eacb5bb978cb05d1372aa2714d5a4790844" name="Accent" x="14" y="294">
      <params/>
      <attribs/>
   </obj>
   <obj type="math/*" sha="ec71f518aa0e133062e5a9d4828d3739865c99f" uuid="922423f2db9f222aa3e5ba095778288c446da47a" name="*_6" x="392" y="322">
      <params/>
      <attribs/>
   </obj>
   <comment type="patch/comment" x="658" y="322" text="This envelope is for the pitched generator"/>
   <obj type="patch/inlet b" sha="17c8e188371661163bfa55cea9974eecb785fb06" uuid="3b0d3eacb5bb978cb05d1372aa2714d5a4790844" name="Ghost" x="14" y="336">
      <params/>
      <attribs/>
   </obj>
   <obj type="math/inv" sha="527f9ea38e810968a5d209b2913f846bcc5cbfea" uuid="565521d3699b36d8095aa1c79b9ad0046fb133ce" name="inv_2" x="168" y="336">
      <params/>
      <attribs/>
   </obj>
   <obj type="math/*c" sha="60143a29e35f452025e9edaa2dec6e660ecb9c6e" uuid="7d5ef61c3bcd571ee6bbd8437ef3612125dfb225" name="div_2" x="252" y="336">
      <params>
         <frac32.u.map name="amp" value="5.5"/>
      </params>
      <attribs/>
   </obj>
   <obj type="env/d m" sha="3f6e6c6081782177f0dc9dfe9e50a99b54fe41f6" uuid="85e82f54dfc28839d300cda777af8907ae2a28d0" name="Body Decay" x="658" y="336">
      <params>
         <frac32.s.map name="d" onParent="true" value="-23.0"/>
      </params>
      <attribs/>
   </obj>
   <obj type="ctrl/dial p" sha="501c30e07dedf3d701e8d0b33c3c234908c3388e" uuid="cc5d2846c3d50e425f450c4b9851371b54f4d674" name="Body Accent Amt" x="658" y="434">
      <params>
         <frac32.u.map name="value" onParent="true" value="12.5"/>
      </params>
      <attribs/>
   </obj>
   <comment type="patch/comment" x="658" y="574" text="This osc will provide the body of the snare"/>
   <obj type="osc/sine" sha="edec4a9d5f533ea748cd564ce8c69673dd78742f" uuid="6e094045cca76a9dbf7ebfa72e44e4700d2b3ba" name="Tune" x="658" y="588">
      <params>
         <frac32.s.map name="pitch" onParent="true" value="-21.709999561309814"/>
      </params>
      <attribs/>
   </obj>
   <comment type="patch/comment" x="1176" y="616" text="Crossfade between Body and Noise"/>
   <comment type="patch/comment" x="1064" y="630" text="Osc"/>
   <obj type="mix/xfade" sha="46677d62cd61f18b6996ffad67cd94b74cd98f2d" uuid="375dc91d218e96cdc9cbc7e92adb48f705ef701a" name="mix_1" x="1190" y="630">
      <params/>
      <attribs/>
   </obj>
   <obj type="gain/vca" sha="c904cdd24d65968df2f5796e107db3747dd691a6" uuid="a9f2dcd18043e2f47364e45cb8814f63c2a37c0d" name="vca_2" x="1064" y="644">
      <params/>
      <attribs/>
   </obj>
   <obj type="math/muls 2" sha="dbfe980a8cd01d08b914f66dca85134a68ea2a5a" uuid="c3083089dc169cd87133000659a02789dec5a151" name="muls_2" x="826" y="672">
      <params/>
      <attribs/>
   </obj>
   <obj type="math/*c" sha="60143a29e35f452025e9edaa2dec6e660ecb9c6e" uuid="7d5ef61c3bcd571ee6bbd8437ef3612125dfb225" name="Pitch Env Amt" x="658" y="700">
      <params>
         <frac32.u.map name="amp" onParent="true" value="64.0"/>
      </params>
      <attribs/>
   </obj>
   <comment type="patch/comment" x="1064" y="728" text="Noise"/>
   <obj type="gain/vca" sha="c904cdd24d65968df2f5796e107db3747dd691a6" uuid="a9f2dcd18043e2f47364e45cb8814f63c2a37c0d" name="vca_1" x="1064" y="742">
      <params/>
      <attribs/>
   </obj>
   <obj type="noise/pink oct" sha="7facef3f51b342f663be4fdfd288f1f893ef0866" uuid="972351c4c2b2b4e358c64a64d119b5ddfa074f8e" name="pink_1" x="826" y="756">
      <params/>
      <attribs>
         <combo attributeName="octaves" selection="6"/>
      </attribs>
   </obj>
   <obj type="env/d" sha="d9f7cfe1295d7bcc550714a18126d4f73c7c8411" uuid="190ae648e41832b41adbedb465317c18a010aefe" name="Pitch Decay" x="658" y="784">
      <params>
         <frac32.s.map name="d" onParent="true" value="-58.0"/>
      </params>
      <attribs/>
   </obj>
   <obj type="ctrl/dial p" sha="501c30e07dedf3d701e8d0b33c3c234908c3388e" uuid="cc5d2846c3d50e425f450c4b9851371b54f4d674" name="Drive" x="672" y="882">
      <params>
         <frac32.u.map name="value" onParent="true" value="0.0"/>
      </params>
      <attribs/>
   </obj>
   <obj type="ctrl/dial p" sha="501c30e07dedf3d701e8d0b33c3c234908c3388e" uuid="cc5d2846c3d50e425f450c4b9851371b54f4d674" name="Drive Accent Amt" x="672" y="966">
      <params>
         <frac32.u.map name="value" onParent="true" value="0.0"/>
      </params>
      <attribs/>
   </obj>
   <comment type="patch/comment" x="986" y="1146" text="Final drive/amp stage"/>
   <obj type="gain/vca" sha="c904cdd24d65968df2f5796e107db3747dd691a6" uuid="a9f2dcd18043e2f47364e45cb8814f63c2a37c0d" name="vca_4" x="1512" y="1162">
      <params/>
      <attribs/>
   </obj>
   <obj type="patch/outlet a" sha="9e7e04867e1d37837b0924c9bf18c44ac68602e6" uuid="abd8c5fd3b0524a6630f65cad6dc27f6c58e2a3e" name="Out" x="1666" y="1162">
      <params/>
      <attribs/>
   </obj>
   <obj type="math/*c" sha="60143a29e35f452025e9edaa2dec6e660ecb9c6e" uuid="7d5ef61c3bcd571ee6bbd8437ef3612125dfb225" name="*c_1" x="882" y="1176">
      <params>
         <frac32.u.map name="amp" value="39.5"/>
      </params>
      <attribs/>
   </obj>
   <obj type="math/+c" sha="d0aea6063c88e27c97acf08b33a056fec3e150f1" uuid="13eec32bd8ad57dd0bb18a02566cc0a117d320e3" name="+c_1" x="994" y="1176">
      <params>
         <frac32.u.map name="c" value="20.5"/>
      </params>
      <attribs/>
   </obj>
   <obj type="math/+" sha="49587c7bc7e1813d8a9a7a9be012580af00ea274" uuid="44553fdc8628c67ab535845ed1be304ad6c9553b" name="+_1" x="1148" y="1176">
      <params/>
      <attribs/>
   </obj>
   <obj type="gain/vca" sha="c904cdd24d65968df2f5796e107db3747dd691a6" uuid="a9f2dcd18043e2f47364e45cb8814f63c2a37c0d" name="vca_3" x="1274" y="1176">
      <params/>
      <attribs/>
   </obj>
   <obj type="math/muls 16" sha="8981527339bf34a9e864af533a739f4b4d5c9dda" uuid="c72d593cdf22887ca55f6da46ea788d091a21d19" name="muls_1" x="1344" y="1176">
      <params/>
      <attribs/>
   </obj>
   <obj type="dist/soft" sha="74960c930c4b6a5c630156778f889d4de48dbdbf" uuid="e680d76a805e4866027cdf654c7efd8b2e54622" name="soft_1" x="1442" y="1176">
      <params/>
      <attribs/>
   </obj>
   <obj type="math/div 4" sha="96a7c0765a638d8fc2cda60dcb59896abb613995" uuid="507955275561b256e540f7205386d31545a2828f" name="div_3" x="1260" y="1246">
      <params/>
      <attribs/>
   </obj>
   <obj type="mux/mux 2" sha="73ba55fbf61b80b78dd5cb4f0c4bd5cbbdd54ea0" uuid="3bcb8a666381ed18b8962eda50b1aa679136f618" name="mux_1" x="994" y="1274">
      <params/>
      <attribs/>
   </obj>
   <obj type="math/*c" sha="60143a29e35f452025e9edaa2dec6e660ecb9c6e" uuid="7d5ef61c3bcd571ee6bbd8437ef3612125dfb225" name="*c_2" x="644" y="1288">
      <params>
         <frac32.u.map name="amp" value="41.5"/>
      </params>
      <attribs/>
   </obj>
   <obj type="math/*" sha="ec71f518aa0e133062e5a9d4828d3739865c99f" uuid="922423f2db9f222aa3e5ba095778288c446da47a" name="*_3" x="784" y="1302">
      <params/>
      <attribs/>
   </obj>
   <obj type="mux/mux 2" sha="73ba55fbf61b80b78dd5cb4f0c4bd5cbbdd54ea0" uuid="3bcb8a666381ed18b8962eda50b1aa679136f618" name="mux_2" x="1372" y="1316">
      <params/>
      <attribs/>
   </obj>
   <obj type="math/div 16" sha="153e55f554960498908693663e38bb762eb480d8" uuid="19cf8d3358046cb69c1139e51a3e1555742492e0" name="div_3 3" x="882" y="1372">
      <params/>
      <attribs/>
   </obj>
   <nets>
      <net>
         <source obj="pink_1" outlet="out"/>
         <dest obj="vca_1" inlet="a"/>
      </net>
      <net>
         <source obj="vca_2" outlet="o"/>
         <dest obj="mix_1" inlet="i1"/>
      </net>
      <net>
         <source obj="Trig In" outlet="inlet"/>
         <dest obj="Pitch Decay" inlet="trig"/>
         <dest obj="Noise Decay" inlet="trig"/>
         <dest obj="latch_1" inlet="trig"/>
         <dest obj="Body Decay" inlet="trig"/>
         <dest obj="latch_2" inlet="trig"/>
      </net>
      <net>
         <source obj="Pitch Decay" outlet="env"/>
         <dest obj="Pitch Env Amt" inlet="in"/>
      </net>
      <net>
         <source obj="Pitch Env Amt" outlet="out"/>
         <dest obj="Tune" inlet="pitch"/>
      </net>
      <net>
         <source obj="muls_1" outlet="out"/>
         <dest obj="soft_1" inlet="in"/>
      </net>
      <net>
         <source obj="vca_1" outlet="o"/>
         <dest obj="mix_1" inlet="i2"/>
      </net>
      <net>
         <source obj="mix_1" outlet="o"/>
         <dest obj="vca_3" inlet="a"/>
      </net>
      <net>
         <source obj="Body/Noise Rate" outlet="out"/>
         <dest obj="mix_1" inlet="c"/>
      </net>
      <net>
         <source obj="Noise Decay" outlet="env"/>
         <dest obj="vca_1" inlet="v"/>
      </net>
      <net>
         <source obj="*_1" outlet="result"/>
         <dest obj="+_2" inlet="in1"/>
      </net>
      <net>
         <source obj="Accent" outlet="inlet"/>
         <dest obj="latch_1" inlet="i"/>
      </net>
      <net>
         <source obj="Noise Accent Amt" outlet="out"/>
         <dest obj="*_1" inlet="b"/>
         <dest obj="inv_1" inlet="in"/>
      </net>
      <net>
         <source obj="latch_1" outlet="o"/>
         <dest obj="*_1" inlet="a"/>
         <dest obj="*_3" inlet="b"/>
         <dest obj="*_5" inlet="a"/>
      </net>
      <net>
         <source obj="Body Decay" outlet="env"/>
         <dest obj="vca_2" inlet="v"/>
      </net>
      <net>
         <source obj="soft_1" outlet="out"/>
         <dest obj="vca_4" inlet="a"/>
      </net>
      <net>
         <source obj="vca_3" outlet="o"/>
         <dest obj="muls_1" inlet="in"/>
      </net>
      <net>
         <source obj="Drive" outlet="out"/>
         <dest obj="*c_1" inlet="in"/>
      </net>
      <net>
         <source obj="+c_1" outlet="out"/>
         <dest obj="+_1" inlet="in1"/>
      </net>
      <net>
         <source obj="*c_1" outlet="out"/>
         <dest obj="+c_1" inlet="in"/>
      </net>
      <net>
         <source obj="Drive Accent Amt" outlet="out"/>
         <dest obj="*c_2" inlet="in"/>
      </net>
      <net>
         <source obj="*c_2" outlet="out"/>
         <dest obj="*_3" inlet="a"/>
      </net>
      <net>
         <source obj="*_3" outlet="result"/>
         <dest obj="mux_1" inlet="i1"/>
         <dest obj="div_3 3" inlet="in"/>
      </net>
      <net>
         <source obj="+_1" outlet="out"/>
         <dest obj="vca_3" inlet="v"/>
         <dest obj="mux_2" inlet="i1"/>
         <dest obj="div_3" inlet="in"/>
      </net>
      <net>
         <source obj="vca_4" outlet="o"/>
         <dest obj="Out" inlet="outlet"/>
      </net>
      <net>
         <source obj="Ghost" outlet="inlet"/>
         <dest obj="latch_2" inlet="i"/>
      </net>
      <net>
         <source obj="inv_1" outlet="out"/>
         <dest obj="div_1" inlet="in"/>
      </net>
      <net>
         <source obj="*_4" outlet="result"/>
         <dest obj="+_2" inlet="in2"/>
      </net>
      <net>
         <source obj="+_2" outlet="out"/>
         <dest obj="Noise Decay" inlet="d"/>
      </net>
      <net>
         <source obj="div_1" outlet="out"/>
         <dest obj="*_4" inlet="b"/>
      </net>
      <net>
         <source obj="*_5" outlet="result"/>
         <dest obj="+_3" inlet="in1"/>
      </net>
      <net>
         <source obj="inv_2" outlet="out"/>
         <dest obj="div_2" inlet="in"/>
      </net>
      <net>
         <source obj="*_6" outlet="result"/>
         <dest obj="+_3" inlet="in2"/>
      </net>
      <net>
         <source obj="+_3" outlet="out"/>
         <dest obj="Body Decay" inlet="d"/>
      </net>
      <net>
         <source obj="div_2" outlet="out"/>
         <dest obj="*_6" inlet="b"/>
      </net>
      <net>
         <source obj="Body Accent Amt" outlet="out"/>
         <dest obj="inv_2" inlet="in"/>
         <dest obj="*_5" inlet="b"/>
      </net>
      <net>
         <source obj="latch_2" outlet="o"/>
         <dest obj="*_4" inlet="a"/>
         <dest obj="*_6" inlet="a"/>
         <dest obj="mux_1" inlet="s"/>
         <dest obj="mux_2" inlet="s"/>
      </net>
      <net>
         <source obj="mux_1" outlet="o"/>
         <dest obj="+_1" inlet="in2"/>
      </net>
      <net>
         <source obj="div_3 3" outlet="out"/>
         <dest obj="mux_1" inlet="i2"/>
      </net>
      <net>
         <source obj="mux_2" outlet="o"/>
         <dest obj="vca_4" inlet="v"/>
      </net>
      <net>
         <source obj="div_3" outlet="out"/>
         <dest obj="mux_2" inlet="i2"/>
      </net>
      <net>
         <source obj="Tune" outlet="wave"/>
         <dest obj="muls_2" inlet="in"/>
      </net>
      <net>
         <source obj="muls_2" outlet="out"/>
         <dest obj="vca_2" inlet="a"/>
      </net>
   </nets>
   <settings>
      <subpatchmode>no</subpatchmode>
      <MidiChannel>1</MidiChannel>
      <NPresets>8</NPresets>
      <NPresetEntries>32</NPresetEntries>
      <NModulationSources>8</NModulationSources>
      <NModulationTargetsPerSource>8</NModulationTargetsPerSource>
      <Author>Sputnki</Author>
   </settings>
   <notes><![CDATA[Drum synthesizer, especially suited for snares!
]]></notes>
   <windowPos>
      <x>0</x>
      <y>-4</y>
      <width>1366</width>
      <height>772</height>
   </windowPos>
</patch-1.0>