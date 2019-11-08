<patch-1.0 appVersion="1.0.12">
   <obj type="gpio/in/digital" uuid="f59f139e8da912742832dc541157f20f30b7ac1b" name="digital_2" x="14" y="14">
      <params/>
      <attribs>
         <combo attributeName="pad" selection="PB7"/>
         <combo attributeName="mode" selection="pulldown"/>
      </attribs>
   </obj>
   <obj type="patch/bankindex" uuid="943bd281-10a5-4994-9876-11a3b1fbde8a" name="bankindex_1" x="112" y="14">
      <params/>
      <attribs/>
   </obj>
   <obj type="math/+1" uuid="13c1a4574bb81783beb8839e81782b9a34e3fc17" name="+1_1" x="252" y="70">
      <params/>
      <attribs/>
   </obj>
   <obj type="patch/load i" uuid="b79124f5b7d1e8b39e187677ddab6260ce8c60a3" name="load_2" x="322" y="70">
      <params/>
      <attribs/>
   </obj>
   <obj type="gpio/in/digital" uuid="f59f139e8da912742832dc541157f20f30b7ac1b" name="digital_1" x="14" y="98">
      <params/>
      <attribs>
         <combo attributeName="pad" selection="PB6"/>
         <combo attributeName="mode" selection="pulldown"/>
      </attribs>
   </obj>
   <obj type="math/-1" uuid="5fd46bab471bb6509ae83de702dea72933683a98" name="-1_1" x="252" y="126">
      <params/>
      <attribs/>
   </obj>
   <obj type="patch/load i" uuid="b79124f5b7d1e8b39e187677ddab6260ce8c60a3" name="load_3" x="322" y="126">
      <params/>
      <attribs/>
   </obj>
   <obj type="math/+1" uuid="13c1a4574bb81783beb8839e81782b9a34e3fc17" name="+1_2" x="238" y="196">
      <params/>
      <attribs/>
   </obj>
   <obj type="math/&gt;&gt;" uuid="d883beaf50b7aae4803ed9941e8f123e87e1e3aa" name="&gt;&gt;_1" x="308" y="196">
      <params/>
      <attribs>
         <spinner attributeName="shift" value="1"/>
      </attribs>
   </obj>
   <obj type="midi/out/cc" uuid="7ca9b2c1b4339022c491a8afd70353c17d765584" name="cc_1" x="406" y="196">
      <params/>
      <attribs>
         <combo attributeName="device" selection="usb host port 1"/>
         <spinner attributeName="channel" value="1"/>
         <spinner attributeName="cc" value="71"/>
      </attribs>
   </obj>
   <obj type="ctrl/toggle" uuid="42b8134fa729d54bfc8d62d6ef3fa99498c1de99" name="toggle_1" x="462" y="322">
      <params>
         <bool32.tgl name="b" value="1"/>
      </params>
      <attribs/>
   </obj>
   <nets>
      <net>
         <source obj="bankindex_1" outlet="index"/>
         <dest obj="+1_1" inlet="a"/>
         <dest obj="-1_1" inlet="a"/>
         <dest obj="+1_2" inlet="a"/>
      </net>
      <net>
         <source obj="+1_1" outlet="result"/>
         <dest obj="load_2" inlet="i"/>
      </net>
      <net>
         <source obj="-1_1" outlet="result"/>
         <dest obj="load_3" inlet="i"/>
      </net>
      <net>
         <source obj="digital_2" outlet="out"/>
         <dest obj="load_2" inlet="trig"/>
      </net>
      <net>
         <source obj="digital_1" outlet="out"/>
         <dest obj="load_3" inlet="trig"/>
      </net>
      <net>
         <source obj="toggle_1" outlet="o"/>
         <dest obj="cc_1" inlet="trig"/>
      </net>
      <net>
         <source obj="&gt;&gt;_1" outlet="result"/>
         <dest obj="cc_1" inlet="v"/>
      </net>
      <net>
         <source obj="+1_2" outlet="result"/>
         <dest obj="&gt;&gt;_1" inlet="a"/>
      </net>
   </nets>
   <settings>
      <subpatchmode>no</subpatchmode>
   </settings>
   <notes><![CDATA[]]></notes>
   <windowPos>
      <x>481</x>
      <y>87</y>
      <width>565</width>
      <height>484</height>
   </windowPos>
</patch-1.0>