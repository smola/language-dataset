-----------------------------------------------------------------------
--  etherscope-analyzer-ethernet -- Ethernet packet analyzer
--  Copyright (C) 2016 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------
with Net.Headers;
with EtherScope.Stats;

--  === Ethernet Packet Analyzer ===
--  The Ethernet packet analyzer collects the different source Ethernet addresses
--  and different Ethernet types seen in the Ethernet header.  The information
--  is collected in two different tables:
--
--  * A device table keeps a list of devices seen on the network.
--  * A protocol table keeps a list of Ethernet protocols.
--
--  Both tables have fixed sizes to avoid dynamic memory allocation.
--  New entries are filled in the tables until all the entries are used.
--  The last table entry is used to collect everything that does not fit.
--
--  The Ethernet packet analyzer computes a device index based on the Ethernet
--  source address.  This device index can be used by other protocol analyzers
--  to easily populate their device analysis.
package EtherScope.Analyzer.Ethernet is

   subtype Device_Index is EtherScope.Stats.Device_Index;

   --  Collect per source Ethernet statistics.
   type Device_Stats is record
      Mac      : Net.Ether_Addr;
      Stats    : EtherScope.Stats.Statistics;
   end record;

   type Device_Table_Stats is array (Device_Index) of Device_Stats;

   --  Collect per Ethernet protocol type statistics.
   type Protocol_Stats is record
      Proto    : Net.Uint16 := 0;
      Stats    : EtherScope.Stats.Statistics;
   end record;

   type Protocol_Table_Stats is array (Device_Index) of Protocol_Stats;

   --  Ethernet packet analysis.
   type Analysis is record
      Devices   : Device_Table_Stats;
      Dev_Count : EtherScope.Stats.Device_Count := 0;
      Protocols : Protocol_Table_Stats;
      Pro_Count : EtherScope.Stats.Device_Count := 0;
      Global    : EtherScope.Stats.Statistics;
   end record;

   --  Analyze the packet and update the analysis.
   procedure Analyze (Ether    : in Net.Headers.Ether_Header_Access;
                      Length   : in Net.Uint16;
                      Result   : in out Analysis;
                      Samples  : in out EtherScope.Stats.Graph_Samples;
                      Device   : out Device_Index);

   --  Compute the bandwidth utilization for different devices and protocols.
   procedure Update_Rates (Current  : in out Analysis;
                           Previous : in out Analysis;
                           Dt       : in Positive);

end EtherScope.Analyzer.Ethernet;
