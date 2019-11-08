filedesc://2-metadata-1.arc 0.0.0.0 20130522083953 text/plain 77
1 0 InternetArchive
URL IP-address Archive-date Content-type Archive-length

metadata://netarkivet.dk/crawl/setup/duplicatereductionjobs?majorversion=1&minorversion=0&harvestid=1&harvestnum=1&jobid=2 172.16.13.127 20130522083953 text/plain 1
1
metadata://netarkivet.dk/crawl/setup/crawl-manifest.txt?heritrixVersion=1.14.4&harvestid=1&jobid=2 172.16.13.127 20130522082955 text/plain 1157
L+ /home/onbpre/work/nas/deploy/onbpre/harvester_high/2_1369211317954/logs/crawl.log
L+ /home/onbpre/work/nas/deploy/onbpre/harvester_high/2_1369211317954/logs/runtime-errors.log
L+ /home/onbpre/work/nas/deploy/onbpre/harvester_high/2_1369211317954/logs/local-errors.log
L+ /home/onbpre/work/nas/deploy/onbpre/harvester_high/2_1369211317954/logs/uri-errors.log
L+ /home/onbpre/work/nas/deploy/onbpre/harvester_high/2_1369211317954/logs/progress-statistics.log
C+ /home/onbpre/work/nas/deploy/onbpre/harvester_high/2_1369211317954/order.xml
C+ /home/onbpre/work/nas/deploy/onbpre/harvester_high/2_1369211317954/seeds.txt
R+ /home/onbpre/work/nas/deploy/onbpre/harvester_high/2_1369211317954/hosts-report.txt
R+ /home/onbpre/work/nas/deploy/onbpre/harvester_high/2_1369211317954/mimetype-report.txt
R+ /home/onbpre/work/nas/deploy/onbpre/harvester_high/2_1369211317954/responsecode-report.txt
R+ /home/onbpre/work/nas/deploy/onbpre/harvester_high/2_1369211317954/seeds-report.txt
R+ /home/onbpre/work/nas/deploy/onbpre/harvester_high/2_1369211317954/crawl-report.txt
R+ /home/onbpre/work/nas/deploy/onbpre/harvester_high/2_1369211317954/processors-report.txt

metadata://netarkivet.dk/crawl/setup/harvestInfo.xml?heritrixVersion=1.14.4&harvestid=1&jobid=2 172.16.13.127 20130522082838 text/xml 591
<?xml version="1.0" encoding="UTF-8"?>

<harvestInfo>
  <version>0.4</version>
  <jobId>2</jobId>
  <priority>HIGHPRIORITY</priority>
  <harvestNum>1</harvestNum>
  <origHarvestDefinitionID>1</origHarvestDefinitionID>
  <maxBytesPerDomain>1000000000</maxBytesPerDomain>
  <maxObjectsPerDomain>-1</maxObjectsPerDomain>
  <orderXMLName>default_orderxml</orderXMLName>
  <origHarvestDefinitionName>fuetest</origHarvestDefinitionName>
  <scheduleName>einmal</scheduleName>
  <harvestFilenamePrefix>2-1</harvestFilenamePrefix>
  <jobSubmitDate>2013-05-22T08:28:37Z</jobSubmitDate>
</harvestInfo>

metadata://netarkivet.dk/crawl/setup/order.xml?heritrixVersion=1.14.4&harvestid=1&jobid=2 172.16.13.127 20130522082940 text/xml 22808
<?xml version="1.0" encoding="UTF-8"?>

<crawl-order xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="heritrix_settings.xsd">  
  <meta> 
    <name>default_orderxml</name>  
    <description>Default Profile</description>  
    <operator>Admin</operator>  
    <organization/>  
    <audience/>  
    <date>20080118111217</date> 
  </meta>  
  <controller> 
    <string name="settings-directory">settings</string>  
    <string name="disk-path">/home/onbpre/work/nas/deploy/onbpre/harvester_high/2_1369211317954</string>  
    <string name="logs-path">logs</string>  
    <string name="checkpoints-path">checkpoints</string>  
    <string name="state-path">state</string>  
    <string name="scratch-path">scratch</string>  
    <long name="max-bytes-download">0</long>  
    <long name="max-document-download">0</long>  
    <long name="max-time-sec">0</long>  
    <integer name="max-toe-threads">50</integer>  
    <integer name="recorder-out-buffer-bytes">4096</integer>  
    <integer name="recorder-in-buffer-bytes">65536</integer>  
    <integer name="bdb-cache-percent">0</integer>  
    <!-- DecidingScope migrated from DomainScope -->  
    <newObject name="scope" class="org.archive.crawler.deciderules.DecidingScope"> 
      <boolean name="enabled">true</boolean>  
      <string name="seedsfile">/home/onbpre/work/nas/deploy/onbpre/harvester_high/2_1369211317954/seeds.txt</string>  
      <boolean name="reread-seeds-on-config">true</boolean>  
      <!-- DecideRuleSequence. Multiple DecideRules applied in order with last non-PASS the resulting decision -->  
      <newObject name="decide-rules" class="org.archive.crawler.deciderules.DecideRuleSequence"> 
        <map name="rules"> 
          <newObject name="rejectByDefault" class="org.archive.crawler.deciderules.RejectDecideRule"/>  
          <newObject name="acceptURIFromSeedDomains" class="dk.netarkivet.harvester.harvesting.OnNSDomainsDecideRule"> 
            <string name="decision">ACCEPT</string>  
            <string name="surts-source-file"/>  
            <boolean name="seeds-as-surt-prefixes">true</boolean>  
            <string name="surts-dump-file"/>  
            <boolean name="also-check-via">false</boolean>  
            <boolean name="rebuild-on-reconfig">true</boolean> 
          </newObject>  
          <newObject name="rejectIfTooManyHops" class="org.archive.crawler.deciderules.TooManyHopsDecideRule"> 
            <integer name="max-hops">25</integer> 
          </newObject>  
          <newObject name="rejectIfPathological" class="org.archive.crawler.deciderules.PathologicalPathDecideRule"> 
            <integer name="max-repetitions">3</integer> 
          </newObject>  
          <newObject name="acceptIfTranscluded" class="org.archive.crawler.deciderules.TransclusionDecideRule"> 
            <integer name="max-trans-hops">3</integer>  
            <integer name="max-speculative-hops">1</integer> 
          </newObject>  
          <newObject name="pathdepthfilter" class="org.archive.crawler.deciderules.TooManyPathSegmentsDecideRule"> 
            <integer name="max-path-depth">20</integer> 
          </newObject>  
          <newObject name="global_crawlertraps" class="org.archive.crawler.deciderules.MatchesListRegExpDecideRule"> 
            <string name="decision">REJECT</string>  
            <string name="list-logic">OR</string>  
            <stringList name="regexp-list"> 
              <string>.*core\.UserAdmin.*core\.UserLogin.*</string>  
              <string>.*core\.UserAdmin.*register\.UserSelfRegistration.*</string>  
              <string>.*\/w\/index\.php\?title=Speci[ae]l:Recentchanges.*</string>  
              <string>.*act=calendar&amp;cal_id=.*</string>  
              <string>.*advCalendar_pi.*</string>  
              <string>.*cal\.asp\?date=.*</string>  
              <string>.*cal\.asp\?view=monthly&amp;date=.*</string>  
              <string>.*cal\.asp\?view=weekly&amp;date=.*</string>  
              <string>.*cal\.asp\?view=yearly&amp;date=.*</string>  
              <string>.*cal\.asp\?view=yearly&amp;year=.*</string>  
              <string>.*cal\/cal_day\.php\?op=day&amp;date=.*</string>  
              <string>.*cal\/cal_week\.php\?op=week&amp;date=.*</string>  
              <string>.*cal\/calendar\.php\?op=cal&amp;month=.*</string>  
              <string>.*cal\/yearcal\.php\?op=yearcal&amp;ycyear=.*</string>  
              <string>.*calendar\.asp\?calmonth=.*</string>  
              <string>.*calendar\.asp\?qMonth=.*</string>  
              <string>.*calendar\.php\?sid=.*</string>  
              <string>.*calendar\.php\?start=.*</string>  
              <string>.*calendar\.php\?Y=.*</string>  
              <string>.*calendar\/\?CLmDemo_horizontal=.*</string>  
              <string>.*calendar_menu\/calendar\.php\?.*</string>  
              <string>.*calendar_scheduler\.php\?d=.*</string>  
              <string>.*calendar_year\.asp\?qYear=.*</string>  
              <string>.*calendarix\/calendar\.php\?op=.*</string>  
              <string>.*calendarix\/yearcal\.php\?op=.*</string>  
              <string>.*calender\/default\.asp\?month=.*</string>  
              <string>.*Default\.asp\?month=.*</string>  
              <string>.*events\.asp\?cat=0&amp;mDate=.*</string>  
              <string>.*events\.asp\?cat=1&amp;mDate=.*</string>  
              <string>.*events\.asp\?MONTH=.*</string>  
              <string>.*events\.asp\?month=.*</string>  
              <string>.*index\.php\?iDate=.*</string>  
              <string>.*index\.php\?module=PostCalendar&amp;func=view.*</string>  
              <string>.*index\.php\?option=com_events&amp;task=view.*</string>  
              <string>.*index\.php\?option=com_events&amp;task=view_day&amp;year=.*</string>  
              <string>.*index\.php\?option=com_events&amp;task=view_detail&amp;year=.*</string>  
              <string>.*index\.php\?option=com_events&amp;task=view_month&amp;year=.*</string>  
              <string>.*index\.php\?option=com_events&amp;task=view_week&amp;year=.*</string>  
              <string>.*index\.php\?option=com_events&amp;task=view_year&amp;year=.*</string>  
              <string>.*index\.php\?option=com_extcalendar&amp;Itemid.*</string>  
              <string>.*modules\.php\?name=Calendar&amp;op=modload&amp;file=index.*</string>  
              <string>.*modules\.php\?name=vwar&amp;file=calendar&amp;action=list&amp;month=.*</string>  
              <string>.*modules\.php\?name=vwar&amp;file=calendar.*</string>  
              <string>.*modules\.php\?name=vWar&amp;mod=calendar.*</string>  
              <string>.*modules\/piCal\/index\.php\?caldate=.*</string>  
              <string>.*modules\/piCal\/index\.php\?cid=.*</string>  
              <string>.*option,com_events\/task,view_day\/year.*</string>  
              <string>.*option,com_events\/task,view_month\/year.*</string>  
              <string>.*option,com_extcalendar\/Itemid.*</string>  
              <string>.*task,view_month\/year.*</string>  
              <string>.*shopping_cart\.php.*</string>  
              <string>.*action.add_product.*</string>  
              <string>.*action.remove_product.*</string>  
              <string>.*action.buy_now.*</string>  
              <string>.*checkout_payment\.php.*</string>  
              <string>.*login.*login.*login.*login.*</string>  
              <string>.*homepage_calendar\.asp.*</string>  
              <string>.*MediaWiki.*Movearticle.*</string>  
              <string>.*index\.php.*action=edit.*</string>  
              <string>.*comcast\.net.*othastar.*</string>  
              <string>.*Login.*Login.*Login.*</string>  
              <string>.*redir.*redir.*redir.*</string>  
              <string>.*bookingsystemtime\.asp\?dato=.*</string>  
              <string>.*bookingsystem\.asp\?date=.*</string>  
              <string>.*cart\.asp\?mode=add.*</string>  
              <string>.*\/photo.*\/photo.*\/photo.*</string>  
              <string>.*\/skins.*\/skins.*\/skins.*</string>  
              <string>.*\/scripts.*\/scripts.*\/scripts.*</string>  
              <string>.*\/styles.*\/styles.*\/styles.*</string>  
              <string>.*\/coppermine\/login\.php\?referer=.*</string>  
              <string>.*\/images.*\/images.*\/images.*</string>  
              <string>.*\/stories.*\/stories.*\/stories.*</string> 
            </stringList> 
          </newObject>  
          <newObject name="onb.ac.at" class="org.archive.crawler.deciderules.MatchesListRegExpDecideRule"> 
            <string name="decision">REJECT</string>  
            <string name="list-logic">OR</string>  
            <stringList name="regexp-list"> 
              <string/> 
            </stringList> 
          </newObject> 
        </map>  
        <!-- end rules --> 
      </newObject>  
      <!-- end decide-rules --> 
    </newObject>  
    <!-- End DecidingScope -->  
    <map name="http-headers"> 
      <string name="user-agent">Mozilla/5.0 (compatible; heritrix/1.12.1 +http://my_website.com/my_infopage.html)</string>  
      <string name="from">my_email@my_website.com</string> 
    </map>  
    <newObject name="robots-honoring-policy" class="org.archive.crawler.datamodel.RobotsHonoringPolicy"> 
      <string name="type">ignore</string>  
      <boolean name="masquerade">false</boolean>  
      <text name="custom-robots"/>  
      <stringList name="user-agents"/> 
    </newObject>  
    <newObject name="frontier" class="org.archive.crawler.frontier.BdbFrontier"> 
      <float name="delay-factor">1.0</float>  
      <integer name="max-delay-ms">1000</integer>  
      <integer name="min-delay-ms">300</integer>  
      <integer name="max-retries">5</integer>  
      <long name="retry-delay-seconds">300</long>  
      <integer name="preference-embed-hops">1</integer>  
      <integer name="total-bandwidth-usage-KB-sec">1500</integer>  
      <integer name="max-per-host-bandwidth-usage-KB-sec">500</integer>  
      <string name="queue-assignment-policy">dk.netarkivet.harvester.harvesting.DomainnameQueueAssignmentPolicy</string>  
      <string name="force-queue-assignment"/>  
      <boolean name="pause-at-start">false</boolean>  
      <boolean name="pause-at-finish">false</boolean>  
      <boolean name="source-tag-seeds">false</boolean>  
      <boolean name="recovery-log-enabled">false</boolean>  
      <boolean name="hold-queues">true</boolean>  
      <integer name="balance-replenish-amount">3000</integer>  
      <integer name="error-penalty-amount">100</integer>  
      <long name="queue-total-budget">-1</long>  
      <string name="cost-policy">org.archive.crawler.frontier.UnitCostAssignmentPolicy</string>  
      <long name="snooze-deactivate-ms">300000</long>  
      <integer name="target-ready-backlog">50</integer>  
      <string name="uri-included-structure">org.archive.crawler.util.BdbUriUniqFilter</string>  
      <boolean name="dump-pending-at-close">false</boolean> 
    </newObject>  
    <map name="uri-canonicalization-rules"> 
      <newObject name="Lowercase" class="org.archive.crawler.url.canonicalize.LowercaseRule"> 
        <boolean name="enabled">true</boolean> 
      </newObject>  
      <newObject name="Userinfo" class="org.archive.crawler.url.canonicalize.StripUserinfoRule"> 
        <boolean name="enabled">true</boolean> 
      </newObject>  
      <newObject name="WWW" class="org.archive.crawler.url.canonicalize.StripWWWRule"> 
        <boolean name="enabled">false</boolean> 
      </newObject>  
      <newObject name="SessionIDs" class="org.archive.crawler.url.canonicalize.StripSessionIDs"> 
        <boolean name="enabled">true</boolean> 
      </newObject>  
      <newObject name="QueryStrPrefix" class="org.archive.crawler.url.canonicalize.FixupQueryStr"> 
        <boolean name="enabled">true</boolean> 
      </newObject> 
    </map>  
    <!-- Heritrix pre-fetch processors -->  
    <map name="pre-fetch-processors"> 
      <newObject name="QuotaEnforcer" class="org.archive.crawler.prefetch.QuotaEnforcer"> 
        <boolean name="force-retire">false</boolean>  
        <boolean name="enabled">true</boolean>  
        <newObject name="QuotaEnforcer#decide-rules" class="org.archive.crawler.deciderules.DecideRuleSequence"> 
          <map name="rules"/> 
        </newObject>  
        <long name="server-max-fetch-successes">-1</long>  
        <long name="server-max-success-kb">-1</long>  
        <long name="server-max-fetch-responses">-1</long>  
        <long name="server-max-all-kb">-1</long>  
        <long name="host-max-fetch-successes">-1</long>  
        <long name="host-max-success-kb">-1</long>  
        <long name="host-max-fetch-responses">-1</long>  
        <long name="host-max-all-kb">-1</long>  
        <long name="group-max-fetch-successes">-1</long>  
        <long name="group-max-success-kb">-1</long>  
        <long name="group-max-fetch-responses">-1</long>  
        <long name="group-max-all-kb">976563</long> 
      </newObject>  
      <newObject name="Preselector" class="org.archive.crawler.prefetch.Preselector"> 
        <boolean name="enabled">true</boolean>  
        <newObject name="Preselector#decide-rules" class="org.archive.crawler.deciderules.DecideRuleSequence"> 
          <map name="rules"/> 
        </newObject>  
        <boolean name="override-logger">false</boolean>  
        <boolean name="recheck-scope">true</boolean>  
        <boolean name="block-all">false</boolean>  
        <string name="block-by-regexp"/>  
        <string name="allow-by-regexp"/> 
      </newObject>  
      <newObject name="Preprocessor" class="org.archive.crawler.prefetch.PreconditionEnforcer"> 
        <boolean name="enabled">true</boolean>  
        <newObject name="Preprocessor#decide-rules" class="org.archive.crawler.deciderules.DecideRuleSequence"> 
          <map name="rules"/> 
        </newObject>  
        <integer name="ip-validity-duration-seconds">21600</integer>  
        <integer name="robot-validity-duration-seconds">86400</integer>  
        <boolean name="calculate-robots-only">false</boolean> 
      </newObject> 
    </map>  
    <!--End of Heritrix pre-fetch processors -->  
    <!-- Heritrix fetch processors -->  
    <map name="fetch-processors"> 
      <newObject name="DNS" class="org.archive.crawler.fetcher.FetchDNS"> 
        <boolean name="enabled">true</boolean>  
        <newObject name="DNS#decide-rules" class="org.archive.crawler.deciderules.DecideRuleSequence"> 
          <map name="rules"/> 
        </newObject>  
        <boolean name="accept-non-dns-resolves">false</boolean>  
        <boolean name="digest-content">true</boolean>  
        <string name="digest-algorithm">sha1</string> 
      </newObject>  
      <newObject name="HTTP" class="org.archive.crawler.fetcher.FetchHTTP"> 
        <boolean name="enabled">true</boolean>  
        <newObject name="HTTP#decide-rules" class="org.archive.crawler.deciderules.DecideRuleSequence"> 
          <map name="rules"/> 
        </newObject>  
        <newObject name="midfetch-decide-rules" class="org.archive.crawler.deciderules.DecideRuleSequence"> 
          <map name="rules"/> 
        </newObject>  
        <integer name="timeout-seconds">1200</integer>  
        <integer name="sotimeout-ms">20000</integer>  
        <integer name="fetch-bandwidth">0</integer>  
        <long name="max-length-bytes">0</long>  
        <boolean name="ignore-cookies">false</boolean>  
        <boolean name="use-bdb-for-cookies">true</boolean>  
        <string name="load-cookies-from-file"/>  
        <string name="save-cookies-to-file"/>  
        <string name="trust-level">open</string>  
        <stringList name="accept-headers"/>  
        <string name="http-proxy-host"/>  
        <string name="http-proxy-port"/>  
        <string name="default-encoding">ISO-8859-1</string>  
        <boolean name="digest-content">true</boolean>  
        <string name="digest-algorithm">sha1</string>  
        <boolean name="send-if-modified-since">true</boolean>  
        <boolean name="send-if-none-match">true</boolean>  
        <boolean name="send-connection-close">true</boolean>  
        <boolean name="send-referer">true</boolean>  
        <boolean name="send-range">false</boolean>  
        <string name="http-bind-address"/> 
      </newObject> 
    </map>  
    <!-- end of Heritrix Fetch processors -->  
    <!-- Heritrix extract processors -->  
    <map name="extract-processors"> 
      <newObject name="ExtractorHTTP" class="org.archive.crawler.extractor.ExtractorHTTP"> 
        <boolean name="enabled">true</boolean>  
        <newObject name="ExtractorHTTP#decide-rules" class="org.archive.crawler.deciderules.DecideRuleSequence"> 
          <map name="rules"/> 
        </newObject> 
      </newObject>  
      <newObject name="ExtractorHTML" class="org.archive.crawler.extractor.ExtractorHTML"> 
        <boolean name="enabled">true</boolean>  
        <newObject name="ExtractorHTML#decide-rules" class="org.archive.crawler.deciderules.DecideRuleSequence"> 
          <map name="rules"/> 
        </newObject>  
        <boolean name="extract-javascript">true</boolean>  
        <boolean name="treat-frames-as-embed-links">true</boolean>  
        <boolean name="ignore-form-action-urls">true</boolean>  
        <boolean name="extract-value-attributes">true</boolean>  
        <boolean name="ignore-unexpected-html">true</boolean> 
      </newObject>  
      <newObject name="ExtractorCSS" class="org.archive.crawler.extractor.ExtractorCSS"> 
        <boolean name="enabled">true</boolean>  
        <newObject name="ExtractorCSS#decide-rules" class="org.archive.crawler.deciderules.DecideRuleSequence"> 
          <map name="rules"/> 
        </newObject> 
      </newObject>  
      <newObject name="ExtractorJS" class="org.archive.crawler.extractor.ExtractorJS"> 
        <boolean name="enabled">true</boolean>  
        <newObject name="ExtractorJS#decide-rules" class="org.archive.crawler.deciderules.DecideRuleSequence"> 
          <map name="rules"/> 
        </newObject> 
      </newObject>  
      <newObject name="ExtractorSWF" class="org.archive.crawler.extractor.ExtractorSWF"> 
        <boolean name="enabled">true</boolean>  
        <newObject name="ExtractorSWF#decide-rules" class="org.archive.crawler.deciderules.DecideRuleSequence"> 
          <map name="rules"/> 
        </newObject> 
      </newObject> 
    </map>  
    <!-- end of Heritrix extract processors -->  
    <!-- Heritrix write processors -->  
    <map name="write-processors"> 
      <newObject name="DeDuplicator" class="is.hi.bok.deduplicator.DeDuplicator"> 
        <boolean name="enabled">true</boolean>  
        <map name="filters"/>  
        <string name="index-location">/home/onbpre/work/nas/deploy/onbpre/cache/DEDUP_CRAWL_LOG/1-cache</string>  
        <string name="matching-method">By URL</string>  
        <boolean name="try-equivalent">true</boolean>  
        <boolean name="change-content-size">false</boolean>  
        <string name="mime-filter">^text/.*</string>  
        <string name="filter-mode">Blacklist</string>  
        <string name="analysis-mode">Timestamp</string>  
        <string name="log-level">SEVERE</string>  
        <string name="origin"/>  
        <string name="origin-handling">Use index information</string>  
        <boolean name="stats-per-host">true</boolean>  
        <boolean name="use-sparse-range-filter">true</boolean> 
      </newObject>  
      <newObject name="Archiver" class="org.archive.crawler.writer.ARCWriterProcessor"> 
        <boolean name="enabled">true</boolean>  
        <newObject name="Archiver#decide-rules" class="org.archive.crawler.deciderules.DecideRuleSequence"> 
          <map name="rules"/> 
        </newObject>  
        <boolean name="compress">false</boolean>  
        <string name="prefix">2-1</string>  
        <string name="suffix">${HOSTNAME}</string>  
        <integer name="max-size-bytes">100000000</integer>  
        <stringList name="path"> 
          <string>arcs</string> 
        </stringList>  
        <integer name="pool-max-active">5</integer>  
        <integer name="pool-max-wait">300000</integer>  
        <long name="total-bytes-to-write">0</long>  
        <boolean name="skip-identical-digests">false</boolean> 
      </newObject> 
    </map>  
    <!-- End of Heritrix write processors -->  
    <!-- Heritrix post processors -->  
    <map name="post-processors"> 
      <newObject name="Updater" class="org.archive.crawler.postprocessor.CrawlStateUpdater"> 
        <boolean name="enabled">true</boolean>  
        <newObject name="Updater#decide-rules" class="org.archive.crawler.deciderules.DecideRuleSequence"> 
          <map name="rules"/> 
        </newObject> 
      </newObject>  
      <newObject name="LinksScoper" class="org.archive.crawler.postprocessor.LinksScoper"> 
        <boolean name="enabled">true</boolean>  
        <newObject name="LinksScoper#decide-rules" class="org.archive.crawler.deciderules.DecideRuleSequence"> 
          <map name="rules"/> 
        </newObject>  
        <boolean name="override-logger">false</boolean>  
        <boolean name="seed-redirects-new-seed">false</boolean>  
        <integer name="preference-depth-hops">-1</integer>  
        <newObject name="scope-rejected-url-rules" class="org.archive.crawler.deciderules.DecideRuleSequence"> 
          <map name="rules"/> 
        </newObject> 
      </newObject>  
      <newObject name="Scheduler" class="org.archive.crawler.postprocessor.FrontierScheduler"> 
        <boolean name="enabled">true</boolean>  
        <newObject name="Scheduler#decide-rules" class="org.archive.crawler.deciderules.DecideRuleSequence"> 
          <map name="rules"/> 
        </newObject> 
      </newObject>  
      <newObject name="ContentSize" class="dk.netarkivet.harvester.harvesting.ContentSizeAnnotationPostProcessor"> 
        <boolean name="enabled">true</boolean>  
        <newObject name="ContentSize#decide-rules" class="org.archive.crawler.deciderules.DecideRuleSequence"> 
          <map name="rules"/> 
        </newObject> 
      </newObject> 
    </map>  
    <!-- end of Heritrix post processors -->  
    <map name="loggers"> 
      <newObject name="crawl-statistics" class="org.archive.crawler.admin.StatisticsTracker"> 
        <integer name="interval-seconds">20</integer> 
      </newObject> 
    </map>  
    <string name="recover-path"/>  
    <boolean name="checkpoint-copy-bdbje-logs">true</boolean>  
    <boolean name="recover-retain-failures">false</boolean>  
    <newObject name="credential-store" class="org.archive.crawler.datamodel.CredentialStore"> 
      <map name="credentials"/> 
    </newObject> 
  </controller> 
</crawl-order>

metadata://netarkivet.dk/crawl/setup/seeds.txt?heritrixVersion=1.14.4&harvestid=1&jobid=2 172.16.13.127 20130522082838 text/plain 26
http://fue.onb.ac.at/test/
metadata://netarkivet.dk/crawl/reports/arcfiles-report.txt?heritrixVersion=1.14.4&harvestid=1&jobid=2 172.16.13.127 20130522083953 text/plain 126
[ARCFILE] [Opened] [Closed] [Size]
2-1-20130522082952-00000-prepc2.arc 2013-05-22T08:29:52.634Z 2013-05-22T08:29:55.706Z 2548

metadata://netarkivet.dk/crawl/reports/crawl-report.txt?heritrixVersion=1.14.4&harvestid=1&jobid=2 172.16.13.127 20130522082955 text/plain 358
Crawl Name: default_orderxml
Crawl Status: Finished
Duration Time: 4s192ms
Total Seeds Crawled: 1
Total Seeds not Crawled: 0
Total Hosts Crawled: 1
Total Documents Crawled: 4
Documents Crawled Successfully: 4
Novel Documents Crawled: 4
Processed docs/sec: 1
Bandwidth in Kbytes/sec: 0
Total Raw Data Size in Bytes: 1850 (1.8 KB) 
Novel Bytes: 1850 (1.8 KB) 

metadata://netarkivet.dk/crawl/reports/frontier-report.txt?heritrixVersion=1.14.4&harvestid=1&jobid=2 172.16.13.127 20130522082955 text/plain 15
frontier empty

metadata://netarkivet.dk/crawl/reports/hosts-report.txt?heritrixVersion=1.14.4&harvestid=1&jobid=2 172.16.13.127 20130522082955 text/plain 229
[#urls] [#bytes] [host] [#robots] [#remaining] [#novel-urls] [#novel-bytes] [#dup-by-hash-urls] [#dup-by-hash-bytes] [#not-modified-urls] [#not-modified-bytes]
3 1793 fue.onb.ac.at 0 0 3 1793 0 0 0 0 
1 57 dns: 0 0 1 57 0 0 0 0 

metadata://netarkivet.dk/crawl/reports/mimetype-report.txt?heritrixVersion=1.14.4&harvestid=1&jobid=2 172.16.13.127 20130522082955 text/plain 76
[#urls] [#bytes] [mime-types]
2 931 text/html
1 862 image/png
1 57 text/dns

metadata://netarkivet.dk/crawl/reports/processors-report.txt?heritrixVersion=1.14.4&harvestid=1&jobid=2 172.16.13.127 20130522082955 text/plain 1834
Processors report - 201305220829
  Job being crawled:    default_orderxml
  Number of Processors: 16
  NOTE: Some processors may not return a report!

Processor: org.archive.crawler.fetcher.FetchHTTP
  Function:          Fetch HTTP URIs
  CrawlURIs handled: 3
  Recovery retries:   0

Processor: org.archive.crawler.extractor.ExtractorHTTP
  Function:          Extracts URIs from HTTP response headers
  CrawlURIs handled: 3
  Links extracted:   0

Processor: org.archive.crawler.extractor.ExtractorHTML
  Function:          Link extraction on HTML documents
  CrawlURIs handled: 2
  Links extracted:   1

Processor: org.archive.crawler.extractor.ExtractorCSS
  Function:          Link extraction on Cascading Style Sheets (.css)
  CrawlURIs handled: 0
  Links extracted:   0

Processor: org.archive.crawler.extractor.ExtractorJS
  Function:          Link extraction on JavaScript code
  CrawlURIs handled: 0
  Links extracted:   0

Processor: org.archive.crawler.extractor.ExtractorSWF
  Function:          Link extraction on Shockwave Flash documents (.swf)
  CrawlURIs handled: 0
  Links extracted:   0

Processor: is.hi.bok.digest.DeDuplicator
  Function:          Abort processing of duplicate records
                     - Lookup by url in use
  Total handled:     1
  Duplicates found:  1 100.0%
  Bytes total:       862 (862 B)
  Bytes discarded:   862 (862 B) 100.0%
  New (no hits):     0
  Exact hits:        1
  Equivalent hits:   0
  Timestamp predicts: (Where exact URL existed in the index)
  Change correctly:  0
  Change falsly:     0
  Non-change correct:1
  Non-change falsly: 0
  Missing timpestamp:0
  [Host] [total] [duplicates] [bytes] [bytes discarded] [new] [exact] [equiv] [change correct] [change falsly] [non-change correct] [non-change falsly] [no timestamp]
  fue.onb.ac.at 1 1 862 862 0 1 0 0 0 1 0 0


metadata://netarkivet.dk/crawl/reports/responsecode-report.txt?heritrixVersion=1.14.4&harvestid=1&jobid=2 172.16.13.127 20130522082955 text/plain 34
[rescode] [#urls]
200 2
1 1
404 1

metadata://netarkivet.dk/crawl/reports/seeds-report.txt?heritrixVersion=1.14.4&harvestid=1&jobid=2 172.16.13.127 20130522082955 text/plain 73
[code] [status] [seed] [redirect]
200 CRAWLED http://fue.onb.ac.at/test/

metadata://netarkivet.dk/crawl/logs/crawl.log?heritrixVersion=1.14.4&harvestid=1&jobid=2 172.16.13.127 20130522082954 text/plain 799
2013-05-22T08:29:52.667Z     1         57 dns:fue.onb.ac.at P http://fue.onb.ac.at/test/ text/dns #048 20130522082951280+265 sha1:OQKJAKGR7GSYOCU74UEN3JQUGV6AKTEN - content-size:57
2013-05-22T08:29:53.196Z   404        287 http://fue.onb.ac.at/robots.txt P http://fue.onb.ac.at/test/ text/html #046 20130522082953013+175 sha1:RANOOZRTRT75UDOQ3X6KTVKVUFUZLSD4 - content-size:490
2013-05-22T08:29:53.596Z   200        164 http://fue.onb.ac.at/test/ - - text/html #048 20130522082953506+11 sha1:KMCIAOLW74QORCY3YR67IOPQCHLYD6ZA - content-size:441,3t
2013-05-22T08:29:54.223Z   200        607 http://fue.onb.ac.at/test/image.png E http://fue.onb.ac.at/test/ image/png #045 20130522082953914+4 sha1:7OSXB7G3DWDCDPJTD5COGLBJ7NHXWAMR - duplicate:"1-1-20130522081727-00000-prepc2.arc,2548",content-size:862

metadata://netarkivet.dk/crawl/logs/heritrix.out?heritrixVersion=1.14.4&harvestid=1&jobid=2 172.16.13.127 20130522083948 text/plain 12588
The Heritrix process is started in the following environment
 (note that some entries will be changed by the starting JVM):
ANT_HOME=/usr/local/apache-ant-1.8.4
CLASSPATH=/home/onbpre/work/nas/deploy/onbpre/lib/heritrix/lib/heritrix-1.14.4.jar:/home/onbpre/work/nas/deploy/onbpre/lib/heritrix/lib/ant-1.6.2.jar:/home/onbpre/work/nas/deploy/onbpre/lib/heritrix/lib/bsh-2.0b4.jar:/home/onbpre/work/nas/deploy/onbpre/lib/heritrix/lib/commons-cli-1.0.jar:/home/onbpre/work/nas/deploy/onbpre/lib/heritrix/lib/commons-codec-1.3.jar:/home/onbpre/work/nas/deploy/onbpre/lib/heritrix/lib/commons-collections-3.1.jar:/home/onbpre/work/nas/deploy/onbpre/lib/heritrix/lib/commons-httpclient-3.1.jar:/home/onbpre/work/nas/deploy/onbpre/lib/heritrix/lib/commons-io-1.3.1.jar:/home/onbpre/work/nas/deploy/onbpre/lib/heritrix/lib/commons-lang-2.3.jar:/home/onbpre/work/nas/deploy/onbpre/lib/heritrix/lib/commons-logging-1.0.4.jar:/home/onbpre/work/nas/deploy/onbpre/lib/heritrix/lib/commons-net-2.0.jar:/home/onbpre/work/nas/deploy/onbpre/lib/heritrix/lib/commons-pool-1.3.jar:/home/onbpre/work/nas/deploy/onbpre/lib/heritrix/lib/dnsjava-2.0.3.jar:/home/onbpre/work/nas/deploy/onbpre/lib/heritrix/lib/fastutil-5.0.3-heritrix-subset-1.0.jar:/home/onbpre/work/nas/deploy/onbpre/lib/heritrix/lib/itext-1.2.0.jar:/home/onbpre/work/nas/deploy/onbpre/lib/heritrix/lib/jasper-compiler-tomcat-4.1.30.jar:/home/onbpre/work/nas/deploy/onbpre/lib/heritrix/lib/jasper-runtime-tomcat-4.1.30.jar:/home/onbpre/work/nas/deploy/onbpre/lib/heritrix/lib/javaswf-CVS-SNAPSHOT-1.jar:/home/onbpre/work/nas/deploy/onbpre/lib/heritrix/lib/je-3.3.82.jar:/home/onbpre/work/nas/deploy/onbpre/lib/heritrix/lib/jericho-html-2.6.jar:/home/onbpre/work/nas/deploy/onbpre/lib/heritrix/lib/jets3t-0.5.0.jar:/home/onbpre/work/nas/deploy/onbpre/lib/heritrix/lib/jetty-4.2.23.jar:/home/onbpre/work/nas/deploy/onbpre/lib/heritrix/lib/joda-time-1.6.jar:/home/onbpre/work/nas/deploy/onbpre/lib/heritrix/lib/junit-3.8.2.jar:/home/onbpre/work/nas/deploy/onbpre/lib/heritrix/lib/libidn-0.5.9.jar:/home/onbpre/work/nas/deploy/onbpre/lib/heritrix/lib/mg4j-1.0.1.jar:/home/onbpre/work/nas/deploy/onbpre/lib/heritrix/lib/poi-2.0-RC1-20031102.jar:/home/onbpre/work/nas/deploy/onbpre/lib/heritrix/lib/poi-scratchpad-2.0-RC1-20031102.jar:/home/onbpre/work/nas/deploy/onbpre/lib/heritrix/lib/servlet-tomcat-4.1.30.jar:/home/onbpre/work/nas/deploy/onbpre/lib/dk.netarkivet.harvester.jar:/home/onbpre/work/nas/deploy/onbpre/lib/dk.netarkivet.archive.jar:/home/onbpre/work/nas/deploy/onbpre/lib/dk.netarkivet.monitor.jar:/home/onbpre/work/nas/deploy/onbpre/lib/dk.netarkivet.wayback.jar
COLORTERM=gnome-terminal
DBUS_SESSION_BUS_ADDRESS=unix:abstract=/tmp/dbus-1a43hFYT9z,guid=5cef685fd77a91e438bf9da400000049
DEFAULTS_PATH=/usr/share/gconf/gnome-fallback.default.path
DESKTOP_SESSION=gnome-fallback
DISPLAY=:0.0
GDMSESSION=gnome-fallback
GNOME_DESKTOP_SESSION_ID=this-is-deprecated
GNOME_KEYRING_CONTROL=/tmp/keyring-WrVBYq
GNOME_KEYRING_PID=2305
GPG_AGENT_INFO=/tmp/keyring-WrVBYq/gpg:0:1
GTK_MODULES=canberra-gtk-module
HOME=/home/onbpre
JDK_HOME=/usr/lib/jvm/java-6-sun
LANG=de_AT.UTF-8
LD_LIBRARY_PATH=/usr/lib/jvm/jdk1.6.0_22/jre/lib/i386/server:/usr/lib/jvm/jdk1.6.0_22/jre/lib/i386:/usr/lib/jvm/jdk1.6.0_22/jre/../lib/i386
LESSCLOSE=/usr/bin/lesspipe %s %s
LESSOPEN=| /usr/bin/lesspipe %s
LOGNAME=onbpre
LS_COLORS=rs=0:di=01;34:ln=01;36:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:su=37;41:sg=30;43:ca=30;41:tw=30;42:ow=34;42:st=37;44:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.lzma=01;31:*.tlz=01;31:*.txz=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.dz=01;31:*.gz=01;31:*.lz=01;31:*.xz=01;31:*.bz2=01;31:*.bz=01;31:*.tbz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.war=01;31:*.ear=01;31:*.sar=01;31:*.rar=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.svg=01;35:*.svgz=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.webm=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.flv=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.cgm=01;35:*.emf=01;35:*.axv=01;35:*.anx=01;35:*.ogv=01;35:*.ogx=01;35:*.aac=00;36:*.au=00;36:*.flac=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:*.axa=00;36:*.oga=00;36:*.spx=00;36:*.xspf=00;36:
MANDATORY_PATH=/usr/share/gconf/gnome-fallback.mandatory.path
NLSPATH=/usr/dt/lib/nls/msg/%L/%N.cat
OLDPWD=/home/onbpre/work/nas/deploy/onbpre/conf
PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/lib/jvm/java-6-sun/bin:/usr/local/apache-ant-1.8.4/bin
PWD=/home/onbpre/work/nas/deploy/onbpre
SESSION_MANAGER=local/prepc2:@/tmp/.ICE-unix/2316,unix/prepc2:/tmp/.ICE-unix/2316
SHELL=/bin/bash
SHLVL=3
SPEECHD_PORT=7560
SSH_AGENT_PID=2384
SSH_AUTH_SOCK=/tmp/keyring-WrVBYq/ssh
TERM=xterm
UBUNTU_MENUPROXY=libappmenu.so
USER=onbpre
WINDOWID=50511122
XAUTHORITY=/home/onbpre/.Xauthority
XDG_CONFIG_DIRS=/etc/xdg/xdg-gnome-fallback:/etc/xdg
XDG_CURRENT_DESKTOP=GNOME
XDG_DATA_DIRS=/usr/share/gnome-fallback:/usr/share/gnome:/usr/local/share/:/usr/share/
XDG_SEAT_PATH=/org/freedesktop/DisplayManager/Seat0
XDG_SESSION_COOKIE=88fb0bb127079b4a8f4e99064b168e58-1365487533.250460-125354738
XDG_SESSION_PATH=/org/freedesktop/DisplayManager/Session0
XFILESEARCHPATH=/usr/dt/app-defaults/%L/Dt
_=/usr/bin/java
Process properties:
dk.netarkivet.settings.file=/home/onbpre/work/nas/deploy/onbpre/conf/settings_HarvestControllerApplication_high.xml
file.encoding=UTF-8
file.encoding.pkg=sun.io
file.separator=/
heritrix.version=1.14.4
java.awt.graphicsenv=sun.awt.X11GraphicsEnvironment
java.awt.printerjob=sun.print.PSPrinterJob
java.class.path=/home/onbpre/work/nas/deploy/onbpre/lib/dk.netarkivet.harvester.jar:/home/onbpre/work/nas/deploy/onbpre/lib/dk.netarkivet.archive.jar:/home/onbpre/work/nas/deploy/onbpre/lib/dk.netarkivet.monitor.jar:/home/onbpre/work/nas/deploy/onbpre/lib/dk.netarkivet.wayback.jar:
java.class.version=50.0
java.endorsed.dirs=/usr/lib/jvm/jdk1.6.0_22/jre/lib/endorsed
java.ext.dirs=/usr/lib/jvm/jdk1.6.0_22/jre/lib/ext:/usr/java/packages/lib/ext
java.home=/usr/lib/jvm/jdk1.6.0_22/jre
java.io.tmpdir=/tmp
java.library.path=/usr/lib/jvm/jdk1.6.0_22/jre/lib/i386/server:/usr/lib/jvm/jdk1.6.0_22/jre/lib/i386:/usr/lib/jvm/jdk1.6.0_22/jre/../lib/i386:/usr/java/packages/lib/i386:/lib:/usr/lib
java.runtime.name=Java(TM) SE Runtime Environment
java.runtime.version=1.6.0_22-b04
java.security.manager=
java.security.policy=/home/onbpre/work/nas/deploy/onbpre/conf/security.policy
java.specification.name=Java Platform API Specification
java.specification.vendor=Sun Microsystems Inc.
java.specification.version=1.6
java.util.logging.config.file=/home/onbpre/work/nas/deploy/onbpre/conf/log_HarvestControllerApplication_high.prop
java.vendor=Sun Microsystems Inc.
java.vendor.url=http://java.sun.com/
java.vendor.url.bug=http://java.sun.com/cgi-bin/bugreport.cgi
java.version=1.6.0_22
java.vm.info=mixed mode
java.vm.name=Java HotSpot(TM) Server VM
java.vm.specification.name=Java Virtual Machine Specification
java.vm.specification.vendor=Sun Microsystems Inc.
java.vm.specification.version=1.0
java.vm.vendor=Sun Microsystems Inc.
java.vm.version=17.1-b03
line.separator=

org.apache.commons.logging.Log=org.apache.commons.logging.impl.Jdk14Logger
org.archive.crawler.frontier.AbstractFrontier.queue-assignment-policy=org.archive.crawler.frontier.HostnameQueueAssignmentPolicy,org.archive.crawler.frontier.IPQueueAssignmentPolicy,org.archive.crawler.frontier.BucketQueueAssignmentPolicy,org.archive.crawler.frontier.SurtAuthorityQueueAssignmentPolicy,dk.netarkivet.harvester.harvesting.DomainnameQueueAssignmentPolicy,dk.netarkivet.harvester.harvesting.SeedUriDomainnameQueueAssignmentPolicy
os.arch=i386
os.name=Linux
os.version=3.2.0-38-generic-pae
path.separator=:
sun.arch.data.model=32
sun.boot.class.path=/usr/lib/jvm/jdk1.6.0_22/jre/lib/resources.jar:/usr/lib/jvm/jdk1.6.0_22/jre/lib/rt.jar:/usr/lib/jvm/jdk1.6.0_22/jre/lib/sunrsasign.jar:/usr/lib/jvm/jdk1.6.0_22/jre/lib/jsse.jar:/usr/lib/jvm/jdk1.6.0_22/jre/lib/jce.jar:/usr/lib/jvm/jdk1.6.0_22/jre/lib/charsets.jar:/usr/lib/jvm/jdk1.6.0_22/jre/classes
sun.boot.library.path=/usr/lib/jvm/jdk1.6.0_22/jre/lib/i386
sun.cpu.endian=little
sun.cpu.isalist=
sun.desktop=gnome
sun.io.unicode.encoding=UnicodeLittle
sun.java.launcher=SUN_STANDARD
sun.jnu.encoding=UTF-8
sun.management.compiler=HotSpot Tiered Compilers
sun.os.patch.level=unknown
user.country=AT
user.dir=/home/onbpre/work/nas/deploy/onbpre
user.home=/home/onbpre
user.language=de
user.name=onbpre
user.timezone=Europe/Vienna
Working directory: harvester_high/2_1369211317954
08:29:43.486 EVENT  Starting Jetty/4.2.23
08:29:44.101 EVENT  Started WebApplicationContext[/,Heritrix Console]
08:29:44.387 EVENT  Started SocketListener on 0.0.0.0:8192
08:29:44.387 EVENT  Started org.mortbay.jetty.Server@60e128
2013-05-22 08:29:46.875 INFO thread-10 org.archive.crawler.Heritrix.postRegister() org.archive.crawler:guiport=8192,host=prepc2,jmxport=8193,name=Heritrix,type=CrawlService registered to MBeanServerId=prepc2_1369211381561, SpecificationVersion=1.4, ImplementationVersion=1.6.0_22-b04, SpecificationVendor=Sun Microsystems
Heritrix version: 1.14.4
2013-05-22 08:29:47.028 INFO thread-11 org.archive.crawler.Heritrix.invoke() JMX invoke: completedJobs()
2013-05-22 08:29:47.031 INFO thread-11 org.archive.crawler.Heritrix.invoke() JMX invoke: pendingJobs()
2013-05-22 08:29:47.043 INFO thread-11 org.archive.crawler.Heritrix.invoke() JMX invoke: addJob("/home/onbpre/work/nas/deploy/onbpre/harvester_high/2_1369211317954/order.xml", "2-1", "Job 2 for harvest 1 performed in harvester_high/2_1369211317954with the deduplication index stored in '/home/onbpre/work/nas/deploy/onbpre/cache/DEDUP_CRAWL_LOG/1-cache' and 1 seeds", "/home/onbpre/work/nas/deploy/onbpre/harvester_high/2_1369211317954/seeds.txt", )
2013-05-22 08:29:47.394 INFO thread-11 org.archive.crawler.Heritrix.invoke() JMX invoke: pendingJobs()
2013-05-22 08:29:47.452 INFO thread-11 org.archive.crawler.Heritrix.invoke() JMX invoke: startCrawling()
log4j:WARN No appenders could be found for logger (dk.netarkivet.common.utils.Settings).
log4j:WARN Please initialize the log4j system properly.
2013-05-22 08:29:49.946 INFO thread-12 org.archive.crawler.admin.CrawlJob.postRegister() org.archive.crawler:host=prepc2,jmxport=8193,mother=Heritrix,name=2-1-20130522082947391,type=CrawlService.Job registered to MBeanServerId=prepc2_1369211381561, SpecificationVersion=1.4, ImplementationVersion=1.6.0_22-b04, SpecificationVendor=Sun Microsystems
2013-05-22 08:29:52.634 INFO thread-14 org.archive.io.WriterPoolMember.createFile() Opened /home/onbpre/work/nas/deploy/onbpre/harvester_high/2_1369211317954/arcs/2-1-20130522082952-00000-prepc2.arc.open
2013-05-22 08:29:55.706 INFO thread-15 org.archive.io.WriterPoolMember.close() Closed /home/onbpre/work/nas/deploy/onbpre/harvester_high/2_1369211317954/arcs/2-1-20130522082952-00000-prepc2.arc, size 2548
2013-05-22 08:29:56.079 INFO thread-15 org.archive.crawler.admin.CrawlJob.postDeregister() org.archive.crawler:host=prepc2,jmxport=8193,mother=Heritrix,name=2-1-20130522082947391,type=CrawlService.Job unregistered from MBeanServerId=prepc2_1369211381561, SpecificationVersion=1.4, ImplementationVersion=1.6.0_22-b04, SpecificationVendor=Sun Microsystems
2013-05-22 08:39:47.724 INFO thread-11 org.archive.crawler.Heritrix.invoke() JMX invoke: shutdown()
2013-05-22 08:39:47.731 INFO thread-17 org.archive.crawler.Heritrix.postDeregister() org.archive.crawler:guiport=8192,host=prepc2,jmxport=8193,name=Heritrix,type=CrawlService unregistered from MBeanServerId=prepc2_1369211381561, SpecificationVersion=1.4, ImplementationVersion=1.6.0_22-b04, SpecificationVendor=Sun Microsystems
08:39:47.768 EVENT  Stopping Acceptor ServerSocket[addr=0.0.0.0/0.0.0.0,port=0,localport=8192]
08:39:47.769 EVENT  Stopped SocketListener on 0.0.0.0:8192
08:39:47.770 EVENT  Stopped WebApplicationContext[/,Heritrix Console]
08:39:47.770 EVENT  Stopped org.mortbay.http.NCSARequestLog@12088db
08:39:47.771 EVENT  Stopped org.mortbay.jetty.Server@60e128
08:39:47.771 EVENT  Stopped WebApplicationContext[/,Heritrix Console]
08:39:47.771 EVENT  Stopped org.mortbay.jetty.Server@60e128

metadata://netarkivet.dk/crawl/logs/heritrix_dmesg.log?heritrixVersion=1.14.4&harvestid=1&jobid=2 172.16.13.127 20130522082946 text/plain 119
Heritrix 1.14.4 is running.
Web console is at: http://0.0.0.0:8192
Web console login and password: admin/adminPassword

metadata://netarkivet.dk/crawl/logs/local-errors.log?heritrixVersion=1.14.4&harvestid=1&jobid=2 172.16.13.127 20130522082947 text/plain 0

metadata://netarkivet.dk/crawl/logs/progress-statistics.log?heritrixVersion=1.14.4&harvestid=1&jobid=2 172.16.13.127 20130522082955 text/plain 474
20130522082951 CRAWL RESUMED - Running
           timestamp  discovered      queued   downloaded       doc/s(avg)  KB/s(avg)   dl-failures   busy-thread   mem-use-KB  heap-size-KB   congestion   max-depth   avg-depth
20130522082954 CRAWL ENDING - Finished
2013-05-22T08:29:55Z           4           0            4             1(1)       0(0)             0             0        23292         61824            �           0           0
20130522082955 CRAWL ENDED - Finished

metadata://netarkivet.dk/crawl/logs/runtime-errors.log?heritrixVersion=1.14.4&harvestid=1&jobid=2 172.16.13.127 20130522082947 text/plain 0

metadata://netarkivet.dk/crawl/logs/uri-errors.log?heritrixVersion=1.14.4&harvestid=1&jobid=2 172.16.13.127 20130522082947 text/plain 0

metadata://netarkivet.dk/crawl/index/cdx?majorversion=1&minorversion=0&harvestid=1&jobid=2&timestamp=20130522082952&serialno=00000 172.16.13.127 20130522083953 application/x-cdx 390
dns:fue.onb.ac.at - 20130522082951 text/dns 57 2-1-20130522082952-00000-prepc2.arc 1356 d8162a42a38237cbbdee8d7c302a2820
http://fue.onb.ac.at/robots.txt - 20130522082953 text/html 490 2-1-20130522082952-00000-prepc2.arc 1470 146d3ba975975e051841d41baadcd3d5
http://fue.onb.ac.at/test/ - 20130522082953 text/html 441 2-1-20130522082952-00000-prepc2.arc 2036 adddf7a8b11972957369d659ea1946db

