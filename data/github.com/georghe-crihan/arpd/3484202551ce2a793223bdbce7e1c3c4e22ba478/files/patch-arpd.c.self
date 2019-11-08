--- arpd.c.orig	Thu Oct 12 09:28:31 2006
+++ arpd.c	Thu Oct 12 10:38:06 2006
@@ -181,6 +181,65 @@
 	return (filter);
 }
 
+/* geocrime */
+static struct intf_entry self_ifent[3];
+static void arpd_exit(int status);
+
+static void
+arp_add_entry(struct addr *ip, struct addr *mac)
+{
+	struct arp_req *req;
+
+	if ((req = calloc(1, sizeof(*req))) == NULL) {
+		syslog(LOG_ERR, "calloc: %m");
+		arpd_exit(1);
+	}
+	
+	addr_pack(&req->ha, ADDR_TYPE_ETH, ETH_ADDR_BITS,
+	    &mac->addr_eth, ETH_ADDR_LEN);
+	addr_pack(&req->pa, ADDR_TYPE_IP, IP_ADDR_BITS,
+	    &ip->addr_ip, IP_ADDR_LEN);
+
+	/* Timeouts skipped here */
+	
+	SPLAY_INSERT(tree, &arpd_reqs, req);
+}
+
+static void
+insert_self_entry(intf_t *intf, char *dev)
+{
+	int i;
+	struct arp_req *p;
+	
+	self_ifent[0].intf_len = sizeof(self_ifent);
+	strncpy(self_ifent[0].intf_name, dev, sizeof(self_ifent[0].intf_name) - 1);
+	self_ifent[0].intf_name[sizeof(self_ifent[0].intf_name) - 1] = '\0';
+	
+	if (intf_get(intf, &self_ifent[0]) < 0)
+		err(1, "intf_get");
+
+	arp_add_entry(&(self_ifent[0].intf_addr), 
+		&self_ifent[0].intf_link_addr
+		);
+	fprintf(stderr, "mac: %s, main_ip: %s, num_aliases: %lu\n", 
+		addr_ntoa(&self_ifent[0].intf_link_addr),
+		addr_ntoa(&self_ifent[0].intf_addr),
+		self_ifent[0].intf_alias_num
+		);
+	for (i = 0; i < self_ifent[0].intf_alias_num; i++)
+	  if (self_ifent[0].intf_alias_addrs[i].addr_type==ADDR_TYPE_IP) {
+	    arp_add_entry(&self_ifent[0].intf_alias_addrs[i],
+			&self_ifent[0].intf_link_addr
+			);
+            fprintf(stderr,"intf_alias[%d]: %s\n", i, addr_ntoa(&self_ifent[0].intf_alias_addrs[i]));
+
+#if 0
+	  SPLAY_FOREACH(p, tree, &arpd_reqs)
+	    fprintf(stderr, "DUMP: %s, %s\n", addr_ntoa(&p->pa), addr_ntoa(&p->ha));
+#endif
+          }
+}
+
 static void
 arpd_init(char *dev, int naddresses, char **addresses)
 {
@@ -218,6 +277,8 @@
 	    dst ? "and (" : "", dst ? dst : "", dst ? ")" : "",
 	    addr_ntoa(&arpd_ifent.intf_link_addr));
 	
+	insert_self_entry(intf, dev);
+
 	if ((arpd_pcap = pcap_open_live(dev, 128, 0, 500, ebuf)) == NULL)
 		errx(1, "pcap_open_live: %s", ebuf);
 	
@@ -275,6 +336,7 @@
 		syslog(LOG_ERR, "couldn't send packet: %m");
 }
 
+#if 0
 static int
 arpd_lookup(struct addr *addr)
 {
@@ -299,6 +361,7 @@
 	}
 	return (error);
 }
+#endif
 
 static void
 arpd_free(struct arp_req *req)
@@ -319,6 +382,7 @@
 	arpd_free(req);
 }
 
+#if 0
 static void
 arpd_discover(struct arp_req *req, struct addr *ha)
 {
@@ -343,6 +407,7 @@
 
 	arpd_discover(req, NULL);
 }
+#endif
 
 static void
 arpd_recv_cb(u_char *u, const struct pcap_pkthdr *pkthdr, const u_char *pkt)
@@ -372,6 +437,7 @@
 
 		req = SPLAY_FIND(tree, &arpd_reqs, &tmp);
 		if (req == NULL) {
+#if 0
 			if ((req = calloc(1, sizeof(*req))) == NULL) {
 				syslog(LOG_ERR, "calloc: %m");
 				arpd_exit(1);
@@ -414,6 +480,14 @@
 				    __FUNCTION__, addr_ntoa(&req->pa),
 				    req->cnt);
 			}
+#else
+			syslog(LOG_DEBUG, "%s: %s not in cache",
+					__FUNCTION__, addr_ntoa(&tmp.pa));
+		} else {
+		        arpd_send(arpd_eth, ARP_OP_REPLY,
+			    &req->ha, &req->pa,
+			    &src.arp_ha, &src.arp_pa);
+#endif
 		}
 		break;
 		
