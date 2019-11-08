inherit "etc/test/tests/pike_test_common.pike"; // Necessary for stuff in testsuite.h to work...

#include <testsuite.h>

#ifdef DAV_DEBUG
#define DAV_WERROR(X...)	werror(X)
#else /* !DAV_DEBUG */
#define DAV_WERROR(X...)
#endif /* DAV_DEBUG */

array(Standards.URI) get_test_urls(Configuration conf,
                                   string webdav_mount_point,
                                   string|void username,
                                   string|void password)
{
  array(Standards.URI) uris = ({});
  // Run the suite once with every http protocol modules in the conf.
  // This allows for testing such things as sub-path mounted sites etc.
  foreach(conf->registered_urls, string full_url) {
    mapping(string:string|Configuration|array(Protocol)) port_info =
      roxen.urls[full_url];
    DAV_WERROR("full url: %O\n"
	       "port_info: %O\n",
	       full_url, port_info);
    if (!test_true(mappingp, port_info)) continue;
    array(Protocol) ports = port_info->ports;
    if (!test_true(arrayp, ports)) continue;
    foreach(ports, Protocol prot) {
      if (!test_true(stringp, prot->prot_name)) continue;
      if (prot->prot_name != "http") continue;

      if (prot->bound != 1) continue;

      if (!test_true(mappingp, prot->urls)) continue;

      DAV_WERROR("prot: %O\n"
		 "prot->urls: %O\n", prot, prot->urls);

      // Strip the fragment from the full_url.
      string url = (full_url/"#")[0];
      mapping(string:mixed) url_data = prot->urls[url];
      if (!test_true(mappingp, url_data)) continue;

      // NB: url_data is typically the same mapping as port_info.
      DAV_WERROR("url data: %O\n", url_data);
      test_true(`==, url_data->conf, conf);
      test_true(stringp, url_data->hostname);
      test_true(stringp, url_data->path || "/");

      Standards.URI url_uri = Standards.URI(url, "http://*/");
      Standards.URI base_uri = Standards.URI(
        Stdio.append_path(url_data->path || "/",
                          webdav_mount_point),
                          url_uri);
      base_uri->port = prot->port;
      base_uri->host = prot->ip || "127.0.0.1";

      if (username) {
        base_uri->user = username;
        base_uri->password = password;
      }
      uris += ({base_uri});
    }
  }
  return uris;
}
