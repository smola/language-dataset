<?xml version="1.0" encoding="utf-8"?>
<?python
import time
def ymd(stamp):
    return time.strftime('%d/%m/%Y', time.localtime(int(stamp)))
?>
<html xmlns:py="http://purl.org/kid/ns#">
<head>
  <title py:content="'RepoView KAOS: %s' % repo_data['title']"/>
  <link rel="stylesheet" href="layout/repostyle.css" type="text/css"/>
  <meta name="robots" content="noindex,follow" />
  <meta property="og:site_name" content="${repo_data['title']}" />
  <meta property="og:title" content="${pkg_data['name']}" />
  <meta property="og:description" content="${pkg_data['summary']}" />
</head>
<body>
    <div class="levbar">
      <p class="page-title" py:content="group_data['name']"/>
      <ul class="levbar-list">
        <li>
        <a href="${group_data['filename']}" 
            title="Back to package listing"
            class="nlink">← Back to group</a>
    </li>
    </ul>

    </div>
    <div class="main">
        <p class="nav">Jump to letter: [
          <span class="letter-list">
            <a py:for="letter in repo_data['letters']"
              class="nlink"
              href="${'letter_%s.group.html' % letter.lower()}" py:content="letter"/>
          </span>]
        </p>

        <h2><span class="pkg-name" py:content="'%s' % pkg_data['name']"></span> - <span class="pkg-summary" py:content="'%s' % pkg_data['summary']"></span></h2>

        <table border="0" cellspacing="0" cellpadding="2">
          <tr py:if="pkg_data['url']">
            <th>Website:</th>
            <td><a href="${pkg_data['url']}" py:content="pkg_data['url']"/></td>
          </tr>
          <tr py:if="pkg_data['rpm_license']">
            <th>License:</th>
            <td py:content="pkg_data['rpm_license']"/>
          </tr>
          <tr py:if="pkg_data['vendor']">
            <th>Vendor:</th>
            <td py:content="pkg_data['vendor']"/>
          </tr>
        </table>

        <dl>
        <dt>Description:</dt>
        <dd><pre py:content="pkg_data['description']"/></dd>
        </dl>

        <h2>Packages</h2>
        <table border="0" cellspacing="0" cellpadding="10">
        <tr py:for="(e, v, r, a, built, size, loc, author, log, added) in pkg_data['rpms']">
            <td valign="top"><a href="${'../%s' % loc}" class="inpage" 
              py:content="'%s-%s-%s.%s' % (pkg_data['name'], v, r, a)"/>
              <span class="pkg-size" py:content="'%s' % size"/></td>
            <td valign="top" py:if="log">
              Changelog by <span py:content="author"/> <span class="changelog-date" py:content="'(%s)' % ymd(added)"/>
              <pre class="changelog" py:content="log"/>
            </td>
            <td valign="top" py:if="not log">
            	<em>(no changelog entry)</em>
            </td>
        </tr>
        </table>
        <p class="footer-note">
          Listing created by
          <a href="https://github.com/essentialkaos/repoview-kaos"
            class="repoview" py:content="'repoview-kaos-%s' % repo_data['my_version']"/>
        </p>
    </div>
</body>
</html>
