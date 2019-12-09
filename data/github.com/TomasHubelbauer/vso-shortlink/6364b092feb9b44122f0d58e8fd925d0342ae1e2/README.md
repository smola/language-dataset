# VSO Shortlink JavaScript Bookmarklet

This is a JavaScript bookmarklet for use in the browser bookmarks bar.

When clicked, if the users is on a VSO PBI page, a shortlink in the form of `http://*.vso.io/#` is derived from the URL.

This is faster than to go into the PBI dropdown, click the shortlink item (after it has loaded) and copying from the
modal it presents (after is has loaded). This flow is super painful and frankly, daft, as all the information to get
the shortlink are in the URL so there is no reason to make not one but two asynchronous network calls to obtain it.

You can drag this: [VSO shortlink](https://hubelbauer.net) to your bookmarks bar, then right-click and select Edit and
replace the URL with one of the code blocks below, depending on your preference.

Do not worry about the link breaks, the browser will collapse them into spaces.

## To-Do

- Add support for issues open in the backlog page: `https://_.visualstudio.com/_/_sprints/backlog/_/_/_/${sprintName}`
  - Scan the DOM for the ticket number as it is not included in the URL: `document.querySelector('a.caption').href`

## Display in `prompt`

```js
javascript:
[,,o,v,c,,,e,n]=location.href.split(/[\.\/]/g);
v+c+e=='visualstudiocomedit'
&&prompt('Copied!',`http://${o}.vso.io/${n}`)
&&false
```

## Copy to the clipboard

```js
javascript:[,,o,v,c,,,e,n]=location.href.split(/[\.\/]/g);
v+c+e=='visualstudiocomedit'
&&navigator.clipboard.writeText(`http://${o}.vso.io/${n}`)
  .then(()=>location.hash='Copied!')
  .catch(e=>location.hash=e.toString())
&&false
```

## Display in the page

```js
javascript:
[,,o,v,c,,,e,n]=location.href.split(/[\.\/]/g);
v+c+e=='visualstudiocomedit'
&&`<a href="http://${o}.vso.io/${n}">${n}</a>`
```
