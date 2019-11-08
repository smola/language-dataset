module repository

import gololang.Errors
import gololang.Async

import goloctokit

function log = |txt, args...| -> println(java.text.MessageFormat.format(txt, args))

function main = |args| {

  let TOKEN_GITHUB_ENTERPRISE = System.getenv("TOKEN_GITHUB_ENTERPRISE")
  let TOKEN_GITHUB_DOT_COM = System.getenv("TOKEN_GITHUB_DOT_COM")

  let gitHubClientEnterprise = GitHubClient(
    uri= "http://ghe.k33g/api/v3",
    token= TOKEN_GITHUB_ENTERPRISE
  )
  let gitHubClient = GitHubClient(
    uri= "https://api.github.com",
    token= TOKEN_GITHUB_DOT_COM
  )

  let k33g = gitHubClientEnterprise: getUser("k33g")
  log("login: {0} email: {1}", k33g: login(), k33g: email())
  log("avatar url: {0}", k33g?: avatar_url())

  # Create a repository for the authenticated user
  let res = gitHubClientEnterprise: createRepository(
    name="yop-"+uuid(),
    description="this is my repository",
    private=false,
    hasIssues=true
  )
  # Create a repository for the ACME organization
  gitHubClientEnterprise: createRepository(
    name="yup-"+uuid(),
    description="this is my repository",
    organization="ACME",
    private=false,
    hasIssues=true
  )

}
