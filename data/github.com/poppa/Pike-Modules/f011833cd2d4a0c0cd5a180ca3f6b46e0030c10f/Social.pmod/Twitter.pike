/*
  Author: Pontus Östlund <https://profiles.google.com/poppanator>

  Permission to copy, modify, and distribute this source for any legal
  purpose granted as long as my name is still attached to it. More
  specifically, the GPL, LGPL and MPL licenses apply to this software.
*/

inherit Social.Oauth1Api : parent;

constant API_URI = "https://api.twitter.com/1.1/";

mapping get(string method, void|ParamsArg args)
{
  return parent::get(get_uri(method), args);
}

private string get_uri(string method)
{
  if (!has_suffix(method, ".json") && !has_suffix(method, ".xml"))
    method += ".json";

  if (has_prefix(method, "/"))
    method = method[1..];

  return API_URI + method;
}

class Authorization
{
  inherit Social.Oauth1Api.Authorization;

  //! The endpoint to send request for a request token
  constant REQUEST_TOKEN_URL = "https://api.twitter.com/oauth/request_token";

  //! The endpoint to send request for an access token
  constant ACCESS_TOKEN_URL = "https://api.twitter.com/oauth/access_token";

  //! The enpoint to redirect to when authenticating an application
  constant USER_AUTH_URL = "https://api.twitter.com/oauth/authenticate";
}