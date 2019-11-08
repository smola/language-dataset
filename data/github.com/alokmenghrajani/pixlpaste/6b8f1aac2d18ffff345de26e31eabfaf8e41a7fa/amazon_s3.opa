/**
 * Amazon S3 module for OPA
 *
 * Amazon Simple Storage Service (S3) offers a very reliable & scalable
 * storage service.
 *
 * You can use it as a key-value store for your data or you can expose
 * the data directly for content delivery (think CDN).
 *
 * Note: this module is still incomplete, and only implements basic operations
 * on Objects.
 *
 * Usage:
 * 1. checkout the soure code from github.com
 * 2. Copy amazon_s3_auth_sample.opa to amazon_s3_auth.opa
 * 3. Configure your public and private keys in amazon_s3_auth.opa
 * 4. Compile you opa application as you would normally do it.
 *
 *    Note: your amazon private key is going to end up embedded in
 *    the final executable. Make sure not to share it. If you
 *    share your source code, you should include amazon_s3_auth_sample.opa
 *    of amazon_s3_auth.opa.
 *
 * @author Alok Menghrajani <alok@fb.com>
 */

import stdlib.crypto

type AmazonS3.error =
  {
    string code,
    string message,
    string request_id,
  }
and AmazonS3.response =
  { success } or
  { AmazonS3.error failure }

module AmazonS3 {
  function AmazonS3.response delete(string bucket, string path) {
    headers = computeSignature("DELETE", "", "", bucket, path)
    _ = WebClient.Delete.try_delete_with_options(
      Option.get(Uri.of_string("http://{bucket}.s3.amazonaws.com/{path}")),
      {
        auth: {none},
        custom_headers: {some: headers},
        custom_agent: {none},
        timeout_sec: {none},
        ssl_key: {none},
        ssl_policy: {none},
      }
    )
    {success}
    // todo: convert response to AmazonS3.response
  }

  function string get(string bucket, string path) {
    headers = computeSignature("GET", "", "", bucket, path)
    r = WebClient.Get.try_get_with_options(
      Option.get(Uri.of_string("http://{bucket}.s3.amazonaws.com/{path}")),
      {
        auth: {none},
        custom_headers: {some: headers},
        custom_agent: {none},
        follow_redirects: 0,
        timeout_sec: {none},
        ssl_key: {none},
        ssl_policy: {none},
      }
    )
    match (r) {
      case {WebClient.failure failure:_}:
        // TODO: handle failure
        "error"
      case {WebClient.success(string) success:success}:
        "{success.content}"
      case _:
        // TODO: handle failure
        "error"
    }
  }

  function put(string bucket, string path, string mimetype, string data) {
    headers = computeSignature("PUT", "", mimetype, bucket, path)
    _ = WebClient.Put.try_put_with_options(
      Option.get(Uri.of_string("http://{bucket}.s3.amazonaws.com/{path}")),
      data,
      {
        auth: {none},
        custom_headers: {some:headers},
        mimetype: mimetype,
        custom_agent: {none},
        redirect_to_get: {none},
        timeout_sec: {none},
        ssl_key: {none},
        ssl_policy: {none},
      }
    )

    // todo: convert response to AmazonS3.response
    {success}
  }

  function string computeSignature(string method, string md5, string mimetype, string bucket, string path) {
    timezone = AmazonS3Auth.timezone
    date_printer = Date.generate_printer("%a, %0d %b %Y %T {timezone}")
    string date = Date.to_formatted_string(date_printer, Date.now())
    string sts = "{method}\n{md5}\n{mimetype}\n{date}\n/{bucket}/{path}"
    string hmac = Crypto.Base64.encode(Crypto.Hash.hmac_sha1(AmazonS3Auth.private_key, sts))
    "Date: {date}\nAuthorization: AWS {AmazonS3Auth.public_key}:{hmac}"
  }
}
