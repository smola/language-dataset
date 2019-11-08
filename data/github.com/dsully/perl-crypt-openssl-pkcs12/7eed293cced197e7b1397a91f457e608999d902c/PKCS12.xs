#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

#include <openssl/crypto.h>
#include <openssl/err.h>
#include <openssl/pem.h>
#include <openssl/pkcs12.h>
#include <openssl/x509.h>
#include <openssl/ssl.h>

#define NOKEYS          0x1
#define NOCERTS         0x2
#define INFO            0x4
#define CLCERTS         0x8
#define CACERTS         0x10

#if OPENSSL_VERSION_NUMBER < 0x10100000L
#define PKCS12_SAFEBAG_get0_p8inf(o) ((o)->value.keybag)
#define PKCS12_SAFEBAG_get0_attr PKCS12_get_attr
#define CONST_PKCS8_PRIV_KEY_INFO PKCS8_PRIV_KEY_INFO
#else
#define CONST_PKCS8_PRIV_KEY_INFO const PKCS8_PRIV_KEY_INFO
#endif

const EVP_CIPHER *enc;

/* fake our package name */
typedef PKCS12*  Crypt__OpenSSL__PKCS12;

void croakSSL(char* p_file, int p_line) {

  const char* errorReason;

  /* Just return the top error on the stack */
  errorReason = ERR_reason_error_string(ERR_get_error());

  ERR_clear_error();

  croak("%s:%d: OpenSSL error: %s", p_file, p_line, errorReason);
}

#define CHECK_OPEN_SSL(p_result) if (!(p_result)) croakSSL(__FILE__, __LINE__);

EVP_PKEY* _load_pkey(char* keyString, EVP_PKEY*(*p_loader)(BIO*, EVP_PKEY**, pem_password_cb*, void*)) {

  EVP_PKEY* pkey;
  BIO* stringBIO;

  if (!strncmp(keyString, "----", 4)) {

    CHECK_OPEN_SSL(stringBIO = BIO_new_mem_buf(keyString, strlen(keyString)));

  } else {

    CHECK_OPEN_SSL(stringBIO = BIO_new_file(keyString, "r"));
  }

  pkey = p_loader(stringBIO, NULL, NULL, NULL);

  (void)BIO_set_close(stringBIO, BIO_CLOSE);
  BIO_free_all(stringBIO);

  CHECK_OPEN_SSL(pkey);
  return pkey;
}

STACK_OF(X509)* _load_cert_chain(char* keyString, STACK_OF(X509_INFO)*(*p_loader)(BIO*, STACK_OF(X509_INFO)*, pem_password_cb*, void*)) {
  int i;
  STACK_OF(X509_INFO) *xis = NULL;
  X509_INFO *xi = NULL;
  BIO* stringBIO;
  STACK_OF(X509) *stack = sk_X509_new_null();

  if (!strncmp(keyString, "----", 4)) {
    CHECK_OPEN_SSL(stringBIO = BIO_new_mem_buf(keyString, strlen(keyString)));
  } else {
    CHECK_OPEN_SSL(stringBIO = BIO_new_file(keyString, "r"));
  }

  xis = p_loader(stringBIO, NULL, NULL, NULL);
  for (i = 0; i < sk_X509_INFO_num(xis); i++) {
    xi = sk_X509_INFO_value(xis, i);
    if (xi->x509 != NULL && stack != NULL) {
      CHECK_OPEN_SSL(xi->x509);
      if (!sk_X509_push(stack, xi->x509))
        goto end;
      xi->x509 = NULL;
    }
  }

 end:
  sk_X509_INFO_pop_free(xis, X509_INFO_free);
  (void)BIO_set_close(stringBIO, BIO_CLOSE);
  BIO_free_all(stringBIO);

  return stack;
}

/* stolen from OpenSSL.xs */
long bio_write_cb(struct bio_st *bm, int m, const char *ptr, int l, long x, long y) {

  if (m == BIO_CB_WRITE) {
    SV *sv = (SV *) BIO_get_callback_arg(bm);
    sv_catpvn(sv, ptr, l);
  }

  if (m == BIO_CB_PUTS) {
    SV *sv = (SV *) BIO_get_callback_arg(bm);
    l = strlen(ptr);
    sv_catpvn(sv, ptr, l);
  }

  return l;
}

static BIO* sv_bio_create(void) {

  SV *sv = newSVpvn("",0);

  /* create an in-memory BIO abstraction and callbacks */
  BIO *bio = BIO_new(BIO_s_mem());

  BIO_set_callback(bio, bio_write_cb);
  BIO_set_callback_arg(bio, (void *)sv);

  return bio;
}

static SV* sv_bio_final(BIO *bio) {

  SV* sv;

  (void)BIO_flush(bio);
  sv = (SV *)BIO_get_callback_arg(bio);
  BIO_set_callback_arg(bio, (void *)NULL);
  BIO_set_callback(bio, (void *)NULL);
  BIO_free_all(bio);

  if (!sv) sv = &PL_sv_undef;

  return sv;
}

static void sv_bio_error(BIO *bio) {

  SV* sv = (SV *)BIO_get_callback_arg(bio);
  if (sv) sv_free(sv);

  BIO_free_all (bio);
}

static const char *ssl_error(void) {
  BIO *bio;
  SV *sv;
  STRLEN l;

  bio = sv_bio_create();
  ERR_print_errors(bio);
  sv = sv_bio_final(bio);
  ERR_clear_error();
  return SvPV(sv, l);
}

/* these are trimmed from their openssl/apps/pkcs12.c counterparts */
int dump_certs_pkeys_bag (BIO *bio, PKCS12_SAFEBAG *bag, char *pass, int passlen, int options, char *pempass) {

  EVP_PKEY *pkey;
  X509 *x509;
  PKCS8_PRIV_KEY_INFO *p8;
  CONST_PKCS8_PRIV_KEY_INFO *p8c;

  switch (M_PKCS12_bag_type(bag)) {

    case NID_keyBag: ;

      if (options & NOKEYS) return 1;

      p8c = PKCS12_SAFEBAG_get0_p8inf(bag);

      if (!(pkey = EVP_PKCS82PKEY (p8c))) return 0;

      PEM_write_bio_PrivateKey (bio, pkey, enc, NULL, 0, NULL, pempass);

      EVP_PKEY_free(pkey);

      break;

    case NID_pkcs8ShroudedKeyBag: ;

      if (options & NOKEYS) return 1;

      if (!(p8 = PKCS12_decrypt_skey(bag, pass, passlen)))
        return 0;

      if (!(pkey = EVP_PKCS82PKEY (p8))) {
        PKCS8_PRIV_KEY_INFO_free(p8);
        return 0;
      }

      PKCS8_PRIV_KEY_INFO_free(p8);

      PEM_write_bio_PrivateKey (bio, pkey, enc, NULL, 0, NULL, pempass);

      EVP_PKEY_free(pkey);

      break;

    case NID_certBag:

      if (options & NOCERTS) return 1;

      if (PKCS12_SAFEBAG_get0_attr(bag, NID_localKeyID)) {

        if (options & CACERTS) return 1;

      } else if (options & CLCERTS) {

        return 1;
      }

      if (M_PKCS12_cert_bag_type(bag) != NID_x509Certificate) return 1;

      if (!(x509 = PKCS12_certbag2x509(bag))) return 0;

      PEM_write_bio_X509 (bio, x509);

      X509_free(x509);

      break;
  }

  return 1;
}

int dump_certs_pkeys_bags(BIO *bio, STACK_OF(PKCS12_SAFEBAG) *bags, char *pass, int passlen, int options, char *pempass) {

  int i;

  for (i = 0; i < sk_PKCS12_SAFEBAG_num(bags); i++) {

    if (!dump_certs_pkeys_bag (bio, sk_PKCS12_SAFEBAG_value (bags, i), pass, passlen, options, pempass)) {
      return 0;
    }
  }

  return 1;
}

int dump_certs_keys_p12(BIO *bio, PKCS12 *p12, char *pass, int passlen, int options, char *pempass) {

  STACK_OF(PKCS7) *asafes;
  STACK_OF(PKCS12_SAFEBAG) *bags;

  int i, bagnid;
  PKCS7 *p7;

  if ((asafes = PKCS12_unpack_authsafes(p12)) == NULL) {
    croak("Unable to PKCS12_unpack_authsafes");
    return 0;
  }

  for (i = 0; i < sk_PKCS7_num(asafes); i++) {

    p7 = sk_PKCS7_value(asafes, i);

    bagnid = OBJ_obj2nid(p7->type);

    if (bagnid == NID_pkcs7_data) {

      bags = PKCS12_unpack_p7data(p7);

    } else if (bagnid == NID_pkcs7_encrypted) {

      bags = PKCS12_unpack_p7encdata(p7, pass, passlen);

    } else {
      continue;
    }

    if (!bags) return 0;

    if (!dump_certs_pkeys_bags(bio, bags, pass, passlen, options, pempass)) {

      sk_PKCS12_SAFEBAG_pop_free(bags, PKCS12_SAFEBAG_free);
      return 0;
    }

    sk_PKCS12_SAFEBAG_pop_free(bags, PKCS12_SAFEBAG_free);
  }

  sk_PKCS7_pop_free(asafes, PKCS7_free);

  return 1;
}

MODULE = Crypt::OpenSSL::PKCS12    PACKAGE = Crypt::OpenSSL::PKCS12

PROTOTYPES: DISABLE

BOOT:
{
  HV *stash;
  char *name;
  int i;

  struct { char *n; I32 v; } Crypt__OpenSSL__PKCS12__const[] = {
    {"NOKEYS", NOKEYS},
    {"NOCERTS", NOCERTS},
    {"INFO", INFO},
    {"CLCERTS", CLCERTS},
    {"CACERTS", CACERTS},
    {Nullch,0}
  };

  OpenSSL_add_all_algorithms();

  stash = gv_stashpvn("Crypt::OpenSSL::PKCS12", 22, TRUE);

  for (i = 0; (name = Crypt__OpenSSL__PKCS12__const[i].n); i++) {
    newCONSTSUB(stash, name, newSViv(Crypt__OpenSSL__PKCS12__const[i].v));
  }
}

Crypt::OpenSSL::PKCS12
new(class)
  SV  *class

  CODE:

  if ((RETVAL = PKCS12_new()) == NULL) {
    croak("Couldn't create PKCS12_new() for class %s", (char*)class);
  }

  OUTPUT:
  RETVAL

Crypt::OpenSSL::PKCS12
new_from_string(class, string)
  SV  *class
  char *string

  ALIAS:
  new_from_file = 1

  PREINIT:
  BIO *bio;

  CODE:

  if (!string) croak("PKCS12_new_from: No string or file was passed.");

  if (ix == 1) {
    bio = BIO_new_file(string, "r");
  } else {
    bio = BIO_new_mem_buf(string, strlen(string));
  }

  if (!bio) croak("Failed to create BIO");

  /* this can come in any number of ways */
  if ((RETVAL = d2i_PKCS12_bio(bio, 0)) == NULL) {
    BIO_free_all(bio);
    croak("%s: Couldn't create PKCS12 from d2i_PKCS12_BIO(): %s", (char*)class, ssl_error());
  }

  BIO_free_all(bio);

  OUTPUT:
  RETVAL

# This is called at per-object destruction time.
void
DESTROY(pkcs12)
  Crypt::OpenSSL::PKCS12 pkcs12;

  CODE:
  if (pkcs12) {
    PKCS12_free(pkcs12);
  }

# This is called via an END block in the Perl module to clean up initialization that happened in BOOT.
void
__PKCS12_cleanup(void)
  CODE:

  CRYPTO_cleanup_all_ex_data();
  ERR_free_strings();
#if OPENSSL_VERSION_NUMBER < 0x10100000L
  ERR_remove_state(0);
#endif
  EVP_cleanup();

SV*
as_string(pkcs12)
  Crypt::OpenSSL::PKCS12 pkcs12;

  PREINIT:
  BIO *bio;

  CODE:

  bio = sv_bio_create();

  if (!(i2d_PKCS12_bio(bio, pkcs12))) {
    sv_bio_error(bio);
    croak("i2d_PKCS12_bio: %s", ssl_error());
  }

  RETVAL = sv_bio_final(bio);

  OUTPUT:
  RETVAL

SV*
mac_ok(pkcs12, pwd = "")
  Crypt::OpenSSL::PKCS12 pkcs12
  char *pwd

  CODE:

  if (!(PKCS12_verify_mac(pkcs12, pwd, strlen(pwd)))) {
    croak("PKCS12_verify_mac: \n%s", ssl_error());
  }

  RETVAL = (PKCS12_verify_mac(pkcs12, pwd, strlen(pwd))) ? &PL_sv_yes : &PL_sv_no;

  OUTPUT:
  RETVAL

SV*
changepass(pkcs12, oldpwd = "", newpwd = "")
  Crypt::OpenSSL::PKCS12 pkcs12
  char *oldpwd
  char *newpwd

  CODE:

  if (!(PKCS12_newpass(pkcs12, oldpwd, newpwd))) {
    warn("PKCS12_newpass: %s %s\n%s", oldpwd, newpwd, ssl_error());
    RETVAL = &PL_sv_no;
  } else {
    RETVAL = &PL_sv_yes;
  }

  OUTPUT:
  RETVAL

SV*
create(pkcs12, cert_chain_pem = "", pk = "", pass = 0, file = 0, name = "PKCS12 Certificate")
  char *cert_chain_pem
  char *pk
  char *pass
  char *file
  char *name

  PREINIT:
  FILE *fp;
  EVP_PKEY* pkey;
  PKCS12 *p12;
  STACK_OF(X509) *cert_chain = NULL;

  CODE:

  pkey       = _load_pkey(pk, PEM_read_bio_PrivateKey);
  cert_chain = _load_cert_chain(cert_chain_pem, PEM_X509_INFO_read_bio);
  p12        = PKCS12_create(pass, name, pkey, sk_X509_shift(cert_chain), cert_chain, 0, 0, 0, 0, 0);

  if (!p12) {
    ERR_print_errors_fp(stderr);
    croak("Error creating PKCS#12 structure\n");
  }

  if (!(fp = fopen(file, "wb"))) {
    ERR_print_errors_fp(stderr);
    croak("Error opening file %s\n", file);
  }

  i2d_PKCS12_fp(fp, p12);
  PKCS12_free(p12);
  fclose(fp);

  RETVAL = &PL_sv_yes;

  OUTPUT:
  RETVAL

SV*
certificate(pkcs12, pwd = "")
  Crypt::OpenSSL::PKCS12 pkcs12
  char *pwd

  PREINIT:
  BIO *bio;

  CODE:

  bio = sv_bio_create();

  PKCS12_unpack_authsafes(pkcs12);
  dump_certs_keys_p12(bio, pkcs12, pwd, strlen(pwd), CLCERTS|NOKEYS, NULL);

  RETVAL = sv_bio_final(bio);

  OUTPUT:
  RETVAL

SV*
private_key(pkcs12, pwd = "")
  Crypt::OpenSSL::PKCS12 pkcs12
  char *pwd

  PREINIT:
  BIO *bio;

  CODE:

  bio = sv_bio_create();

  PKCS12_unpack_authsafes(pkcs12);
  dump_certs_keys_p12(bio, pkcs12, pwd, strlen(pwd), NOCERTS, NULL);

  RETVAL = sv_bio_final(bio);

  OUTPUT:
  RETVAL
