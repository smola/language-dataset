/**
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 **/

#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

#include <stdio.h>
#include <string.h>
#include <stdbool.h>

#if defined(XP_UNIX)
#include <unistd.h>
#endif

// #include "prerror.h"

#include "pk11func.h"
#include "seccomon.h"
#include "secmod.h"
#include "secitem.h"
#include "secder.h"
#include "cert.h"
#include "certdb.h"
#include "ocsp.h"
#include "keyhi.h"
#include "secerr.h"
#include "blapit.h"

#include "nspr.h"
#include "plgetopt.h"
#include "prio.h"
#include "nss.h"

/* #include "vfyutil.h" */

#define RD_BUF_SIZE (60 * 1024)


/* fake our package name */
typedef CERTCertificate* Crypt__NSS__X509__Certificate;
typedef CERTCertList* Crypt__NSS__X509__CertList;
typedef CERTSignedCrl* Crypt__NSS__X509__CRL;

char* initstring;
int initialized = 0;

//---- Beginning here this is a direct copy from NSS vfychain.c

#define REVCONFIG_TEST_UNDEFINED      0
#define REVCONFIG_TEST_LEAF           1
#define REVCONFIG_TEST_CHAIN          2
#define REVCONFIG_METHOD_CRL          1
#define REVCONFIG_METHOD_OCSP         2

#define REV_METHOD_INDEX_MAX  4

typedef struct RevMethodsStruct {
    uint testType;
    char *testTypeStr;
    uint testFlags;
    char *testFlagsStr;
    uint methodType;
    char *methodTypeStr;
    uint methodFlags;
    char *methodFlagsStr;
} RevMethods;

RevMethods revMethodsData[REV_METHOD_INDEX_MAX];

static SECStatus
configureRevocationParams(CERTRevocationFlags *flags)
{
   int i;
   uint testType = REVCONFIG_TEST_UNDEFINED;
   static CERTRevocationTests *revTests = NULL;
   PRUint64 *revFlags = NULL;

   for(i = 0;i < REV_METHOD_INDEX_MAX;i++) {
       if (revMethodsData[i].testType == REVCONFIG_TEST_UNDEFINED) {
           continue;
       }
       if (revMethodsData[i].testType != testType) {
           testType = revMethodsData[i].testType;
           if (testType == REVCONFIG_TEST_CHAIN) {
               revTests = &flags->chainTests;
           } else {
               revTests = &flags->leafTests;
           }
           revTests->number_of_preferred_methods = 0;
           revTests->preferred_methods = 0;
           revFlags = revTests->cert_rev_flags_per_method;
       }
       /* Set the number of the methods independently to the max number of
        * methods. If method flags are not set it will be ignored due to
        * default DO_NOT_USE flag. */
       revTests->number_of_defined_methods = cert_revocation_method_count;
       revTests->cert_rev_method_independent_flags |=
           revMethodsData[i].testFlags;
       if (revMethodsData[i].methodType == REVCONFIG_METHOD_CRL) {
           revFlags[cert_revocation_method_crl] =
               revMethodsData[i].methodFlags;
       } else if (revMethodsData[i].methodType == REVCONFIG_METHOD_OCSP) {
           revFlags[cert_revocation_method_ocsp] =
               revMethodsData[i].methodFlags;
       }
   }
   return SECSuccess;
}

//---- end direct copy from vfychain.c

// adapted from alg1485.c

/* RDNs are sorted from most general to most specific.
 * This code returns the FIRST one found, the most general one found.
 */
static SECItem *
CERT_GetNameElement(PRArenaPool *arena, CERTName *name, int wantedTag)
{
    CERTRDN** rdns = name->rdns;
    CERTRDN*  rdn;
    CERTAVA*  ava  = NULL;

    while (rdns && (rdn = *rdns++) != 0) {
	CERTAVA** avas = rdn->avas;
	while (avas && (ava = *avas++) != 0) {
	    int tag = CERT_GetAVATag(ava);
	    if ( tag == wantedTag ) {
		avas = NULL;
		rdns = NULL; /* break out of all loops */
	    }
	}
    }
    return ava ? CERT_DecodeAVAValue(&ava->value) : NULL;
}


// adapted from secutil.c - removed unnecessary argument.

/*
 * Find the issuer of a Crl.  Use the authorityKeyID if it exists.
 */
CERTCertificate *
FindCrlIssuer(CERTCertDBHandle *dbhandle, SECItem* subject,
                   PRTime validTime)
{
    CERTCertificate *issuerCert = NULL;
    CERTCertList *certList = NULL;

    if (!subject) {
        PORT_SetError(SEC_ERROR_INVALID_ARGS);
        return NULL;
    }

    certList =
        CERT_CreateSubjectCertList(NULL, dbhandle, subject,
                                   validTime, PR_TRUE);
    if (certList) {
        CERTCertListNode *node = CERT_LIST_HEAD(certList);

        /* XXX and authoritykeyid in the future */
        while ( ! CERT_LIST_END(node, certList) ) {
            CERTCertificate *cert = node->cert;
            /* check cert CERTCertTrust data is allocated, check cert
               usage extension, check that cert has pkey in db. Select
               the first (newest) user cert */
            if (cert->trust &&
                CERT_CheckCertUsage(cert, KU_CRL_SIGN) == SECSuccess) {

                issuerCert = CERT_DupCertificate(cert);
                break;
            }
            node = CERT_LIST_NEXT(node);
        }
        CERT_DestroyCertList(certList);
    }
    return(issuerCert);
}



// function more or less ripped from nsNSSCertHelper.cpp, because NSS does apparently
// not support giving string names for OIDs. I mean, what the heck would you POSSIBLY
// want to use THAT for?

static
SV* oid_to_sv(SECItem *oid)
{
  SECOidTag oidTag = SECOID_FindOIDTag(oid);
  const char* out = 0;

  switch (oidTag) {
  case SEC_OID_PKCS1_MD2_WITH_RSA_ENCRYPTION:
    out = "MD2WithRSA";
    break;
  case SEC_OID_PKCS1_MD5_WITH_RSA_ENCRYPTION:
    out = "MD5WithRSA";
    break;
  case SEC_OID_PKCS1_SHA1_WITH_RSA_ENCRYPTION:
    out = "SHA1WithRSA";
    break;
  case SEC_OID_PKCS1_SHA256_WITH_RSA_ENCRYPTION:
    out = "SHA256WithRSA";
    break;
  case SEC_OID_PKCS1_SHA384_WITH_RSA_ENCRYPTION:
    out = "SHA384WithRSA";
    break;
  case SEC_OID_PKCS1_SHA512_WITH_RSA_ENCRYPTION:
    out = "SHA512WithRSA";
    break;
  case SEC_OID_PKCS1_RSA_ENCRYPTION:
    out = "RSAEncr";
    break;
  case SEC_OID_PKCS1_RSA_PSS_SIGNATURE:
    out = "RSAPSSSignature";
    break;
  case SEC_OID_NS_CERT_EXT_CERT_TYPE:
    out = "CertType";
    break;
  case SEC_OID_NS_CERT_EXT_BASE_URL:
    out = "NSCertExtBaseUrl";
    break;
  case SEC_OID_NS_CERT_EXT_REVOCATION_URL:
    out = "NSCertExtRevocationUrl";
    break;
  case SEC_OID_NS_CERT_EXT_CA_REVOCATION_URL:
    out = "NSCertExtCARevocationUrl";
    break;
  case SEC_OID_NS_CERT_EXT_CERT_RENEWAL_URL:
    out = "NSCertExtCertRenewalUrl";
    break;
  case SEC_OID_NS_CERT_EXT_CA_POLICY_URL:
    out = "NSCertExtCAPolicyUrl";
    break;
  case SEC_OID_NS_CERT_EXT_SSL_SERVER_NAME:
    out = "NSCertExtSslServerName";
    break;
  case SEC_OID_NS_CERT_EXT_COMMENT:
    out = "NSCertExtComment";
    break;
  case SEC_OID_NS_CERT_EXT_LOST_PASSWORD_URL:
    out = "NSCertExtLostPasswordUrl";
    break;
  case SEC_OID_NS_CERT_EXT_CERT_RENEWAL_TIME:
    out = "NSCertExtCertRenewalTime";
    break;
  case SEC_OID_NETSCAPE_AOLSCREENNAME:
    out = "NetscapeAolScreenname";
    break;
  case SEC_OID_AVA_COUNTRY_NAME:
    out = "AVACountry";
    break;
  case SEC_OID_AVA_COMMON_NAME:
    out = "AVACN";
    break;
  case SEC_OID_AVA_ORGANIZATIONAL_UNIT_NAME:
    out = "AVAOU";
    break;
  case SEC_OID_AVA_ORGANIZATION_NAME:
    out = "AVAOrg";
    break;
  case SEC_OID_AVA_LOCALITY:
    out = "AVALocality";
    break;
  case SEC_OID_AVA_DN_QUALIFIER:
    out = "AVADN";
    break;
  case SEC_OID_AVA_DC:
    out = "AVADC";
    break;
  case SEC_OID_AVA_STATE_OR_PROVINCE:
    out = "AVAState";
    break;
  case SEC_OID_AVA_SURNAME:
    out = "Surname";
    break;
  case SEC_OID_AVA_GIVEN_NAME:
    out = "GivenName";
    break;
  case SEC_OID_X509_SUBJECT_DIRECTORY_ATTR:
    out = "SubjectDirectoryAttr";
    break;
  case SEC_OID_X509_SUBJECT_KEY_ID:
    out = "SubjectKeyID";
    break;
  case SEC_OID_X509_KEY_USAGE:
    out = "KeyUsage";
    break;
  case SEC_OID_X509_SUBJECT_ALT_NAME:
    out = "SubjectAltName";
    break;
  case SEC_OID_X509_ISSUER_ALT_NAME:
    out = "IssuerAltName";
    break;
  case SEC_OID_X509_BASIC_CONSTRAINTS:
    out = "BasicConstraints";
    break;
  case SEC_OID_X509_NAME_CONSTRAINTS:
    out = "NameConstraints";
    break;
  case SEC_OID_X509_CRL_DIST_POINTS:
    out = "CrlDistPoints";
    break;
  case SEC_OID_X509_CERTIFICATE_POLICIES:
    out = "CertPolicies";
    break;
  case SEC_OID_X509_POLICY_MAPPINGS:
    out = "PolicyMappings";
    break;
  case SEC_OID_X509_POLICY_CONSTRAINTS:
    out = "PolicyConstraints";
    break;
  case SEC_OID_X509_AUTH_KEY_ID:
    out = "AuthKeyID";
    break;
  case SEC_OID_X509_EXT_KEY_USAGE:
    out = "ExtKeyUsage";
    break;
  case SEC_OID_X509_AUTH_INFO_ACCESS:
    out = "AuthInfoAccess";
    break;
  case SEC_OID_ANSIX9_DSA_SIGNATURE:
    out = "AnsiX9DsaSignature";
    break;
  case SEC_OID_ANSIX9_DSA_SIGNATURE_WITH_SHA1_DIGEST:
    out = "AnsiX9DsaSignatureWithSha1";
    break;
  case SEC_OID_ANSIX962_ECDSA_SIGNATURE_WITH_SHA1_DIGEST:
    out = "AnsiX962ECDsaSignatureWithSha1";
    break;
  case SEC_OID_ANSIX962_ECDSA_SHA224_SIGNATURE:
    out = "AnsiX962ECDsaSignatureWithSha224";
    break;
  case SEC_OID_ANSIX962_ECDSA_SHA256_SIGNATURE:
    out = "AnsiX962ECDsaSignatureWithSha256";
    break;
  case SEC_OID_ANSIX962_ECDSA_SHA384_SIGNATURE:
    out = "AnsiX962ECDsaSignatureWithSha384";
    break;
  case SEC_OID_ANSIX962_ECDSA_SHA512_SIGNATURE:
    out = "AnsiX962ECDsaSignatureWithSha512";
    break;
  case SEC_OID_RFC1274_UID:
    out = "UserID";
    break;
  case SEC_OID_PKCS9_EMAIL_ADDRESS:
    out = "PK9Email";
    break;
  case SEC_OID_ANSIX962_EC_PUBLIC_KEY:
    out = "ECPublicKey";
    break;
  /* ANSI X9.62 named elliptic curves (prime field) */
  case SEC_OID_ANSIX962_EC_PRIME192V1:
    /* same as SEC_OID_SECG_EC_SECP192r1 */
    out = "ECprime192v1";
    break;
  case SEC_OID_ANSIX962_EC_PRIME192V2:
    out = "ECprime192v2";
    break;
  case SEC_OID_ANSIX962_EC_PRIME192V3:
    out = "ECprime192v3";
    break;
  case SEC_OID_ANSIX962_EC_PRIME239V1:
    out = "ECprime239v1";
    break;
  case SEC_OID_ANSIX962_EC_PRIME239V2:
    out = "ECprime239v2";
    break;
  case SEC_OID_ANSIX962_EC_PRIME239V3:
    out = "ECprime239v3";
    break;
  case SEC_OID_ANSIX962_EC_PRIME256V1:
    /* same as SEC_OID_SECG_EC_SECP256r1 */
    out = "ECprime256v1";
    break;
  /* SECG named elliptic curves (prime field) */
  case SEC_OID_SECG_EC_SECP112R1:
    out = "ECsecp112r1";
    break;
  case SEC_OID_SECG_EC_SECP112R2:
    out = "ECsecp112r2";
    break;
  case SEC_OID_SECG_EC_SECP128R1:
    out = "ECsecp128r1";
    break;
  case SEC_OID_SECG_EC_SECP128R2:
    out = "ECsecp128r2";
    break;
  case SEC_OID_SECG_EC_SECP160K1:
    out = "ECsecp160k1";
    break;
  case SEC_OID_SECG_EC_SECP160R1:
    out = "ECsecp160r1";
    break;
  case SEC_OID_SECG_EC_SECP160R2:
    out = "ECsecp160r2";
    break;
  case SEC_OID_SECG_EC_SECP192K1:
    out = "ECsecp192k1";
    break;
  case SEC_OID_SECG_EC_SECP224K1:
    out = "ECsecp224k1";
    break;
  case SEC_OID_SECG_EC_SECP224R1:
    out = "ECsecp224r1";
    break;
  case SEC_OID_SECG_EC_SECP256K1:
    out = "ECsecp256k1";
    break;
  case SEC_OID_SECG_EC_SECP384R1:
    out = "ECsecp384r1";
    break;

  case SEC_OID_SECG_EC_SECP521R1:
    out = "ECsecp521r1";
    break;
  /* ANSI X9.62 named elliptic curves (characteristic two field) */
  case SEC_OID_ANSIX962_EC_C2PNB163V1:
    out = "ECc2pnb163v1";
    break;
  case SEC_OID_ANSIX962_EC_C2PNB163V2:
    out = "ECc2pnb163v2";
    break;
  case SEC_OID_ANSIX962_EC_C2PNB163V3:
    out = "ECc2pnb163v3";
    break;
  case SEC_OID_ANSIX962_EC_C2PNB176V1:
    out = "ECc2pnb176v1";
    break;
  case SEC_OID_ANSIX962_EC_C2TNB191V1:
    out = "ECc2tnb191v1";
    break;
  case SEC_OID_ANSIX962_EC_C2TNB191V2:
    out = "ECc2tnb191v2";
    break;
  case SEC_OID_ANSIX962_EC_C2TNB191V3:
    out = "ECc2tnb191v3";
    break;
  case SEC_OID_ANSIX962_EC_C2ONB191V4:
    out = "ECc2onb191v4";
    break;
  case SEC_OID_ANSIX962_EC_C2ONB191V5:
    out = "ECc2onb191v5";
    break;
  case SEC_OID_ANSIX962_EC_C2PNB208W1:
    out = "ECc2pnb208w1";
    break;
  case SEC_OID_ANSIX962_EC_C2TNB239V1:
    out = "ECc2tnb239v1";
    break;
  case SEC_OID_ANSIX962_EC_C2TNB239V2:
    out = "ECc2tnb239v2";
    break;
  case SEC_OID_ANSIX962_EC_C2TNB239V3:
    out = "ECc2tnb239v3";
    break;
  case SEC_OID_ANSIX962_EC_C2ONB239V4:
    out = "ECc2onb239v4";
    break;
  case SEC_OID_ANSIX962_EC_C2ONB239V5:
    out = "ECc2onb239v5";
    break;
  case SEC_OID_ANSIX962_EC_C2PNB272W1:
    out = "ECc2pnb272w1";
    break;
  case SEC_OID_ANSIX962_EC_C2PNB304W1:
    out = "ECc2pnb304w1";
    break;
  case SEC_OID_ANSIX962_EC_C2TNB359V1:
    out = "ECc2tnb359v1";
    break;
  case SEC_OID_ANSIX962_EC_C2PNB368W1:
    out = "ECc2pnb368w1";
    break;
  case SEC_OID_ANSIX962_EC_C2TNB431R1:
    out = "ECc2tnb431r1";
    break;
  /* SECG named elliptic curves (characteristic two field) */
  case SEC_OID_SECG_EC_SECT113R1:
    out = "ECsect113r1";
    break;
  case SEC_OID_SECG_EC_SECT113R2:
    out = "ECsect113r2";
    break;
  case SEC_OID_SECG_EC_SECT131R1:
    out = "ECsect131r1";
    break;
  case SEC_OID_SECG_EC_SECT131R2:
    out = "ECsect131r2";
    break;
  case SEC_OID_SECG_EC_SECT163K1:
    out = "ECsect163k1";
    break;
  case SEC_OID_SECG_EC_SECT163R1:
    out = "ECsect163r1";
    break;
  case SEC_OID_SECG_EC_SECT163R2:
    out = "ECsect163r2";
    break;
  case SEC_OID_SECG_EC_SECT193R1:
    out = "ECsect193r1";
    break;
  case SEC_OID_SECG_EC_SECT193R2:
    out = "ECsect193r2";
    break;
  case SEC_OID_SECG_EC_SECT233K1:
    out = "ECsect233k1";
    break;
  case SEC_OID_SECG_EC_SECT233R1:
    out = "ECsect233r1";
    break;
  case SEC_OID_SECG_EC_SECT239K1:
    out = "ECsect239k1";
    break;
  case SEC_OID_SECG_EC_SECT283K1:
    out = "ECsect283k1";
    break;
  case SEC_OID_SECG_EC_SECT283R1:
    out = "ECsect283r1";
    break;
  case SEC_OID_SECG_EC_SECT409K1:
    out = "ECsect409k1";
    break;
  case SEC_OID_SECG_EC_SECT409R1:
    out = "ECsect409r1";
    break;
  case SEC_OID_SECG_EC_SECT571K1:
    out = "ECsect571k1";
    break;
  case SEC_OID_SECG_EC_SECT571R1:
    out = "ECsect571r1";
    break;
  case SEC_OID_X509_REASON_CODE:
    out = "revocationReason";
    break;
  default: {
 /*
    if (oidTag == SEC_OID(MS_CERT_EXT_CERTTYPE)) {
      out = "MSCerttype";
      break;
    }
    if (oidTag == SEC_OID(MS_CERTSERV_CA_VERSION)) {
      out = "MSCAVersion";
      break;
    }
    if (oidTag == SEC_OID(PKIX_LOGOTYPE)) {
      out = "Logotype";
      break;
    }
    */

    char* oidchar = CERT_GetOidString(oid);
    SV* oidstr = newSVpv(oidchar, 0);
    PR_smprintf_free(oidchar);
    return oidstr;
    }

  }

  return newSVpvf("%s", out);
}

static void sv_encode(SV* in) {
  if (!sv_utf8_decode(in)) {
    // ok, yep, let's just handle it over to encode.
    dSP;
    int count = 0;

    SV *utf8 = newSVpvn("UTF-8", 5);

    ENTER;
    SAVETMPS;

    PUSHMARK(SP);
    XPUSHs(utf8);
    XPUSHs(in);
    PUTBACK;

    count = call_pv("Encode::encode", G_SCALAR);

    SPAGAIN;

    if ( count != 1 )
      croak("Encode returned something... strange... count = %d", count);

    SV* out = POPs;

    //sv_replace(in, out);
    sv_copypv(in, out);
    //SvREFCNT_dec(out);
    SvREFCNT_dec(utf8);

    PUTBACK;
    FREETMPS;
    LEAVE;

  }
}

static PRInt64 cert_usage_to_certificate_usage(enum SECCertUsageEnum usage) {
  switch(usage) {
    case certUsageSSLClient:
      return certificateUsageSSLClient;
    case certUsageSSLServer:
      return certificateUsageSSLServer;
    case certUsageSSLServerWithStepUp:
      return certificateUsageSSLServerWithStepUp;
    case certUsageSSLCA:
      return certificateUsageSSLCA;
    case certUsageEmailSigner:
      return certificateUsageEmailSigner;
    case certUsageEmailRecipient:
      return certificateUsageEmailRecipient;
    case certUsageObjectSigner:
      return certificateUsageObjectSigner;
    case certUsageUserCertImport:
      return certificateUsageUserCertImport;
    case certUsageVerifyCA:
      return certificateUsageVerifyCA;
    case certUsageProtectedObjectSigner:
      return certificateUsageProtectedObjectSigner;
    case certUsageStatusResponder:
      return certificateUsageStatusResponder;
    case certUsageAnyCA:
      return certificateUsageAnyCA;
    default:
      croak("Unknown certificate usage %d", usage);
  }
}

static HV* node_to_hv(CERTVerifyLogNode* node) {
  HV* out = newHV();

  hv_stores(out, "depth", newSVuv(node->depth)) ? : croak("Could not store data in hv");
  hv_stores(out, "error", newSViv(node->error)) ? : croak("Could not store data in hv");
  if ( node->cert ) {
    SV* cert = newSV(0);

    sv_setref_pv(cert, "Crypt::NSS::X509::Certificate", node->cert); // the beauty of this is that it should be cleaned by perl refcounting now
    hv_stores(out, "certificate", cert)  ? : croak("Could not store data in hv");
  }

  return out;
}



static
SV* item_to_hex(SECItem *data) {
  // do it like mozilla - if data <= 4 -> make integger

  if ( data-> len <=4 ) {
    int i = DER_GetInteger(data);
    return newSViv(i);
  }

  // ok. That was easy. Now let's produce a hex-dump otherwise

  SV* out = newSVpvn("",0);
  for ( unsigned int i = 0; i < data->len; i++ ) {
    sv_catpvf(out, "%02x", data->data[i]);
  }

  return out;
}


static
SV* item_to_hhex(SECItem *data) {
  SV* out = newSVpvn("",0);
  for ( unsigned int i = 0; i < data->len; i++ ) {
    sv_catpvf(out, "%02x", data->data[i]);
  }

  return out;
}

static SECStatus sv_to_item(SV* certSv, SECItem* dst) {
  STRLEN len;
  char *cert;

  cert = SvPV(certSv, len);

  if ( len <= 0 ) {
    return SECFailure;
  }

  dst->len = 0;
  dst->data = NULL;

  dst->data = (unsigned char*)PORT_Alloc(len);
  PORT_Memcpy(dst->data, cert, len);
  dst->len = len;

  return SECSuccess;
}

static SV* item_to_sv(SECItem* item) {
  return newSVpvn((const char*) item->data, item->len);
}

static SV* safe_item_to_sv(SECItem* item) {
 if ( item == NULL || item->data == NULL || item->len == 0 ) {
   return &PL_sv_undef;
 } else {
   return item_to_sv(item);
 }
}

static
bool strip_tag_and_length(SECItem *i)
{
  unsigned int start;

  if (!i || !i->data || i->len < 2) { /* must be at least tag and length */
    return false;
  }
  start = ((i->data[1] & 0x80) ? (i->data[1] & 0x7f) + 2 : 2);
  if (i->len < start) {
    return false;
  }
  i->data += start;
  i->len  -= start;
  return true;
}

static
SV*
universal_to_sv(SECItem* item) {
  switch (item->data[0] & SEC_ASN1_TAGNUM_MASK) {
    case SEC_ASN1_ENUMERATED:
    case SEC_ASN1_INTEGER:
      strip_tag_and_length(item) ? : croak("could not strip tag and length");
      return item_to_hex(item); // item_to_hex returns ints for data <=4
    case SEC_ASN1_OBJECT_ID: {
      strip_tag_and_length(item) ? : croak("could not strip tag and length");
      char* oidchar = CERT_GetOidString(item);
      SV* oidstr = newSVpv(oidchar, 0);
      PR_smprintf_free(oidchar);
      return oidstr;
    }
    case SEC_ASN1_BOOLEAN: {
      int val = 0;
      if ( !strip_tag_and_length(item) ) {
        return &PL_sv_no; // guessing here...
      }
      if ( item->data && item->len ) {
        val = item->data[0];
      }

      if ( val )
        return &PL_sv_yes;
      else
        return &PL_sv_no;
    }
    case SEC_ASN1_UTF8_STRING:
    case SEC_ASN1_PRINTABLE_STRING:
    case SEC_ASN1_VISIBLE_STRING:
    case SEC_ASN1_IA5_STRING:
    case SEC_ASN1_T61_STRING:
      croak("String not implemented");
      break;
    case SEC_ASN1_GENERALIZED_TIME:
      croak("Time not implemented");
      break;
    case SEC_ASN1_UTC_TIME:
      croak("UTCTime not implemented");
      break;
    case SEC_ASN1_NULL:
      return &PL_sv_undef;
     case SEC_ASN1_SET:
     case SEC_ASN1_SEQUENCE:
      croak("SET, Sequence not implemented");
      break;
    case SEC_ASN1_OCTET_STRING:
      croak("Octet string not implemented");
      break;
    case SEC_ASN1_BIT_STRING:
      croak("Bit string not implemented");
      break;
    case SEC_ASN1_BMP_STRING:
      croak("BMP String not implemented");
      break;
    case SEC_ASN1_UNIVERSAL_STRING:
      croak("Universal String not implemented");
      break;
    default:
      return item_to_hhex(item);
      break;
  }

  return 0;
}

static
SV* item_to_sv_parsed(SECItem* i) {
  if ( !( i && i->len && i->data ) ) {
    croak("Empty item");
  }

  switch ( i->data[0] & SEC_ASN1_CLASS_MASK ) {
    case SEC_ASN1_CONTEXT_SPECIFIC:
     croak("Cannot parse context specific");
    case SEC_ASN1_UNIVERSAL:
      return universal_to_sv(i);
    default:
      return item_to_hhex(i);
  }

}

static
SV* extension_to_sv(CERTCertExtension *extension) {
  SECOidTag oidTag;
  SV* out = 0;

  oidTag = SECOID_FindOIDTag (&extension->id);


  switch ( oidTag ) {
    case SEC_OID_X509_REASON_CODE:
      return item_to_sv_parsed(&extension->value);

    default: {
      char* oidchar = CERT_GetOidString(&extension->id);
      croak("Could not parse extension %s", oidchar);
      PR_smprintf_free(oidchar); // memleak because this is never executed"
    }
  }

  return out;
}

MODULE = Crypt::NSS::X509    PACKAGE = Crypt::NSS::X509

PROTOTYPES: DISABLE

BOOT:
{
  HV *stash = gv_stashpvn("Crypt::NSS::X509", 16, TRUE);

  struct { char *n; I32 s; } Crypt__NSS__X509__const[] = {

  {"certUsageSSLClient", certUsageSSLClient},
  {"certUsageSSLServer", certUsageSSLServer},
  {"certUsageSSLServerWithStepUp", certUsageSSLServerWithStepUp},
  {"certUsageSSLCA", certUsageSSLCA},
  {"certUsageEmailSigner", certUsageEmailSigner},
  {"certUsageEmailRecipient", certUsageEmailRecipient},
  {"certUsageObjectSigner", certUsageObjectSigner},
  {"certUsageUserCertImport", certUsageUserCertImport},
  {"certUsageVerifyCA", certUsageVerifyCA},
  {"certUsageProtectedObjectSigner", certUsageProtectedObjectSigner},
  {"certUsageStatusResponder",  certUsageStatusResponder},
  {"certUsageAnyCA",  certUsageAnyCA},
  {Nullch,0}
  };

  char *name;
  int i;

  for (i = 0; (name = Crypt__NSS__X509__const[i].n); i++) {
    newCONSTSUB(stash, name, newSViv(Crypt__NSS__X509__const[i].s));
  }


  PR_Init( PR_SYSTEM_THREAD, PR_PRIORITY_NORMAL, 1);

  //SECU_RegisterDynamicOids();
}

void
_init_nodb()

  PREINIT:
  SECStatus secStatus;
  //PRUint32 initFlags;

  CODE:
  //initFlags = NSS_INIT_NOCERTDB | NSS_INIT_NOMODDB | NSS_INIT_NOROOTINIT;

  //secStatus = NSS_Initialize("test2", "", "", SECMOD_DB, initFlags);
  secStatus = NSS_NoDB_Init(NULL);
  initstring = NULL;
  //SECMOD_AddNewModule("Builtins", DLL_PREFIX"nssckbi."DLL_SUFFIX, 0, 0);

  if (secStatus != SECSuccess) {
    PRErrorCode err = PR_GetError();
    croak("NSS_NoDB_Init failed: %d = %s\n",
                 err, PORT_ErrorToString(err));
  }

  initialized = 1;


void
_init_db(string)
  SV* string;

  PREINIT:
  SECStatus secStatus;
  char* path;
  STRLEN pathlen;

  CODE:
  path = SvPV(string, pathlen);

  secStatus = NSS_InitReadWrite(path);
  initstring = (char*) malloc(pathlen+1);
  bzero(initstring, pathlen+1);
  memcpy(initstring, path, pathlen);

  //SECMOD_AddNewModule("Builtins", DLL_PREFIX"nssckbi."DLL_SUFFIX, 0, 0);

  if (secStatus != SECSuccess) {
    PRErrorCode err = PR_GetError();
    croak("NSS Init failed: %d = %s\n",
                 err, PORT_ErrorToString(err));
  }

  initialized = 1;


void
__cleanup(void)

  PREINIT:
  SECStatus rv;

  CODE:

  if ( !initialized ) {
    // no init, no shutdown
    return;
  }

  rv = NSS_Shutdown();

  if (rv != SECSuccess) {
    PRErrorCode err = PR_GetError();
    croak( "NSS Shutdown failed %d = %s\n",
           err, PORT_ErrorToString(err));
  }
  //printf("Destroy was happy\n");

void
__add_builtins(string)
  SV* string;

  PREINIT:
  char* path;
  SECStatus rv;

  CODE:

  path = SvPV_nolen(string);

  rv = SECMOD_AddNewModule("Builtins", path, 0, 0);  // nssckbi
  if (rv != SECSuccess) {
    PRErrorCode err = PR_GetError();
    croak( "could not add certificate to db %d = %s\n",
           err, PORT_ErrorToString(err));
  }

SV*
add_cert_to_db(cert, string)
  Crypt::NSS::X509::Certificate cert;
  SV* string;

  ALIAS:
  add_trusted_cert_to_db = 1

  PREINIT:
  PK11SlotInfo *slot = NULL;
  CERTCertTrust *trust = NULL;
  CERTCertDBHandle *defaultDB;
  SECStatus rv;
  char* nick;

  CODE:
  RETVAL = 0;
  nick = SvPV_nolen(string);

  defaultDB = CERT_GetDefaultCertDB();

  slot = PK11_GetInternalKeySlot();

  if ( ix == 1 ) {
    // trusted Certificate

    trust = (CERTCertTrust *)PORT_ZAlloc(sizeof(CERTCertTrust));
    if (!trust) {
      croak("Could not create trust");
    }

    rv = CERT_DecodeTrustString(trust, "TCu,Cu,Tu"); // take THAT trust ;)
    if (rv) {
      croak("unable to decode trust string");
    }
  }

  rv = PK11_ImportCert(slot, cert, CK_INVALID_HANDLE, nick, PR_FALSE);
  if (rv != SECSuccess) {
    PRErrorCode err = PR_GetError();
    croak( "could not add certificate to db %d = %s\n",
           err, PORT_ErrorToString(err));
  }

  if ( ix == 1 ) {
    rv = CERT_ChangeCertTrust(defaultDB, cert, trust);
    if (rv != SECSuccess) {
      croak("Could not change cert trust");
    }
  }

  PORT_Free(trust);

  PK11_FreeSlot(slot);

  RETVAL = newSViv(1);

  OUTPUT:
  RETVAL


void
_reinit()

  PREINIT:
  SECStatus rv;

  CODE:

  rv = NSS_Shutdown();

  if (rv != SECSuccess) {
    PRErrorCode err = PR_GetError();
    croak( "NSS Shutdown failed during reinit. Last error-code: %d = %s\n", err, PORT_ErrorToString(err));
  }


  if ( initstring == NULL ) {
    rv = NSS_NoDB_Init(NULL);
  } else {
    //printf("%s\n\n", initstring);
    rv = NSS_InitReadWrite(initstring);
  }

  if (rv != SECSuccess) {
    PRErrorCode err = PR_GetError();
    croak("NSS Init failed: %d = %s\n",
    err, PORT_ErrorToString(err));
  }



void
dump_certificate_cache_info()

  CODE:
  nss_DumpCertificateCacheInfo();



MODULE = Crypt::NSS::X509    PACKAGE = Crypt::NSS::X509::CRL

Crypt::NSS::X509::CRL
new_from_der(class, string)
  SV* string

  PREINIT:
  SECItem item;
  CERTSignedCrl *signedCrl;
  SECStatus rv;

  CODE:
  rv = sv_to_item(string, &item);
  if (rv != SECSuccess) {
    croak("sv_to_item failed");
  }

  //PRInt32 decodeOptions = CRL_DECODE_DEFAULT_OPTIONS;

  signedCrl = CERT_DecodeDERCrlWithFlags(NULL, &item, SEC_CRL_TYPE, CRL_DECODE_DEFAULT_OPTIONS);

  if ( !signedCrl ) {
    PRErrorCode err = PR_GetError();
    croak( "Could not decode CRL %d = %s\n",
           err, PORT_ErrorToString(err));
  }

  RETVAL = signedCrl;

  OUTPUT:
  RETVAL


SV*
issuer(crl)
  Crypt::NSS::X509::CRL crl

  PREINIT:
  char* c;

  CODE:
  c = CERT_DerNameToAscii(&crl->crl.derName);

  RETVAL = newSVpvf("%s", c);

  PORT_Free(c);

  OUTPUT:
  RETVAL

SV*
version(crl)
  Crypt::NSS::X509::CRL crl

  PREINIT:
  int version;

  CODE:
  version = crl->crl.version.len ? DER_GetInteger(&crl->crl.version) : 0;
  version++;
  RETVAL = newSViv(version);

  OUTPUT:
  RETVAL


void
verify(crl, cert, timedouble = NO_INIT)
  Crypt::NSS::X509::CRL crl
  Crypt::NSS::X509::Certificate cert
  SV* timedouble

  PREINIT:
  SECStatus rv;
  PRTime time = 0;

  PPCODE:
  if ( items < 3 || SvIV(timedouble) == 0 ) {
    time = PR_Now();
  } else {
    double tmptime = SvNV(timedouble);
    // time contains seconds since epoch - netscape expects microseconds
    tmptime = tmptime * 1000000;
    LL_D2L(time, tmptime); // and convert to 64-bit int
  }

  rv = CERT_VerifySignedData(&crl->signatureWrap, cert, time, NULL);

  if ( rv != SECSuccess ) {
    XSRETURN_NO;
  }

  XSRETURN_YES;

Crypt::NSS::X509::Certificate
find_issuer(crl, timedouble = NO_INIT)
  Crypt::NSS::X509::CRL crl
  SV* timedouble

  ALIAS:
  verify_db = 1

  PREINIT:
  PRTime time = 0;
  CERTCertDBHandle *defaultDB;
  SECItem* subject = NULL;
  CERTCertificate* cert;
  SECStatus rv;

  CODE:
  if ( items < 2 || SvIV(timedouble) == 0 ) {
    time = PR_Now();
  } else {
    double tmptime = SvNV(timedouble);
    // time contains seconds since epoch - netscape expects microseconds
    tmptime = tmptime * 1000000;
    LL_D2L(time, tmptime); // and convert to 64-bit int
  }

  defaultDB = CERT_GetDefaultCertDB();

  subject = &crl->crl.derName;
  cert = FindCrlIssuer(defaultDB, subject, time);

  if ( !cert ) {
    XSRETURN_UNDEF;
  }

  if ( ix == 1 ) {
    rv = CERT_VerifySignedData(&crl->signatureWrap, cert, time, NULL);
    CERT_DestroyCertificate(cert);

    if ( rv != SECSuccess ) {
      XSRETURN_NO;
    }

    XSRETURN_YES;
  }


  RETVAL = cert;

  OUTPUT:
  RETVAL

void
DESTROY(crl)
  Crypt::NSS::X509::CRL crl

  PPCODE:

  if ( crl ) {
    SEC_DestroyCrl(crl);
    crl = 0;
  }


void
entries(crl)
  Crypt::NSS::X509::CRL crl

  PREINIT:
  CERTCrlEntry *entry;
  int iv;

  PPCODE:
  if ( crl->crl.entries != NULL ) {
    iv = 0;
    while( (entry = crl->crl.entries[iv++]) != NULL) {
      HV* h = newHV();

      hv_store(h, "serial", 6, item_to_hhex(&entry->serialNumber), 0) ? : croak("Could not store data in hv");

      {
        // TODO: factor out to time_to_sv
        int64 time;
        SECStatus rv;
        char *timeString;
        PRExplodedTime printableTime;
	SV* timeSV;

	rv = DER_UTCTimeToTime(&time, &(entry->revocationDate));

        if (rv != SECSuccess)
          croak("Could not parse CRL element revocation time");

        PR_ExplodeTime(time, PR_GMTParameters, &printableTime);
        timeString = PORT_Alloc(256);
        if ( ! PR_FormatTimeUSEnglish(timeString, 256, "%a %b %d %H:%M:%S %Y", &printableTime) ) {
          croak("Could not format time string");
        }

        timeSV = newSVpvf("%s", timeString);

        hv_store(h, "revocationDate", 14, timeSV, 0) ? : croak("Could not store data in hv");
      }

      {
        CERTCertExtension **extensions = entry->extensions;
	if ( extensions ) {
	  while ( *extensions ) {
	    hv_store_ent(h, oid_to_sv(&(*extensions)->id), extension_to_sv(*extensions), 0) ? : croak("Could not store data in hv");

	    extensions++;
	  }
	}

      }

      mXPUSHs(newRV ((SV*)h) );
    }
  }


MODULE = Crypt::NSS::X509    PACKAGE = Crypt::NSS::X509::CertList

Crypt::NSS::X509::CertList
new(class)

  PREINIT:
  CERTCertList *certList;

  CODE:
  certList = CERT_NewCertList();

  RETVAL = certList;

  OUTPUT:
  RETVAL

void
add(certlist, cert)
  Crypt::NSS::X509::CertList certlist;
  Crypt::NSS::X509::Certificate cert;

  CODE:
  CERTCertificate* addcert = CERT_DupCertificate(cert);
  CERT_AddCertToListTail(certlist, addcert);


void
dump(certlist)
  Crypt::NSS::X509::CertList certlist;

  PREINIT:
  CERTCertListNode *node;

  PPCODE:

  node = CERT_LIST_HEAD(certlist);

  while ( !CERT_LIST_END(node, certlist) ) {
    if ( node->cert ) {
      CERTCertificate* currcert = CERT_DupCertificate(node->cert);
      SV* cert = newSV(0);

      sv_setref_pv(cert, "Crypt::NSS::X509::Certificate", currcert); // the beauty of this is that it should be cleaned by perl refcounting now
      mXPUSHs(cert);
    } else {
      croak("Bad certificate list, encountered node without cert.");
    }

    node = CERT_LIST_NEXT(node);
  }


void
DESTROY(certlist)
  Crypt::NSS::X509::CertList certlist;

  PPCODE:

  if ( certlist ) {
    CERT_DestroyCertList(certlist);
    certlist = 0;
  }


MODULE = Crypt::NSS::X509    PACKAGE = Crypt::NSS::X509::Certificate


SV*
curve(cert)
  Crypt::NSS::X509::Certificate cert

  PREINIT:
  SECKEYPublicKey *key;
  SECOidTag tag;

  CODE:

  // This function extracts the curve from a NSS Certificate
  // As you will see, this is one of the moist straightforward
  // things to do. Totally easy. Everyone can do that. And find
  // out. Only took like 10 minutes.
  // Srsly.

  SECItem oid = { siBuffer, NULL, 0};

  // this is the complicated part
  key = SECKEY_ExtractPublicKey(&cert->subjectPublicKeyInfo);
  if ( key->keyType != ecKey ) {
    SECKEY_DestroyPublicKey(key);
    croak("Only ec-keys have a curve");
  }

  // ok, now it gets simple.
  oid.len = key->u.ec.DEREncodedParams.len - 2;
  oid.data = key->u.ec.DEREncodedParams.data + 2;
  if ((key->u.ec.DEREncodedParams.data[0] != SEC_ASN1_OBJECT_ID) ||
        ((tag = SECOID_FindOIDTag(&oid)) == SEC_OID_UNKNOWN)) {
    SECKEY_DestroyPublicKey(key);
    croak("Unknown EC-key");
  }

  RETVAL = oid_to_sv(&oid);

  SECKEY_DestroyPublicKey(key);

  OUTPUT:
  RETVAL

SV*
raw_spki(cert)
  Crypt::NSS::X509::Certificate cert

  PREINIT:
  SV* out;

  CODE:
  out = item_to_sv(&cert->derPublicKey);

  RETVAL = out;

  OUTPUT:
  RETVAL

SV*
bit_length(cert)
  Crypt::NSS::X509::Certificate cert

  ALIAS:
  modulus = 1
  public_key = 1
  exponent = 2

  PREINIT:
  SECKEYPublicKey *key;

  CODE:
  key = SECKEY_ExtractPublicKey(&cert->subjectPublicKeyInfo);

  if ( key ) {
    switch(key->keyType) {
    case rsaKey: {

      if ( ix == 0 ) {
        RETVAL = newSViv(key->u.rsa.modulus.len * 8);
      } else if ( ix == 1 ) {
        RETVAL = item_to_hex(&key->u.rsa.modulus);
      } else if ( ix == 2 ) {
        RETVAL = item_to_hex(&key->u.rsa.publicExponent);
      } else {
        SECKEY_DestroyPublicKey(key);
        croak("Internal error");
      }

      break;
    }
    case ecKey: {
      if ( ix == 0 ) {
        int len = SECKEY_ECParamsToKeySize(&key->u.ec.DEREncodedParams);
        RETVAL = newSViv(len);
      } else if ( ix == 1 ) {
        RETVAL = item_to_hex(&key->u.ec.publicValue);
      } else {
        SECKEY_DestroyPublicKey(key);
        croak("EC certificates do not have exponent");
      }

      break;
    }
    case dsaKey: {
      if ( ix == 0 ) {
        RETVAL = newSViv(key->u.dsa.publicValue.len * 8);
      } else if ( ix == 1 ) {
        RETVAL = item_to_hex(&key->u.dsa.publicValue);
      } else {
        SECKEY_DestroyPublicKey(key);
        croak("DSA certificates do not have exponent");
      }
      break;
    }
    default:
      croak("Unknown key type %d", key->keyType);
    }

    SECKEY_DestroyPublicKey(key);
  } else {
    XSRETURN_UNDEF;
  }

  OUTPUT:
  RETVAL


SV*
fingerprint_md5(cert)
  Crypt::NSS::X509::Certificate cert

  ALIAS:
  fingerprint_sha1 = 1
  fingerprint_sha256 = 2

  PREINIT:
  SECItem item;
  SECStatus rv;
  unsigned char fingerprint[32];

  CODE:

  memset(fingerprint, 0, sizeof(fingerprint));
  if ( ix == 0 ) {
    rv = PK11_HashBuf(SEC_OID_MD5, fingerprint, cert->derCert.data, cert->derCert.len);
    item.len = MD5_LENGTH;
  } else if ( ix == 1 ) {
    rv = PK11_HashBuf(SEC_OID_SHA1, fingerprint, cert->derCert.data, cert->derCert.len);
    item.len = SHA1_LENGTH;
  } else if ( ix == 2 ) {
    rv = PK11_HashBuf(SEC_OID_SHA256, fingerprint, cert->derCert.data, cert->derCert.len);
    item.len = SHA256_LENGTH;
  } else {
    croak("Internal error - unknown function");
  }

  if ( rv != SECSuccess ) {
    croak("Could not calculate fingerprint");
  }

  item.data = fingerprint;
  RETVAL = item_to_hhex(&item);

  OUTPUT:
  RETVAL

AV*
extension_oids(cert)
  Crypt::NSS::X509::Certificate cert

  PREINIT:

  CODE:
  RETVAL = newAV();

  if (!cert->extensions) {
	XSRETURN_UNDEF;
  }

  for ( int i = 0; cert->extensions[i] != NULL; i++ ) {
    const SECItem *oid = &cert->extensions[i]->id;
    char* oidstr = CERT_GetOidString(oid);
    SV* oidsv = newSVpv(oidstr, 0);
    av_push(RETVAL, oidsv);
  }


  OUTPUT:
  RETVAL


HV*
policy(cert)
  Crypt::NSS::X509::Certificate cert

  PREINIT:
    SECStatus rv;
    SECItem           cps;
    CERTCertificatePolicies *policies;
    CERTPolicyInfo **policyInfos, *policyInfo;
    CERTPolicyQualifier **policyQualifiers, *policyQualifier;

  CODE:
    rv = CERT_FindCertExtension(cert, SEC_OID_X509_CERTIFICATE_POLICIES, &cps);
    if (rv != SECSuccess) {
      XSRETURN_NO;
    }

    policies = CERT_DecodeCertificatePoliciesExtension(&cps);
    if ( !policies )
      XSRETURN_NO;

    RETVAL = newHV();
    sv_2mortal((SV*)RETVAL);

    policyInfos = policies->policyInfos;
    while ( *policyInfos != NULL ) {
      policyInfo = *policyInfos++;
      switch (policyInfo->oid) {
      case SEC_OID_VERISIGN_USER_NOTICES:
        assert(false); // implement me.
	break;
      default: {

        char* policyoid = CERT_GetOidString(&policyInfo->policyID);
	SV* policySV = newSVpv(policyoid, 0);
	if ( !hv_store(RETVAL, "policy_oid", 10, policySV, 0) ) croak("Error storing data in hash");
	break;
	}
      }

      if ( policyInfo->policyQualifiers) {
        AV* qualifiers = newAV();
	if ( !hv_store(RETVAL, "qualifiers", 10, newRV_noinc((SV*)qualifiers), 0) ) croak("Error storing data in hash");
        policyQualifiers = policyInfo->policyQualifiers;
	while ( *policyQualifiers != NULL ) {
	  policyQualifier = *policyQualifiers++;
	  HV* qualifier = newHV();
	  av_push(qualifiers, newRV_noinc((SV*)qualifier));
	  char* policyoid = CERT_GetOidString(&policyQualifier->qualifierID);
          SV* policySV = newSVpv(policyoid, 0);
	  if ( !hv_store(qualifier, "oid", 3, policySV, 0)) croak("Error storing data in hash");
	  switch (policyQualifier->oid) {
	    case SEC_OID_PKIX_CPS_POINTER_QUALIFIER: {
	    if ( !hv_store(qualifier, "type", 4, newSVpv("Certification Practice Statement", 0), 0) ) croak("Error storing data in hash");
	    if ( !hv_store(qualifier, "value", 5, item_to_sv(&policyQualifier->qualifierValue), 0) ) croak("Error storing data in hash");

	    break;
	    }
	    default:{
	    }
	  }
	}
      }
    }

  OUTPUT:
  RETVAL

SV*
subj_alt_name(cert)
  Crypt::NSS::X509::Certificate cert

  PREINIT:
    SECStatus rv;
    SECItem           subAltName;
    CERTGeneralName * nameList;
    CERTGeneralName * current;
    PRArenaPool *     arena          = NULL;

  CODE:
    SV* out = newSVpvn("", 0);

    rv = CERT_FindCertExtension(cert, SEC_OID_X509_SUBJECT_ALT_NAME, &subAltName);

    if (rv != SECSuccess) {
      XSRETURN_NO;
    }

    arena = PORT_NewArena(DER_DEFAULT_CHUNKSIZE);
    if ( !arena )
      croak("Could not create arena");

    nameList = current = CERT_DecodeAltNameExtension(arena, &subAltName);
    if(!current)
      croak("No namelist");

    bool first = true;
    do {
  switch (current->type) {
  case certDNSName:
            {
            if ( !first )
		sv_catpv(out, ",");
	    else
		first = false;
            sv_catpv(out, "DNS:");
      sv_catpvn(out, (const char*) current->name.other.data, current->name.other.len);
      break;
            }
  case certIPAddress:
	    if ( !first )
            	sv_catpv(out, ",");
	    else
		first = false;
      sv_catpv(out, "IP:");
      sv_catpvn(out, (const char*) current->name.other.data, current->name.other.len);
      break;
  default:
      // simply ignore for now - more or less like firefox
      //sv_catpv(out, "UnknownElement,");
      break;
  }
  current = CERT_GetNextGeneralName(current);
    } while (current != nameList);

    RETVAL = out;

    if (arena) {
      PORT_FreeArena(arena, PR_FALSE);
    }

    if (subAltName.data) {
      SECITEM_FreeItem(&subAltName, PR_FALSE);
    }

  OUTPUT:
  RETVAL


SV*
subject(cert)
  Crypt::NSS::X509::Certificate cert

  ALIAS:
  issuer = 2
  serial = 3
  notBefore = 5
  notAfter = 6
#  email = 7
  version = 8
  common_name = 10
  is_root = 11
  sig_alg_name = 12
  key_alg_name = 13
  nickname = 14
  dbnickname = 15
  der = 16
  country_name = 17
  org_name = 18
  org_unit_name = 19
  locality_name = 20
  state_name = 21
  business_category = 40
  ev_incorporation_locality = 41
  ev_incorporation_state = 42
  ev_incorporation_country = 43
  subject_serial = 44
  street_address = 45
  locality = 46
  postal_code = 47


  PREINIT:

  CODE:

  if ( ix == 0 ) {
    RETVAL = newSVpvf("%s", cert->subjectName);
  } else if ( ix == 2 ) {
    RETVAL = newSVpvf("%s", cert->issuerName);
  } else if ( ix == 3 ) {
    RETVAL = item_to_hhex(&cert->serialNumber);
  } else if ( ix == 16 ) {
    RETVAL = item_to_sv(&cert->derCert);
  //} else if ( ix == 7 ) { Function is not provided in all nss versions :(
    // char * ce = CERT_GetCertificateEmailAddress(cert);
    //if ( ce == NULL )
    //  XSRETURN_UNDEF;
    //RETVAL = newSVpvf("%s", ce);
    // PORT_Free(ce);
  } else if ( ix == 14 ) {
    RETVAL = newSVpvf("%s", cert->nickname);
  } else if ( ix == 15 ) {
    RETVAL = newSVpvf("%s", cert->dbnickname);
  } else if ( ix == 10 || ( ix >= 17 && ix <= 21 ) ) {
    char * cn = NULL;
    switch ( ix ) {
    	case 10:
		cn = CERT_GetCommonName(&cert->subject);
		break;
	case 17:
    		cn = CERT_GetCountryName(&cert->subject);
		break;
	case 18:
    		cn = CERT_GetOrgName(&cert->subject);
		break;
	case 19:
    		cn = CERT_GetOrgUnitName(&cert->subject);
		break;
	case 20:
		cn = CERT_GetLocalityName(&cert->subject);
		break;
	case 21:
		cn = CERT_GetStateName(&cert->subject);
		break;
	default:
		croak("Internal error");
	}
	
    if ( !cn )
      XSRETURN_UNDEF;
    SV* out = newSVpvf("%s", cn);
    PORT_Free(cn);
    sv_encode(out);
    RETVAL = out;
  } else if ( ix == 11 ) {
    if ( cert->isRoot == PR_TRUE ) {
      XSRETURN_YES;
    } else {
      XSRETURN_NO;
    }
  } else if ( ix == 40 ) {
    RETVAL = safe_item_to_sv(CERT_GetNameElement(NULL, &cert->subject, SEC_OID_BUSINESS_CATEGORY));
  } else if ( ix == 41 ) {
    RETVAL = safe_item_to_sv(CERT_GetNameElement(NULL, &cert->subject, SEC_OID_EV_INCORPORATION_LOCALITY));
  } else if ( ix == 42 ) {
    RETVAL = safe_item_to_sv(CERT_GetNameElement(NULL, &cert->subject, SEC_OID_EV_INCORPORATION_STATE));
  } else if ( ix == 43 ) {
    RETVAL = safe_item_to_sv(CERT_GetNameElement(NULL, &cert->subject, SEC_OID_EV_INCORPORATION_COUNTRY));
  } else if ( ix == 44 ) {
    RETVAL = safe_item_to_sv(CERT_GetNameElement(NULL, &cert->subject, SEC_OID_AVA_SERIAL_NUMBER));
  } else if ( ix == 45 ) {
    RETVAL = safe_item_to_sv(CERT_GetNameElement(NULL, &cert->subject, SEC_OID_AVA_POSTAL_ADDRESS));
  } else if ( ix == 46 ) {
    RETVAL = safe_item_to_sv(CERT_GetNameElement(NULL, &cert->subject, SEC_OID_AVA_LOCALITY));
  } else if ( ix == 47 ) {
    RETVAL = safe_item_to_sv(CERT_GetNameElement(NULL, &cert->subject, SEC_OID_AVA_POSTAL_CODE));
  } else if ( ix == 12 ) {
    RETVAL = oid_to_sv(&cert->signature.algorithm);
  } else if ( ix == 13 ) {
    RETVAL = oid_to_sv(&cert->subjectPublicKeyInfo.algorithm.algorithm);
  } else if ( ix == 5 || ix == 6 ) {
    int64 time;
    SECStatus rv;
    char *timeString;
    PRExplodedTime printableTime;

    if ( ix == 5 )
      rv = DER_UTCTimeToTime(&time, &cert->validity.notBefore);
    else if ( ix == 6 )
      rv = DER_UTCTimeToTime(&time, &cert->validity.notAfter);
    else
      croak("not possible");

    if (rv != SECSuccess)
      croak("Could not parse time");

    PR_ExplodeTime(time, PR_GMTParameters, &printableTime);
    timeString = PORT_Alloc(256);
    if ( ! PR_FormatTimeUSEnglish(timeString, 256, "%a %b %d %H:%M:%S %Y", &printableTime) ) {
      croak("Could not format time string");
    }

    RETVAL = newSVpvf("%s", timeString);
    PORT_Free(timeString);
  } else if ( ix == 8 ) {
    // if version is not specified it it 1 (0).
    int version = cert->version.len ? DER_GetInteger(&cert->version) : 0;
    RETVAL = newSViv(version+1);
  } else {
    croak("Unknown accessor %d", ix);
  }


  OUTPUT:
  RETVAL

void
verify_certificate(cert, timedouble = NO_INIT, usage = certUsageSSLServer)
  Crypt::NSS::X509::Certificate cert;
  SV* timedouble;
  I32 usage;

  ALIAS:
  verify_certificate_pkix = 1
  verify_cert = 2
  verify_certificate_log = 3
  verify_certificate_pkix_log = 4
  verify_cert_log = 5

  PREINIT:
  SECStatus secStatus;
  PRTime time = 0;
  CERTVerifyLog log;
  CERTCertDBHandle *defaultDB;

  PPCODE:
  defaultDB = CERT_GetDefaultCertDB();

  if ( items == 1 || SvIV(timedouble) == 0 ) {
    time = PR_Now();
  } else {
    double tmptime = SvNV(timedouble);
    // time contains seconds since epoch - netscape expects microseconds
    tmptime = tmptime * 1000000;
    LL_D2L(time, tmptime); // and convert to 64-bit int
  }

  if ( ix == 1 || ix == 4 )
    CERT_SetUsePKIXForValidation(PR_TRUE);
  else
    CERT_SetUsePKIXForValidation(PR_FALSE);


  log.arena = PORT_NewArena(512);
  log.head = log.tail = NULL;
  log.count = 0;

  if ( ix == 2 ) {
    secStatus = CERT_VerifyCert(defaultDB, cert,
               PR_TRUE, // check sig
               usage,
               time,
               NULL,
               NULL);
  } else if ( ix == 5) { // supplying log changes the return value
    secStatus = CERT_VerifyCert(defaultDB, cert,
               PR_TRUE, // check sig
               usage,
               time,
               NULL,
               &log);
  } else {
    secStatus = CERT_VerifyCertificate(defaultDB, cert,
               PR_TRUE, // check sig
               cert_usage_to_certificate_usage(usage),
               time,
               NULL,
               &log, NULL);
  }


  if ( ix <= 2 ) { // no log
    for (CERTVerifyLogNode *node = log.head; node; node = node->next) {
      if (node->cert)
        CERT_DestroyCertificate(node->cert);
    }

    PORT_FreeArena(log.arena, PR_FALSE);

    if (secStatus != SECSuccess ) {
      ST(0) = newSViv(PR_GetError());
    } else {
      ST(0) = newSViv(1); // return 1 on success
    }

    sv_2mortal(ST(0));
    XSRETURN(1);

  } else {
    for (CERTVerifyLogNode *node = log.head; node; node = node->next) {
      HV* out = node_to_hv(node);
      SV* rv = newRV_noinc((SV*) out);
      mXPUSHs(rv);
    }

    PORT_FreeArena(log.arena, PR_FALSE);

  }


SV* match_name(cert, string)
  Crypt::NSS::X509::Certificate cert;
  SV* string;

  PREINIT:
  char* hostname;
  SECStatus secStatus;

  CODE:
  hostname = SvPV_nolen(string);

  secStatus = CERT_VerifyCertName(cert, hostname);

  if ( secStatus != SECSuccess ) {
    RETVAL = &PL_sv_no;
  } else {
    RETVAL = &PL_sv_yes;
  }

  OUTPUT:
  RETVAL

SV*
verify_pkix(cert, timedouble = NO_INIT, usage = certUsageSSLServer, trustedCertList = NO_INIT)
  Crypt::NSS::X509::Certificate cert;
  SV* timedouble;
  I32 usage;
  Crypt::NSS::X509::CertList trustedCertList;

  ALIAS:
  verify_pkix_aia = 1

  PREINIT:
  SECStatus secStatus;
  PRBool certFetching = PR_FALSE; // automatically get AIA certs

  static CERTValOutParam cvout[4];
  static CERTValInParam cvin[6];
  int inParamIndex = 0;
  static CERTRevocationFlags rev;
  static PRUint64 revFlagsLeaf[2];
  static PRUint64 revFlagsChain[2];
  CERTVerifyLog log;

  CODE:

  if ( ix == 1 ) {
    certFetching = PR_TRUE;
  }

  cvin[inParamIndex].type = cert_pi_useAIACertFetch;
  cvin[inParamIndex].value.scalar.b = certFetching;
  inParamIndex++;

  rev.leafTests.cert_rev_flags_per_method = revFlagsLeaf;
  rev.chainTests.cert_rev_flags_per_method = revFlagsChain;
  secStatus = configureRevocationParams(&rev);

  if (secStatus) {
    croak("Can not configure revocation parameters");
  }

  cvin[inParamIndex].type = cert_pi_revocationFlags;
  cvin[inParamIndex].value.pointer.revocation = &rev;
  inParamIndex++;

  if ( items >= 2 && SvIV(timedouble) > 0 ) {
    PRTime time;
    double tmptime = SvNV(timedouble);
    // time contains seconds since epoch - netscape expects microseconds
    tmptime = tmptime * 1000000;
    LL_D2L(time, tmptime); // and convert to 64-bit int
    cvin[inParamIndex].type = cert_pi_date;
    cvin[inParamIndex].value.scalar.time = time;
    inParamIndex++;
  }
  if ( items == 4 ) {
    // we have a trustedCertList
    cvin[inParamIndex].type = cert_pi_trustAnchors;
    cvin[inParamIndex].value.pointer.chain = trustedCertList;
    inParamIndex++;
  }

  cvin[inParamIndex].type = cert_pi_end;

  // Initialize log
  log.arena = PORT_NewArena(512);
  log.head = log.tail = NULL;
  log.count = 0;

  /* cvout[0].type = cert_po_trustAnchor;
  cvout[0].value.pointer.cert = NULL;
  cvout[1].type = cert_po_certList;
  cvout[1].value.pointer.chain = NULL;
  cvout[2].type = cert_po_errorLog;
  cvout[2].value.pointer.log = &log; */
  cvout[0].type = cert_po_end;

  secStatus = CERT_PKIXVerifyCert(cert, cert_usage_to_certificate_usage(usage),
                                  cvin, cvout, NULL);


  if (secStatus != SECSuccess ) {
    RETVAL = newSViv(PR_GetError()); // return error code
  } else {
    /* CERTCertificate* issuerCert = cvout[0].value.pointer.cert;
    CERTCertList* builtChain = cvout[1].value.pointer.chain;

    CERT_DestroyCertList(builtChain);
    CERT_DestroyCertificate(issuerCert); */

    RETVAL = newSViv(1);
  }

  // destroy refs in the log
  for (CERTVerifyLogNode *node = log.head; node; node = node->next) {
    if (node->cert)
      CERT_DestroyCertificate(node->cert);
  }

  PORT_FreeArena(log.arena, PR_FALSE);

  OUTPUT:
  RETVAL


Crypt::NSS::X509::CertList
get_cert_chain_from_cert(cert, timedouble = NO_INIT, usage = certUsageSSLServer)
  Crypt::NSS::X509::Certificate cert;
  I32 usage;
  SV* timedouble;

  PREINIT:
  CERTCertList* list = NULL;
  PRTime time;

  CODE:

  if ( items == 1 || SvIV(timedouble) == 0 ) {
    time = PR_Now();
  } else {
    double tmptime = SvNV(timedouble);
    // time contains seconds since epoch - netscape expects microseconds
    tmptime = tmptime * 1000000;
    LL_D2L(time, tmptime); // and convert to 64-bit int
  }

  list = CERT_GetCertChainFromCert(cert, time, usage);

  if ( list == NULL ) {
    XSRETURN_UNDEF;
  }

  RETVAL = list;

  OUTPUT:
  RETVAL

Crypt::NSS::X509::Certificate
new(class, string, nickSv = NO_INIT)
  SV  *string
  SV  *nickSv

  PREINIT:
  CERTCertificate *cert;
  CERTCertDBHandle *defaultDB;
  //PRFileDesc*     fd;
  SECStatus       rv;
  SECItem         item        = {0, NULL, 0};
  char* nick = NULL;

  CODE:
 // Note: nick functionality seems to not really work in NSS

  if ( items == 3 ) {
    nick = SvPV_nolen(nickSv);
  }

  defaultDB = CERT_GetDefaultCertDB();
  rv = sv_to_item(string, &item);
  if (rv != SECSuccess) {
    croak("sv_to_item failed");
  }

  cert = CERT_NewTempCertificate(defaultDB, &item,
                                   nick     /* nickname */,
                                   PR_FALSE /* isPerm */,
           			   PR_TRUE  /* copyDER */);

  if (!cert) {
    PRErrorCode err = PR_GetError();
    croak( "couldn't import certificate %d = %s\n",
           err, PORT_ErrorToString(err));
  }
  PORT_Free(item.data);

  RETVAL = cert;

  OUTPUT:
  RETVAL

Crypt::NSS::X509::Certificate
new_from_nick(class, string)
  SV* string

  PREINIT:
  CERTCertDBHandle *defaultDB;
  char* nick = NULL;

  CODE:
  nick = SvPV_nolen(string);

  defaultDB = CERT_GetDefaultCertDB();

  RETVAL = CERT_FindCertByNickname(defaultDB, nick);

  if ( RETVAL == NULL )
    XSRETURN_UNDEF;


  OUTPUT:
  RETVAL


void DESTROY(cert)
  Crypt::NSS::X509::Certificate cert;

  PPCODE:

  if ( cert ) {
    if ( cert->nssCertificate ) {
  //printf("Is nsscertificate\n");
  //printf("Refcount: %d\n", cert->nssCertificate->object.refCount);
    }
    //printf("Certificate %s destroyed\n", cert->subjectName);
    CERT_DestroyCertificate(cert);
    cert = 0;
  }

