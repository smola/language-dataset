#include "typetiny.h"
#include "xs_version.h"

#ifndef SvRXOK
#define SvRXOK(sv) (SvROK(sv) && SvMAGICAL(SvRV(sv)) && mg_find(SvRV(sv), PERL_MAGIC_qr))
#endif

#define MY_CXT_KEY "Type::Tiny::XS::_guts" XS_VERSION
typedef struct sui_cxt{
    GV* universal_isa;
    GV* universal_can;
    AV* tc_extra_args;
} my_cxt_t;
START_MY_CXT

typedef int (*check_fptr_t)(pTHX_ SV* const data, SV* const sv);

static
XSPROTO(XS_TypeTiny_constraint_check);

/*
    NOTE: typetiny_tc_check() handles GETMAGIC
*/
int
typetiny_tc_check(pTHX_ SV* const tc_code, SV* const sv) {
    CV* const cv = (CV*)SvRV(tc_code);
    assert(SvTYPE(cv) == SVt_PVCV);

    if(CvXSUB(cv) == XS_TypeTiny_constraint_check){ /* built-in type constraints */
        MAGIC* const mg = (MAGIC*)CvXSUBANY(cv).any_ptr;

        assert(CvXSUBANY(cv).any_ptr != NULL);
        assert(mg->mg_ptr            != NULL);

        SvGETMAGIC(sv);
        /* call the check function directly, skipping call_sv() */
        return CALL_FPTR((check_fptr_t)mg->mg_ptr)(aTHX_ mg->mg_obj, sv);
    }
    else { /* custom */
        int ok;
        dSP;
        dMY_CXT;

        ENTER;
        SAVETMPS;

        PUSHMARK(SP);
        XPUSHs(sv);
        if( MY_CXT.tc_extra_args ) {
            AV* const av  = MY_CXT.tc_extra_args;
            I32 const len = AvFILLp(av) + 1;
            int i;
            for(i = 0; i < len; i++) {
                XPUSHs( AvARRAY(av)[i] );
            }
        }
        PUTBACK;

        call_sv(tc_code, G_SCALAR);

        SPAGAIN;
        ok = sv_true(POPs);
        PUTBACK;

        FREETMPS;
        LEAVE;

        return ok;
    }
}

/*
    The following type check functions return an integer, not a bool, to keep
    the code simple,
    so if you assign these return value to a bool variable, you must use
    "expr ? TRUE : FALSE".
*/

int
typetiny_tc_Any(pTHX_ SV* const data PERL_UNUSED_DECL, SV* const sv PERL_UNUSED_DECL) {
    assert(sv);
    return TRUE;
}

int
typetiny_tc_Bool(pTHX_ SV* const data PERL_UNUSED_DECL, SV* const sv) {
    assert(sv);

    if (SvROK(sv)) {
        return FALSE;
    }

    if(sv_true(sv)){
        if(SvPOKp(sv)){ /* "1" */
            return SvCUR(sv) == 1 && SvPVX(sv)[0] == '1';
        }
        else if(SvIOKp(sv)){
            return SvIVX(sv) == 1;
        }
        else if(SvNOKp(sv)){
            return SvNVX(sv) == 1.0;
        }
        else{
            STRLEN len;
            char * ptr = SvPV(sv, len);
            if(len == 1 && ptr[0] == '1'){
                return TRUE;
            } else {
                return FALSE;
            }
        }
    }
    else{
        /* any false value is a boolean */
        return TRUE;
    }
}

int
typetiny_tc_Undef(pTHX_ SV* const data PERL_UNUSED_DECL, SV* const sv) {
    assert(sv);
    return !SvOK(sv);
}

int
typetiny_tc_Defined(pTHX_ SV* const data PERL_UNUSED_DECL, SV* const sv) {
    assert(sv);
    return SvOK(sv);
}

int
typetiny_tc_Value(pTHX_ SV* const data PERL_UNUSED_DECL, SV* const sv) {
    assert(sv);
    return SvOK(sv) && !SvROK(sv);
}

int
typetiny_tc_Num(pTHX_ SV* const data PERL_UNUSED_DECL, SV* const sv) {
    assert(sv);
    return LooksLikeNumber(sv);
}

static int
S_pv_is_integer(pTHX_ char* const pv) {
    const char* p;
    p = &pv[0];

    /* -?[0-9]+ */
    if(*p == '-') p++;

    if (!*p) return FALSE;

    while(*p){
        if(!isDIGIT(*p)){
            return FALSE;
        }
        p++;
    }
    return TRUE;
}

static int
S_nv_is_integer(pTHX_ NV const nv) {
    if(nv == (NV)(IV)nv){
        return TRUE;
    }
    else {
        char buf[64];  /* Must fit sprintf/Gconvert of longest NV */
        const char* p;
        (void)Gconvert(nv, NV_DIG, 0, buf);
        return S_pv_is_integer(aTHX_ buf);
    }
}

int
typetiny_tc_Int(pTHX_ SV* const data PERL_UNUSED_DECL, SV* const sv) {
    assert(sv);
    if (SvOK(sv) && !SvROK(sv) && !isGV(sv)) {
        if(SvPOK(sv)){
            return S_pv_is_integer(aTHX_ SvPVX(sv));
        }
        else if(SvIOK(sv)){
            return TRUE;
        }
        else if(SvNOK(sv)) {
            return S_nv_is_integer(aTHX_ SvNVX(sv));
        }
    }
    return FALSE;
}

int
typetiny_tc_PositiveInt(pTHX_ SV* const data PERL_UNUSED_DECL, SV* const sv) {
    char* i;
    STRLEN len;
    assert(sv);
    int j;
    if ((!SvOK(sv)) || SvROK(sv) || isGV(sv)) {
        return FALSE;
    }
    if(SvPOKp(sv)){
        if (!S_pv_is_integer(aTHX_ SvPVX(sv))) {
            return FALSE;
        }
    }
    else if(SvIOKp(sv)){
        /* ok */
    }
    else if(SvNOKp(sv)) {
        if (!S_nv_is_integer(aTHX_ SvNVX(sv))) {
            return FALSE;
        }
    }
    
    i = SvPVx(sv, len);
    if (len == 1 && i[0] == '0') {
        return FALSE;
    }
    else if (i[0] == '0') {
        for (j = 0; j < len; j++) {
            if (i[j] != '0') {
                return TRUE; // "01", "001", etc
            }
        }
        return FALSE; // "00", "000", etc
    }
    return ((len > 0 && i[0] != '-') ? TRUE : FALSE);
}

int
typetiny_tc_PositiveOrZeroInt(pTHX_ SV* const data PERL_UNUSED_DECL, SV* const sv) {
    char* i;
    STRLEN len;
    assert(sv);
    if ((!SvOK(sv)) || SvROK(sv) || isGV(sv)) {
        return FALSE;
    }
    if(SvPOKp(sv)){
        if (!S_pv_is_integer(aTHX_ SvPVX(sv))) {
            return FALSE;
        }
    }
    else if(SvIOKp(sv)){
        /* ok */
    }
    else if(SvNOKp(sv)) {
        if (!S_nv_is_integer(aTHX_ SvNVX(sv))) {
            return FALSE;
        }
    }
    
    i = SvPVx(sv, len);
    return ((len > 0 && i[0] != '-') ? TRUE : FALSE);
}

int
typetiny_tc_Str(pTHX_ SV* const data PERL_UNUSED_DECL, SV* const sv) {
    assert(sv);
    return SvOK(sv) && !SvROK(sv) && !isGV(sv);
}

int
typetiny_tc_Enum(pTHX_ SV* const data PERL_UNUSED_DECL, SV* const sv) {
    assert(sv);
    return SvOK(sv) && !SvROK(sv) && !isGV(sv);
}

int
typetiny_tc_NonEmptyStr(pTHX_ SV* const data PERL_UNUSED_DECL, SV* const sv) {
    assert(sv);
    if (SvOK(sv) && !SvROK(sv) && !isGV(sv)) {
        STRLEN l = sv_len(sv);
        return( (l==0) ? FALSE : TRUE );
    }
    return FALSE;
}

int
typetiny_tc_ClassName(pTHX_ SV* const data PERL_UNUSED_DECL, SV* const sv){
    assert(sv);
    return is_class_loaded(sv);
}

int
typetiny_tc_Ref(pTHX_ SV* const data PERL_UNUSED_DECL, SV* const sv) {
    assert(sv);
    return SvROK(sv);
}

int
typetiny_tc_ScalarRef(pTHX_ SV* const data PERL_UNUSED_DECL, SV* sv) {
    assert(sv);
    if(SvROK(sv)){
         sv = SvRV(sv);
         return !SvOBJECT(sv) && (SvTYPE(sv) <= SVt_PVLV && !isGV(sv));
    }
    return FALSE;
}

int
typetiny_tc_ArrayRef(pTHX_ SV* const data PERL_UNUSED_DECL, SV* const sv) {
    assert(sv);
    return IsArrayRef(sv);
}

int
typetiny_tc_HashRef(pTHX_ SV* const data PERL_UNUSED_DECL, SV* const sv) {
    assert(sv);
    return IsHashRef(sv);
}

int
typetiny_tc_Map(pTHX_ SV* const data PERL_UNUSED_DECL, SV* const sv) {
    assert(sv);
    return IsHashRef(sv);
}

int
typetiny_tc_Tuple(pTHX_ SV* const data PERL_UNUSED_DECL, SV* const sv) {
    assert(sv);
    return IsArrayRef(sv);
}

int
typetiny_tc_CodeRef(pTHX_ SV* const data PERL_UNUSED_DECL, SV* const sv) {
    assert(sv);
    return IsCodeRef(sv);
}

int
typetiny_tc_RegexpRef(pTHX_ SV* const data PERL_UNUSED_DECL, SV* const sv) {
    assert(sv);
    return SvRXOK(sv);
}

int
typetiny_tc_GlobRef(pTHX_ SV* const data PERL_UNUSED_DECL, SV* const sv) {
    assert(sv);
    return SvROK(sv) && !SvOBJECT(SvRV(sv)) && isGV(SvRV(sv));
}

int
typetiny_tc_FileHandle(pTHX_ SV* const data PERL_UNUSED_DECL, SV* const sv) {
    GV* gv;
    assert(sv);

    /* see pp_fileno() in pp_sys.c and Scalar::Util::openhandle() */

    gv = (GV*)(SvROK(sv) ? SvRV(sv) : sv);
    if(isGV(gv) || SvTYPE(gv) == SVt_PVIO){
        IO* const io = isGV(gv) ? GvIO(gv) : (IO*)gv;

        if(io && ( IoIFP(io) || SvTIED_mg((SV*)io, PERL_MAGIC_tiedscalar) )){
            return TRUE;
        }
    }

    return is_an_instance_of("IO::Handle", sv);
}

int
typetiny_tc_Object(pTHX_ SV* const data PERL_UNUSED_DECL, SV* const sv) {
    assert(sv);
    return SvROK(sv) && SvOBJECT(SvRV(sv));
}

/* Parameterized type constraints */

static int
typetiny_parameterized_ArrayRef(pTHX_ SV* const param, SV* const sv) {
    if(IsArrayRef(sv)){
        AV* const av  = (AV*)SvRV(sv);
        I32 const len = av_len(av) + 1;
        I32 i;
        for(i = 0; i < len; i++){
            SV* const value = *av_fetch(av, i, TRUE);
            if(!typetiny_tc_check(aTHX_ param, value)){
                return FALSE;
            }
        }
        return TRUE;
    }
    return FALSE;
}

static int
typetiny_parameterized_HashRef(pTHX_ SV* const param, SV* const sv) {
    if(IsHashRef(sv)){
        HV* const hv  = (HV*)SvRV(sv);
        HE* he;

        hv_iterinit(hv);
        while((he = hv_iternext(hv))){
            SV* const value = hv_iterval(hv, he);
            if(!typetiny_tc_check(aTHX_ param, value)){
                hv_iterinit(hv); /* reset */
                return FALSE;
            }
        }
        return TRUE;
    }
    return FALSE;
}

static int
typetiny_parameterized_Map(pTHX_ SV* const param, SV* const sv) {
    if(IsHashRef(sv)){
        HV* const hv  = (HV*)SvRV(sv);
        HE* he;

        AV* const params = (AV*)SvRV(param);
        SV* const param1 = *av_fetch(params, 0, TRUE);
        SV* const param2 = *av_fetch(params, 1, TRUE);

        hv_iterinit(hv);
        while((he = hv_iternext(hv))){
            SV* const key   = hv_iterkeysv(he);
            SV* const value = hv_iterval(hv, he);
            
            if(!typetiny_tc_check(aTHX_ param1, key)
            || !typetiny_tc_check(aTHX_ param2, value)){
                hv_iterinit(hv); /* reset */
                return FALSE;
            }
        }
        return TRUE;
    }
    return FALSE;
}

static int
typetiny_parameterized_Tuple(pTHX_ SV* const param, SV* const sv) {
    I32 i;
    if(IsArrayRef(sv)){
        AV* const av  = (AV*)SvRV(sv);
        I32 const len = av_len(av) + 1;

        AV* const params  = (AV*)SvRV(param);
        if (len - 1 != av_len(params)) {
            return FALSE;
        }

        for(i = 0; i < len; i++){
            SV* const check = *av_fetch(params, i, TRUE);
            SV* const value = *av_fetch(av, i, TRUE);
            if(!typetiny_tc_check(aTHX_ check, value)){
                return FALSE;
            }
        }
        return TRUE;
    }
    return FALSE;
}

static int
typetiny_parameterized_Enum(pTHX_ SV* const param, SV* const sv) {
    AV* av;
    I32 len;
    I32 i;
    
    assert(sv);
    if(!(SvOK(sv) && !SvROK(sv) && !isGV(sv))) {
        return FALSE;
    }

    av  = (AV*)SvRV(param);
    len = av_len(av) + 1;
    for(i = 0; i < len; i++){
        SV* const x = *av_fetch(av, i, TRUE);
        if(sv_eq(sv, x)){
            return TRUE;
        }
    }

    return FALSE;
}

static int
typetiny_parameterized_Maybe(pTHX_ SV* const param, SV* const sv) {
    if(SvOK(sv)){
        return typetiny_tc_check(aTHX_ param, sv);
    }
    return TRUE;
}

int
typetiny_tc_AnyOf(pTHX_ SV* const data PERL_UNUSED_DECL, SV* const sv PERL_UNUSED_DECL) {
    assert(sv);
    return FALSE;
}

int
typetiny_tc_AllOf(pTHX_ SV* const data PERL_UNUSED_DECL, SV* const sv PERL_UNUSED_DECL) {
    assert(sv);
    return TRUE;
}

static int
typetiny_parameterized_AnyOf(pTHX_ SV* const param, SV* const sv) {
    AV *types = (AV*)SvRV(param);
    I32 const len = AvFILLp(types) + 1;
    I32 i;

    for(i = 0; i < len; i++){
        if(typetiny_tc_check(aTHX_ AvARRAY(types)[i], sv)){
            return TRUE;
        }
    }

    return FALSE;
}

static int
typetiny_parameterized_AllOf(pTHX_ SV* const param, SV* const sv) {
    AV *types = (AV*)SvRV(param);
    I32 const len = AvFILLp(types) + 1;
    I32 i;

    ENTER;
    SAVE_DEFSV;
    DEFSV_set(sv);

    for(i = 0; i < len; i++){
        if(!typetiny_tc_check(aTHX_ AvARRAY(types)[i], sv)){
            LEAVE;
            return FALSE;
        }
    }

    LEAVE;

    return TRUE;
}

/*
 *  This class_type generator is taken from Scalar::Util::Instance
 */


#define MG_klass_stash(mg) ((HV*)(mg)->mg_obj)
#define MG_klass_pv(mg)    ((mg)->mg_ptr)
#define MG_klass_len(mg)   ((mg)->mg_len)

static const char*
typetiny_canonicalize_package_name(const char* name){

    /* "::Foo" -> "Foo" */
    if(name[0] == ':' && name[1] == ':'){
        name += 2;
    }

    /* "main::main::main::Foo" -> "Foo" */
    while(strnEQ(name, "main::", sizeof("main::")-1)){
        name += sizeof("main::")-1;
    }

    return name;
}

static int
typetiny_lookup_isa(pTHX_ HV* const instance_stash, const char* const klass_pv){
    AV*  const linearized_isa = mro_get_linear_isa(instance_stash);
    SV**       svp            = AvARRAY(linearized_isa);
    SV** const end            = svp + AvFILLp(linearized_isa) + 1;

    while(svp != end){
        assert(SvPVX(*svp));
        if(strEQ(klass_pv, typetiny_canonicalize_package_name(SvPVX(*svp)))){
            return TRUE;
        }
        svp++;
    }
    return FALSE;
}

#define find_method_pvn(a, b, c) typetiny_stash_find_method(aTHX_ a, b, c)
#define find_method_pvs(a, b)    typetiny_stash_find_method(aTHX_ a, STR_WITH_LEN(b))

STATIC_INLINE GV*
typetiny_stash_find_method(pTHX_ HV* const stash, const char* const name, I32 const namelen){
    GV** const gvp = (GV**)hv_fetch(stash, name, namelen, FALSE);
    if(gvp && isGV(*gvp) && GvCV(*gvp)){ /* shortcut */
        return *gvp;
    }

    return gv_fetchmeth(stash, name, namelen, 0);
}

int
typetiny_is_an_instance_of(pTHX_ HV* const stash, SV* const instance){
    assert(stash);
    assert(SvTYPE(stash) == SVt_PVHV);

    if(IsObject(instance)){
        dMY_CXT;
        HV* const instance_stash = SvSTASH(SvRV(instance));
        GV* const myisa          = find_method_pvs(instance_stash, "isa");

        /* the instance has no own isa method */
        if(myisa == NULL || GvCV(myisa) == GvCV(MY_CXT.universal_isa)){
            return stash == instance_stash
                || typetiny_lookup_isa(aTHX_ instance_stash, HvNAME_get(stash));
        }
        /* the instance has its own isa method */
        else {
            SV* package;
            int ok;

            ENTER;
            SAVETMPS;

            package = newSVpvn_share(HvNAME_get(stash), HvNAMELEN_get(stash), 0U);
            ok = sv_true(mcall1s(instance, "isa", sv_2mortal(package)));

            FREETMPS;
            LEAVE;

            return ok;
        }
    }
    return FALSE;
}

static int
typetiny_is_an_instance_of_universal(pTHX_ SV* const data, SV* const sv){
    PERL_UNUSED_ARG(data);
    return SvROK(sv) && SvOBJECT(SvRV(sv));
}

static int
typetiny_can_methods(pTHX_ AV* const methods, SV* const instance){
    if(IsObject(instance)){
        dMY_CXT;
        HV* const mystash      = SvSTASH(SvRV(instance));
        GV* const mycan        = find_method_pvs(mystash, "can");
        bool const use_builtin = (mycan == NULL || GvCV(mycan) == GvCV(MY_CXT.universal_can)) ? TRUE : FALSE;
        I32 const len           = AvFILLp(methods) + 1;
        I32 i;
        for(i = 0; i < len; i++){
            SV* const name = TYPETINY_av_at(methods, i);

            if(use_builtin){
                if(!find_method_pvn(mystash, SvPVX(name), SvCUR(name))){
                    return FALSE;
                }
            }
            else{
                bool ok;

                ENTER;
                SAVETMPS;

                ok = sv_true(mcall1s(instance, "can", sv_mortalcopy(name)));

                FREETMPS;
                LEAVE;

                if(!ok){
                    return FALSE;
                }
            }
        }
        return TRUE;
    }
    return FALSE;
}

static MGVTBL typetiny_util_type_constraints_vtbl; /* not used, only for identity */

static CV*
typetiny_tc_generate(pTHX_ const char* const name, check_fptr_t const fptr, SV* const param) {
    CV* xsub;

    xsub = newXS(name, XS_TypeTiny_constraint_check, __FILE__);
    CvXSUBANY(xsub).any_ptr = sv_magicext(
        (SV*)xsub,
        param,       /* mg_obj: refcnt will be increased */
        PERL_MAGIC_ext,
        &typetiny_util_type_constraints_vtbl,
        (char*)fptr, /* mg_ptr */
        0            /* mg_len: 0 for static data */
    );

    if(!name){
        sv_2mortal((SV*)xsub);
    }

    return xsub;
}

CV*
typetiny_generate_isa_predicate_for(pTHX_ SV* const klass, const char* const predicate_name){
    STRLEN klass_len;
    const char* klass_pv = SvPV_const(klass, klass_len);
    SV*   param;
    check_fptr_t fptr;

    klass_pv = typetiny_canonicalize_package_name(klass_pv);

    if(strNE(klass_pv, "UNIVERSAL")){
        param = (SV*)gv_stashpvn(klass_pv, klass_len, GV_ADD);
        fptr = (check_fptr_t)typetiny_is_an_instance_of;

    }
    else{
        param = NULL;
        fptr = (check_fptr_t)typetiny_is_an_instance_of_universal;
    }

    return typetiny_tc_generate(aTHX_ predicate_name, fptr, param);
}

CV*
typetiny_generate_can_predicate_for(pTHX_ SV* const methods, const char* const predicate_name){
    AV* av;
    AV* const param = newAV_mortal();
    I32 len;
    I32 i;

    must_ref(methods, "an ARRAY ref for method names", SVt_PVAV);
    av = (AV*)SvRV(methods);

    len = av_len(av) + 1;
    for(i = 0; i < len; i++){
        SV* const name = *av_fetch(av, i, TRUE);
        STRLEN pvlen;
        const char* const pv = SvPV_const(name, pvlen);

        av_push(param, newSVpvn_share(pv, pvlen, 0U));
    }

    return typetiny_tc_generate(aTHX_ predicate_name, (check_fptr_t)typetiny_can_methods, (SV*)param);
}

static
XSPROTO(XS_TypeTiny_constraint_check) {
    dVAR;
    dXSARGS;
    MAGIC* const mg = (MAGIC*)XSANY.any_ptr;
    SV* sv;

    if(items < 1){
        sv = &PL_sv_undef;
    }
    else {
        sv = ST(0);
        SvGETMAGIC(sv);
    }

    ST(0) = boolSV( CALL_FPTR((check_fptr_t)mg->mg_ptr)(aTHX_ mg->mg_obj, sv) );
    XSRETURN(1);
}

static
XSPROTO(XS_TypeTiny_TypeConstraint_fallback) {
    dXSARGS;
    PERL_UNUSED_VAR(cv);
    PERL_UNUSED_VAR(items);
    XSRETURN_EMPTY;
}

static void
setup_my_cxt(pTHX_ pMY_CXT){
    MY_CXT.universal_isa = gv_fetchpvs("UNIVERSAL::isa", GV_ADD, SVt_PVCV);
    SvREFCNT_inc_simple_void_NN(MY_CXT.universal_isa);

    MY_CXT.universal_can = gv_fetchpvs("UNIVERSAL::can", GV_ADD, SVt_PVCV);
    SvREFCNT_inc_simple_void_NN(MY_CXT.universal_can);

    MY_CXT.tc_extra_args = NULL;
}

#define DEFINE_TC(name) typetiny_tc_generate(aTHX_ "Type::Tiny::XS::" STRINGIFY(name), CAT2(typetiny_tc_, name), NULL)

#define MTC_CLASS "Type::Tiny::XS::TC"

MODULE = Type::Tiny::XS    PACKAGE = Type::Tiny::XS

PROTOTYPES:   DISABLE
VERSIONCHECK: DISABLE

BOOT:
{
    MY_CXT_INIT;
    boot_Type__Tiny__XS__Util(aTHX_ cv);
    setup_my_cxt(aTHX_ aMY_CXT);
    
    /* setup built-in type constraints */
    DEFINE_TC(Any);
    DEFINE_TC(Undef);
    DEFINE_TC(Defined);
    DEFINE_TC(Bool);
    DEFINE_TC(Value);
    DEFINE_TC(Ref);
    DEFINE_TC(Str);
    DEFINE_TC(NonEmptyStr);
    DEFINE_TC(Num);
    DEFINE_TC(Int);
    DEFINE_TC(PositiveInt);
    DEFINE_TC(PositiveOrZeroInt);
    DEFINE_TC(ScalarRef);
    DEFINE_TC(ArrayRef);
    DEFINE_TC(HashRef);
    DEFINE_TC(Map);
    DEFINE_TC(Enum);
    DEFINE_TC(Tuple);
    DEFINE_TC(CodeRef);
    DEFINE_TC(GlobRef);
    DEFINE_TC(FileHandle);
    DEFINE_TC(RegexpRef);
    DEFINE_TC(Object);
    DEFINE_TC(ClassName);
    DEFINE_TC(AnyOf);
    DEFINE_TC(AllOf);
}

#ifdef USE_ITHREADS

void
CLONE(...)
CODE:
{
    MY_CXT_CLONE;
    setup_my_cxt(aTHX_ aMY_CXT);
    PERL_UNUSED_VAR(items);
}

#endif /* !USE_ITHREADS */

#define TYPETINY_TC_MAYBE     0
#define TYPETINY_TC_ARRAY_REF 1
#define TYPETINY_TC_HASH_REF  2
#define TYPETINY_TC_MAP       3
#define TYPETINY_TC_TUPLE     4
#define TYPETINY_TC_ENUM      5
#define TYPETINY_TC_ANYOF     6
#define TYPETINY_TC_ALLOF     7

CV*
_parameterize_ArrayRef_for(SV* param)
ALIAS:
    _parameterize_ArrayRef_for = TYPETINY_TC_ARRAY_REF
    _parameterize_HashRef_for  = TYPETINY_TC_HASH_REF
    _parameterize_Maybe_for    = TYPETINY_TC_MAYBE
    _parameterize_Map_for      = TYPETINY_TC_MAP
    _parameterize_Tuple_for    = TYPETINY_TC_TUPLE
    _parameterize_Enum_for     = TYPETINY_TC_ENUM
    _parameterize_AnyOf_for    = TYPETINY_TC_ANYOF
    _parameterize_AllOf_for    = TYPETINY_TC_ALLOF
CODE:
{
    check_fptr_t fptr;
    SV* const tc_code = param;
    if(ix == TYPETINY_TC_MAP
    || ix == TYPETINY_TC_TUPLE
    || ix == TYPETINY_TC_ENUM
    || ix == TYPETINY_TC_ANYOF
    || ix == TYPETINY_TC_ALLOF) {
        if(!IsArrayRef(tc_code)){
            croak("Didn't supply an ARRAY reference");
        }
    }
    else {
        if(!IsCodeRef(tc_code)){
            croak("Didn't supply a CODE reference");
        }
    }

    switch(ix){
        case TYPETINY_TC_ARRAY_REF:
            fptr = typetiny_parameterized_ArrayRef;
            break;
        case TYPETINY_TC_HASH_REF:
            fptr = typetiny_parameterized_HashRef;
            break;
        case TYPETINY_TC_MAP:
            fptr = typetiny_parameterized_Map;
            break;
        case TYPETINY_TC_TUPLE:
            fptr = typetiny_parameterized_Tuple;
            break;
        case TYPETINY_TC_ENUM:
            fptr = typetiny_parameterized_Enum;
            break;
        case TYPETINY_TC_ANYOF:
            fptr = typetiny_parameterized_AnyOf;
            break;
        case TYPETINY_TC_ALLOF:
            fptr = typetiny_parameterized_AllOf;
            break;
        default: /* Maybe type */
            fptr = typetiny_parameterized_Maybe;
    }
    RETVAL = typetiny_tc_generate(aTHX_ NULL, fptr, tc_code);
}
OUTPUT:
    RETVAL
