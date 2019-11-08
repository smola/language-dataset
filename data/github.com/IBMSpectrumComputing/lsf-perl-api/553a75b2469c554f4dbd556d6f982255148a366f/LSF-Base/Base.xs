#ifdef __cplusplus
extern "C" {
#endif
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#include "ppport.h"
#include "Av_CharPtrPtr.h"   /*XS_*_charPtrPtr() */
#include "Av_IntPtr.h"  /* XS_*_intPtr() */
#ifdef sun
/*This is to fix a problem in lsf.h regarding LS_WAIT_T*/
#define SVR4 1 
#include <netdb.h>
#endif
#include <lsf/lsf.h>
#ifdef __cplusplus
}
#endif
#ifndef PL_errgv
#define PL_errgv errgv
#endif

/*general cluster information*/
typedef struct lsInfo LSF_Base_lsInfo;
typedef struct resItem LSF_Base_resItem;
typedef struct hostInfo LSF_Base_hostInfo;
typedef struct lsfAcctRec LSF_Base_lsfAcctRec;

/*load information and placement*/
typedef struct hostLoad LSF_Base_hostLoad;

/*remote execution and task control*/
typedef struct rusage LSF_Base_rusage;

/*set $@ to lsf error message*/
#define SET_LSF_ERRMSG sv_setpv(GvSV(PL_errgv),ls_sysmsg())

#ifndef STATUS_NATIVE_SET
#define STATUS_NATIVE_SET STATUS_NATIVE_CHILD_SET
#endif

static int li_ni = 0;

MODULE = LSF::Base		PACKAGE = LSF::Base		PREFIX = ls_

PROTOTYPES:DISABLE

LSF_Base_lsInfo *
ls_info(self)
	void *self
    CODE:
	RETVAL = ls_info();
	if(RETVAL == NULL){
           STATUS_NATIVE_SET(lserrno);
	   SET_LSF_ERRMSG;
	   XSRETURN_UNDEF;
	}
    OUTPUT:
	RETVAL

char*
ls_getmyhostname(self)
	void *self
    CODE:
	if( (RETVAL = ls_getmyhostname()) == NULL ){
           STATUS_NATIVE_SET(lserrno);
	   SET_LSF_ERRMSG;
	   XSRETURN_UNDEF;
	}	
    OUTPUT:
	RETVAL

char* 
ls_getclustername(self)
	void *self
    CODE:
	RETVAL = ls_getclustername();
	if(RETVAL == NULL){
            STATUS_NATIVE_SET(lserrno);
	    SET_LSF_ERRMSG;
	    XSRETURN_UNDEF;
	}
    OUTPUT:
	RETVAL

char*
ls_getmastername(self)
	void *self
    CODE:
	RETVAL = ls_getmastername();
	if(RETVAL == NULL){
	    STATUS_NATIVE_SET(lserrno);
	    SET_LSF_ERRMSG;
	    XSRETURN_UNDEF;
	}
    OUTPUT:
	RETVAL

float*
ls_getmodelfactor(self,modelname)
	void	*self
	char*	modelname
    CODE:
	RETVAL = ls_getmodelfactor(modelname);
	if(RETVAL == NULL){
	    STATUS_NATIVE_SET(lserrno);
	    SET_LSF_ERRMSG;
	    XSRETURN_UNDEF;
        }
    OUTPUT:
	RETVAL

char*
ls_gethosttype(self,hostname)
	void	*self
	char*	hostname
    CODE:
	RETVAL = ls_gethosttype(hostname);
	if(RETVAL == NULL){
	    STATUS_NATIVE_SET(lserrno);
	    SET_LSF_ERRMSG;	
	    XSRETURN_UNDEF;
	}
    OUTPUT:
	RETVAL

char*
ls_gethostmodel(self, hostname)
	void	*self
	char*	hostname
    CODE:
	RETVAL = ls_gethostmodel(hostname);
	if(RETVAL == NULL){
            STATUS_NATIVE_SET(lserrno);
	    SET_LSF_ERRMSG;
	    XSRETURN_UNDEF;	    	
	}
    OUTPUT:
	RETVAL

float*
ls_gethostfactor(self,hostname)
	void	*self
	char*	hostname
    CODE:
	RETVAL = ls_gethostfactor(hostname);
	if(RETVAL == NULL){
	    STATUS_NATIVE_SET(lserrno);
	    SET_LSF_ERRMSG;
	    XSRETURN_UNDEF;
	}
    OUTPUT:
	RETVAL


void
ls_gethostinfo( self, resreq, hostlist, options )
	void	*self
	char*	resreq
	char**	hostlist
	int	options
    PREINIT:
	SV *rv;	
	int i, count=0, num;
	LSF_Base_hostInfo *hi, *p;
	char **c, **hl;
    PPCODE:
	for( c = hostlist; hostlist && *c; c++ ) count++;
	if( count == 0 ) hostlist = NULL;
	hi = ls_gethostinfo(resreq, &num, hostlist, count, options );
	if(hi == NULL){
	    STATUS_NATIVE_SET(lserrno);
	    SET_LSF_ERRMSG;
	    XSRETURN_EMPTY;
 	}
	for( i = 0, p = hi; i < num; i++,p++ ){
	    rv = newRV_inc(&PL_sv_undef);
	    sv_setref_iv(rv, "LSF::Base::hostInfoPtr",(IV)p);
	    XPUSHs(sv_2mortal(rv));
	}
	XSRETURN(num);
	
void
ls_readconfenv(self, env, path)
	void	*self
	char** 	env
	char* 	path
    PREINIT:
	struct config_param *param;
	int i, count=0;
	char **p;
    PPCODE:
	for( p = env; env && *p; p++ ) count++;
	param = safemalloc(sizeof(struct config_param)*(count + 1));
	for( i=0; i<count; i++){
	    param[i].paramName = env[i];
	    param[i].paramValue = NULL;
	}
	param[count].paramName = NULL;
	param[count].paramValue = NULL;
	if( ls_readconfenv(param, path) < 0 ){
	    STATUS_NATIVE_SET(lserrno);
            SET_LSF_ERRMSG;
	    safefree(param);
	    XSRETURN_EMPTY;
	}
	for( i=0; i<count; i++){
	    XPUSHs(sv_2mortal(newSVpv(param[i].paramName, 0)));	
	    if(param[i].paramValue == NULL)
	    {
	        XPUSHs(sv_2mortal(newSVpv(param[i].paramValue, 1)));
	    }
	    else
	    {
	        XPUSHs(sv_2mortal(newSVpv(param[i].paramValue, 0)));
	    }
	}
	safefree(param);
	XSRETURN(count*2);

MODULE = LSF::Base		PACKAGE = LSF::Base::lsInfoPtr PREFIX = li_

int
li_nRes(self)
	LSF_Base_lsInfo *self;
    CODE:
	RETVAL = self->nRes;
    OUTPUT:
	RETVAL

void
li_resTable(self)
	LSF_Base_lsInfo *self;
    PREINIT:
	SV *rv;	
	int i;
	LSF_Base_resItem *p;
    PPCODE:
	for( i = 0,p =self->resTable; i < self->nRes; i++,p++ ){
	    rv = newRV_inc(&PL_sv_undef);
	    sv_setref_iv(rv, "LSF::Base::resItemPtr",(IV)p);
	    XPUSHs(sv_2mortal(rv));
	}
	XSRETURN(self->nRes);

void
li_hostTypes(self)
	LSF_Base_lsInfo *self;
    PREINIT:
	int i;
    PPCODE:
	for( i = 0; i < self->nTypes; i++){
	    XPUSHs(sv_2mortal(newSVpv(self->hostTypes[i],0)));
	}
	XSRETURN(self->nTypes);

void
li_hostModels(self)
	LSF_Base_lsInfo *self;
    PREINIT:
	int i;
    PPCODE:
	for( i = 0; i < self->nModels; i++ ){
	    XPUSHs(sv_2mortal(newSVpv(self->hostModels[i],0)));
	}
	XSRETURN(self->nModels);

void
li_hostArchs(self)
	LSF_Base_lsInfo *self;
    PREINIT:
	int i;
    PPCODE:
	for( i = 0; i < self->nTypes; i++){
	    XPUSHs(sv_2mortal(newSVpv(self->hostArchs[i],0)));
	}
	XSRETURN(self->nTypes);

void
li_modelRefs(self)
	LSF_Base_lsInfo *self;
    PREINIT:
	int i;
	int *p;
    PPCODE:
	for( i = 0,p = self->modelRefs; i < self->nModels; i++,p++ ){
	    XPUSHs(sv_2mortal(newSViv((int)*p)));
	}
	XSRETURN(self->nModels);

void
li_cpuFactor(self)
	LSF_Base_lsInfo *self;
    PREINIT:
	int i;
	float *p;
    PPCODE:
	for( i = 0,p = self->cpuFactor; i < self->nModels; i++,p++ ){
	    XPUSHs(sv_2mortal(newSVnv((double)*p)));
	}
	XSRETURN(self->nModels);

int 
li_numIndx(self)
	LSF_Base_lsInfo *self;
    CODE:
	RETVAL = self->numIndx;
    OUTPUT:
	RETVAL

int 
li_numUsrIndx(self)
	LSF_Base_lsInfo *self;
    CODE:
	RETVAL = self->numUsrIndx;
    OUTPUT:
	RETVAL


MODULE = LSF::Base		PACKAGE = LSF::Base::resItemPtr PREFIX = ri_

char*
ri_name(self)
	LSF_Base_resItem *self;
    CODE:
	RETVAL = self->name;
    OUTPUT:
	RETVAL

char*
ri_des(self)
	LSF_Base_resItem *self;
    CODE:
	RETVAL = self->des;
    OUTPUT:
	RETVAL

enum valueType
ri_valueType(self)
	LSF_Base_resItem *self;
    CODE:
	RETVAL = self->valueType;
    OUTPUT:
	RETVAL

enum orderType
ri_orderType(self)
	LSF_Base_resItem *self;
    CODE:
	RETVAL = self->orderType;
    OUTPUT:
	RETVAL

int
ri_flags(self)
	LSF_Base_resItem *self;
    CODE:
	RETVAL = self->flags;
    OUTPUT:
	RETVAL

int
ri_interval(self)
	LSF_Base_resItem *self;
    CODE:
	RETVAL = self->interval;
    OUTPUT:
	RETVAL

MODULE = LSF::Base		PACKAGE = LSF::Base::hostInfoPtr PREFIX = hi_

char*
hi_hostName(self)
	LSF_Base_hostInfo *self;
    CODE:
	RETVAL = self->hostName;
    OUTPUT:
	RETVAL

char*
hi_hostType(self)
	LSF_Base_hostInfo *self;
    CODE:
	RETVAL = self->hostType;
    OUTPUT:
	RETVAL

char*
hi_hostModel(self)
	LSF_Base_hostInfo *self;
    CODE:
	RETVAL = self->hostModel;
    OUTPUT:
	RETVAL

double
hi_cpuFactor(self)
	LSF_Base_hostInfo *self;
    CODE:
	RETVAL = (double)self->cpuFactor;
    OUTPUT:
	RETVAL

int
hi_maxCpus(self)
	LSF_Base_hostInfo *self;
    CODE:
	RETVAL = self->maxCpus;
    OUTPUT:
	RETVAL

int
hi_maxMem(self)
	LSF_Base_hostInfo *self;
    CODE:
	RETVAL = self->maxMem;
    OUTPUT:
	RETVAL

int
hi_maxSwap(self)
	LSF_Base_hostInfo *self;
    CODE:
	RETVAL = self->maxSwap;
    OUTPUT:
	RETVAL

int
hi_maxTmp(self)
	LSF_Base_hostInfo *self;
    CODE:
	RETVAL = self->maxTmp;
    OUTPUT:
	RETVAL

int
hi_nDisks(self)
	LSF_Base_hostInfo *self;
    CODE:
	RETVAL = self->nDisks;
    OUTPUT:
	RETVAL

void
hi_resources(self)
	LSF_Base_hostInfo *self;
    PREINIT:
	int i;
	char *p;
    PPCODE:
	for( i = 0; i < self->nRes; i++ ){
	    XPUSHs(sv_2mortal(newSVpv(self->resources[i],0)));
	}
	XSRETURN(self->nRes);

void
hi_DResources(self)
        LSF_Base_hostInfo *self;
    PREINIT:
        int i;
    PPCODE:
        for( i = 0; i < self->nDRes; i++ ){
            XPUSHs(sv_2mortal(newSVpv(self->DResources[i],0)));
        }
        XSRETURN(self->nDRes);

char*
hi_windows(self)
	LSF_Base_hostInfo *self;
    CODE:
	RETVAL = self->windows;
    OUTPUT:
	RETVAL

void
hi_busyThreshold(self)
	LSF_Base_hostInfo *self;
    PREINIT:
	int i;
    PPCODE:
	for( i=0; i < self->numIndx; i++ ){
	   XPUSHs(sv_2mortal(newSVnv(self->busyThreshold[i])));
        }
	XSRETURN(self->numIndx);

char
hi_isServer(self)
	LSF_Base_hostInfo *self;
    CODE:
	RETVAL = self->isServer;
    OUTPUT:
	RETVAL

char
hi_licensed(self)
	LSF_Base_hostInfo *self;
    CODE:
	RETVAL = self->licensed;
    OUTPUT:
	RETVAL

int
hi_rexPriority(self)
	LSF_Base_hostInfo *self;
    CODE:
	RETVAL = self->rexPriority;
    OUTPUT:
	RETVAL

int
hi_licFeaturesNeeded(self)
	LSF_Base_hostInfo *self;
    CODE:
	RETVAL = self->licFeaturesNeeded;
    OUTPUT:
	RETVAL

 #load information and placement

MODULE = LSF::Base		PACKAGE = LSF::Base		PREFIX = ls_

void
ls_load( self, resreq, numhosts, options, fromhost )
	void *self
	char*	resreq
	int	numhosts
	int	options
	char*	fromhost
	
    PREINIT:
	struct hostLoad *hl, *p;
	int num, i;
	SV *rv;
    PPCODE:
	num = numhosts;
	if(strlen(fromhost)==0) fromhost = NULL;
	hl = ls_load( resreq, &num, options, fromhost);
	if(hl == NULL){
	    STATUS_NATIVE_SET(lserrno);
	    SET_LSF_ERRMSG;
	    XSRETURN_EMPTY;
 	}
	/*num contains number of records*/
	for( i = 0, p = hl; i < num; i++,p++ ){
	    rv = newRV_inc(&PL_sv_undef);
	    sv_setref_iv(rv, "LSF::Base::hostLoadPtr", (IV)p);
	    XPUSHs(sv_2mortal(rv));
	}
	XSRETURN(num);
	    
 #I'll do this one later.	
 #void
 #ls_loadinfo( resreq, numhosts, options, fromhost, hostlist, indxnamelist )
 #	char*	resreq
 #	int	numhosts
 #	int	options
 #	char*	fromhost
 #	char**	hostlist
 #	char**	indxnamelist
 #	PPCODE:

void
ls_loadofhosts( self, resreq, numhosts, options, fromhost, hostlist )
	void	*self
	char*	resreq
	int	numhosts
	int	options
	char*	fromhost
	char 	**hostlist;
    PREINIT:
	LSF_Base_hostLoad *hl, *p;
	int num, i, count;
	SV *rv;
	char **c;
    PPCODE:
	num = numhosts;
        count = 0;
      	for( c = hostlist; hostlist && *c; c++ ) count++;
	if( count == 0 ) hostlist = NULL;
	if(strlen(fromhost)==0) fromhost = NULL;
	hl = ls_loadofhosts( resreq, &num, options, fromhost, hostlist, count);
	if(hl == NULL){
	    STATUS_NATIVE_SET(lserrno);
	    SET_LSF_ERRMSG;
	    XSRETURN_EMPTY;
 	}
	for( i = 0, p = hl; i < num; i++,p++ ){
	    rv = newRV_inc(&PL_sv_undef);
	    sv_setref_iv(rv, "LSF::Base::hostLoadPtr", (IV)p);
	    XPUSHs(sv_2mortal(rv));
	}
	XSRETURN(num);

void
ls_placereq(self, resreq, numhosts, options, fromhost)
	void	*self
	char*	resreq
	int	numhosts
	int	options
	char* 	fromhost
	
	PREINIT:
	int num;
	char *s, **list;
	int i;

	PPCODE:
	num = numhosts;
	if(strlen(fromhost)==0) fromhost = NULL;
	list = ls_placereq( resreq, &num, options, fromhost );
	if( list == NULL ){
	   STATUS_NATIVE_SET(lserrno);
	   SET_LSF_ERRMSG;
	   XSRETURN_EMPTY;
	}
	else{
	   for( i = 0; i < num; i++ ){
	      XPUSHs(sv_2mortal(newSVpv(list[i],0)));
           }
        }
        XSRETURN(num);

void
ls_placeofhosts( self, resreq, numhosts, options, fromhost, hostlist )
	int	self
	char*	resreq
	int	numhosts
	int	options
	char*	fromhost
	char 	**hostlist
    PREINIT:
	char **list,**c;
	int num, i,count=0;
    PPCODE:
	num = numhosts;
       	for( c = hostlist; hostlist && *c; c++ ) count++;
	if(strlen(fromhost)==0) fromhost = NULL;
	list = ls_placeofhosts( resreq, &num, options, fromhost, 
                             hostlist, count);
	if(list == NULL){
	    STATUS_NATIVE_SET(lserrno);
	    SET_LSF_ERRMSG;
	    XSRETURN_EMPTY;
 	}
	for( i = 0;i < num; i++ ){
	      XPUSHs(sv_2mortal(newSVpv(list[i],0)));
	}
	XSRETURN(num);

int
ls_loadadj(self, resreq, placeinfo)
	void	*self
	char*	resreq
	HV*	placeinfo; 
    PREINIT:
	HE*	entry;
	int	sz, size, i=0, value;
	I32	len;
	char	*key;
	struct placeInfo *pi;
    CODE:
	if (strlen(resreq) == 0) resreq = NULL;
	sz = size = hv_iterinit(placeinfo);
	pi = safemalloc(sizeof(struct placeInfo)*size);
	while(sz--){
	   entry = hv_iternext(placeinfo);
	   key = hv_iterkey(entry, &len);
	   value = SvIV(hv_iterval(placeinfo,entry));
	   strncpy(pi[i].hostName,key,len);
	   pi[i++].numtask = value;
	}
	if( ls_loadadj(resreq, pi, size) == 0 ){
	   RETVAL = 1;
	}	
	else{
	   STATUS_NATIVE_SET(lserrno);
	   SET_LSF_ERRMSG;
	   RETVAL = 0;
	}
	safefree(pi);
    OUTPUT:
	RETVAL

MODULE = LSF::Base		PACKAGE = LSF::Base::hostLoadPtr PREFIX = hl_

char*
hl_hostName(self)
	LSF_Base_hostLoad *self
    CODE:
	RETVAL = self->hostName;
    OUTPUT:
	RETVAL

void
hl_status(self)
	LSF_Base_hostLoad *self;
    PPCODE:
	XPUSHs(sv_2mortal(newSViv(self->status[0])));
	XPUSHs(sv_2mortal(newSViv(self->status[1])));
	XSRETURN(2);

void
hl_li(self)
	LSF_Base_hostLoad *self;
    PREINIT:
	int i, ni;
	float *p;
	LSF_Base_lsInfo *li;
    PPCODE:
	/* At this point, I don't know the number of indices to return */
	if(li_ni == 0){	
	   li = ls_info();	
	   li_ni = li->numIndx;
	}
	p = self->li;
	for(i=0; i<li_ni ;i++,p++ ){
	   XPUSHs(sv_2mortal(newSVnv((double)*p)));
	}
	XSRETURN(li_ni);

 #Task list manipulation functions

MODULE = LSF::Base		PACKAGE = LSF::Base		PREFIX = ls_

char*
ls_resreq(self, task)
	void	*self;
	char*	task
    CODE:
	RETVAL = ls_resreq(task);
	if(RETVAL == NULL){
	    STATUS_NATIVE_SET(lserrno);
	    SET_LSF_ERRMSG;
	    XSRETURN_UNDEF;
 	}
    OUTPUT:
	RETVAL

void
ls_eligible( self, task, mode )
        void    *self
        char*   task
        int     mode
    PREINIT:
        char resreqstr[1024];
        char * resreq;
        int ret, i;
    PPCODE:
        resreq = (char *) calloc(sizeof(resreqstr), sizeof(char));
        ret = ls_eligible(task, &resreq, mode);
        if( ret < 0 ){
            STATUS_NATIVE_SET(lserrno);
            SET_LSF_ERRMSG;
            if (resreq) free(resreq);
            XSRETURN_EMPTY;
        }
        for(i=0; i<sizeof(resreqstr); i++) resreqstr[i] = resreq[i];
        free(resreq);

        XPUSHs(sv_2mortal(newSViv(ret)));
        XPUSHs(sv_2mortal(newSVpv(resreqstr,0)));
        XSRETURN(2);

int
ls_insertrtask(self, task)
	void	*self
	char*	task
    CODE:
	if( ls_insertrtask(task) < 0 ){	
	    STATUS_NATIVE_SET(lserrno);
	    SET_LSF_ERRMSG;
	    XSRETURN_EMPTY;
 	}
	RETVAL = 1;	    
    OUTPUT:
	RETVAL

int
ls_insertltask(self, task)
	void	*self
	char*	task
    CODE:
	if( ls_insertltask(task) < 0 ){	
	    STATUS_NATIVE_SET(lserrno);
	    SET_LSF_ERRMSG;
	    XSRETURN_EMPTY;
 	}
	RETVAL = 1;	    
    OUTPUT:
	RETVAL

int
ls_deletertask(self, task)
	void	*self
	char*	task
    CODE:
	if( ls_deletertask(task) < 0 ){	
	    STATUS_NATIVE_SET(lserrno);
	    SET_LSF_ERRMSG;
	    XSRETURN_EMPTY;
 	}
	RETVAL = 1;	    
    OUTPUT:
	RETVAL

int
ls_deleteltask(self, task)
	void	*self
	char*	task
    CODE:
	if( ls_deleteltask(task) < 0 ){	
	    STATUS_NATIVE_SET(lserrno);
	    SET_LSF_ERRMSG;
	    XSRETURN_EMPTY;
 	}
	RETVAL = 1;	    
    OUTPUT:
	RETVAL

void
ls_listrtask(self, sortflag)
	void	*self
	int	sortflag
    PREINIT:
	char **list;
	int num,i;
    PPCODE:
	num = ls_listrtask(&list, sortflag);
	if( num == 0 ){
	    STATUS_NATIVE_SET(lserrno);
	    SET_LSF_ERRMSG;
	    XSRETURN_EMPTY;
	}
	else{
	    for( i=0; i < num; i++ ){
	      XPUSHs(sv_2mortal(newSVpv(list[i],0)));
            }
            XSRETURN(num);
        }

void
ls_listltask(self, sortflag)
	void	*self
	int	sortflag
    PREINIT:
	char **list;
	int num,i;
    PPCODE:
	num = ls_listltask(&list, sortflag);
	if( num == 0 ){
	    STATUS_NATIVE_SET(lserrno);
            SET_LSF_ERRMSG;
	    XSRETURN_EMPTY;
	}
	else{
	    for( i=0; i < num; i++ ){
	      XPUSHs(sv_2mortal(newSVpv(list[i],0)));
            }
            XSRETURN(num);
        }


 #Remote Execution and Task control functions


int 
ls_initrex(self, numPorts, options)
	void	*self
	int	numPorts
	int 	options
    CODE:
	RETVAL = ls_initrex(numPorts,options);
	if( RETVAL < 0 ){
	   STATUS_NATIVE_SET(lserrno);
	   SET_LSF_ERRMSG;
	   XSRETURN_UNDEF;
        }
    OUTPUT:
	RETVAL

int 
ls_connect(self,hostname)
	void	*self
	char*	hostname
    CODE:
	if(ls_connect(hostname) < 0){
	   RETVAL = 0;
	   STATUS_NATIVE_SET(lserrno);
           SET_LSF_ERRMSG;
        }
	else
	   RETVAL = 1;
    OUTPUT:
	RETVAL

int 
ls_isconnected(self,hostname)
	void	*self
	char*	hostname
    CODE:
	RETVAL = ls_isconnected(hostname);
    OUTPUT:
	RETVAL

void
ls_findmyconnections(self)
	void	*self
    PREINIT:
	char **p,**hosts;
	int count=0;
    PPCODE:
	hosts = ls_findmyconnections();
	if(hosts){
	   for( p = hosts; *p; p++){
     	      XPUSHs(sv_2mortal(newSVpv(*p,0)));
	      count++;
	   }
	   XSRETURN(count);
	}
	else{
	   STATUS_NATIVE_SET(lserrno);
	   SET_LSF_ERRMSG;
	   XSRETURN_EMPTY;
	}

int 
ls_rexecv(self, host, argv, options)
	void	*self
	char	*host
	char 	**argv
	int	options
    CODE:
	if( ls_rexecv(host, argv, options) < 0){
	   STATUS_NATIVE_SET(lserrno);
	   SET_LSF_ERRMSG;
	   RETVAL = 0;
	}
	/*should never get to this point.*/
    OUTPUT:
	RETVAL

int 
ls_rexecve(self, host, argv, options, envp)
	void	*self
	char 	*host
	char 	**argv
	int 	options
	char 	**envp
    CODE:
	if( ls_rexecve(host, argv, options, envp) < 0){
	   STATUS_NATIVE_SET(lserrno);
	   SET_LSF_ERRMSG;
	   RETVAL = 0;
	}
	/*should never get to this point.*/
    OUTPUT:
	RETVAL

int 
ls_rtask(self, host, argv, options)
	void	*self
	char 	*host
	char 	**argv
	int 	options
    CODE:
	RETVAL = ls_rtask(host, argv, options);
	if( RETVAL < 0 ){
	   STATUS_NATIVE_SET(lserrno);
           SET_LSF_ERRMSG;
	   XSRETURN_UNDEF;
 	}
    OUTPUT:
	RETVAL

int 
ls_rtaske(self, host, argv, options, envp)
	void	*self
	char 	*host
	char 	**argv
	int 	options
	char 	**envp
    CODE:
	RETVAL = ls_rtaske(host, argv, options, envp);
	if( RETVAL < 0 ){
	   STATUS_NATIVE_SET(lserrno);
           SET_LSF_ERRMSG;
	   XSRETURN_UNDEF;
 	}
    OUTPUT:
	RETVAL

void
ls_rwait(self, options)
	void	*self
	int	options
    PREINIT:
	LSF_Base_rusage *ru;
	LS_WAIT_T status;
	int tid;
	SV *rv;
    PPCODE:
	ru = safemalloc(sizeof(LSF_Base_rusage));
	tid = ls_rwait(&status, options, ru);
	if( tid < 0 ){
	   safefree(ru);
	   STATUS_NATIVE_SET(lserrno);
	   SET_LSF_ERRMSG;
           XSRETURN_EMPTY;
        }
	STATUS_NATIVE_SET(status);
	XPUSHs(sv_2mortal(newSViv(tid)));
	rv = newRV_inc(&PL_sv_undef);
	sv_setref_iv(rv, "LSF::Base::rusagePtr",(IV)ru);
	XPUSHs(sv_2mortal(rv));
	XSRETURN(2);

void
ls_rwaittid(self, tid, options)
	void	*self
	int	tid
	int	options
    PREINIT:
	LSF_Base_rusage *ru;
	LS_WAIT_T status;
	SV *rv;
    PPCODE:
	ru = safemalloc(sizeof(LSF_Base_rusage));
	tid = ls_rwaittid(tid, &status, options, ru);
	if( tid < 0 ){
	   safefree(ru);
	   STATUS_NATIVE_SET(lserrno);
	   SET_LSF_ERRMSG;
	   XSRETURN_UNDEF;
	}
	STATUS_NATIVE_SET(status);
	rv = newRV_inc(&PL_sv_undef);
	sv_setref_iv(rv, "LSF::Base::rusagePtr",(IV)ru);
	XPUSHs(sv_2mortal(rv));
	XSRETURN(1);


int 
ls_rkill(self, tid, sig)
	void	*self
	int	tid
	int	sig
    CODE:
	if( ls_rkill(tid, sig) < 0 ){
	   STATUS_NATIVE_SET(lserrno);
	   SET_LSF_ERRMSG;
           RETVAL = 0;
        }
        RETVAL = 1;
    OUTPUT:
	RETVAL

int
ls_rsetenv(self, host, envp)
	void	*self
	char	*host
	char 	**envp
    CODE:
	if(ls_rsetenv(host, envp) < 0 ){
	   STATUS_NATIVE_SET(lserrno);
	   SET_LSF_ERRMSG;
	   RETVAL = 0;
	}
	RETVAL = 1;
    OUTPUT:
	RETVAL

int 
ls_chdir(self, host, clntdir)
	void	*self	
	char 	*host
	char 	*clntdir
   CODE:
	if(ls_chdir(host, clntdir) < 0){
	   STATUS_NATIVE_SET(lserrno);
	   SET_LSF_ERRMSG;
	   RETVAL = 0;
	}
	else
	   RETVAL = 1;
    OUTPUT:
	RETVAL

int
ls_stdinmode(self, remote)
	void	*self;
	int	remote;
    CODE:
	if( ls_stdinmode(remote) < 0 ){
	   STATUS_NATIVE_SET(lserrno);
	   SET_LSF_ERRMSG;
	   RETVAL = 0;
	}
        else
	   RETVAL = 1;
    OUTPUT:
	RETVAL

void
ls_getstdin(self, on, max)
	void	*self;
	int	on;
	int	max;
   PREINIT:
	int	*tidlist;
	int	i, count;
   PPCODE:
	tidlist = safemalloc(sizeof(int)*max);
	if( (count = ls_getstdin(on, tidlist, max)) < 0 ){
	   safefree(tidlist);
	   STATUS_NATIVE_SET(lserrno);
	   SET_LSF_ERRMSG;
	   XSRETURN_UNDEF;
	}
	if(count == 0)
	{
	   XSRETURN_YES;   
	}
	for( i = 0; i < count; i++){
	   XPUSHs(sv_2mortal(newSViv(tidlist[i])));
	}
	safefree(tidlist);
	XSRETURN(count);

int
ls_setstdin(self, on, tidlist)
	void	*self;
	int	on;
	int*	tidlist;
    CODE:
	if( ls_setstdin(on, tidlist + 1, tidlist[0]) < 0 ){
	    STATUS_NATIVE_SET(lserrno);
	    SET_LSF_ERRMSG;
	    RETVAL = 0;
	}
	else
	   RETVAL = 1;
    OUTPUT:
	RETVAL
	   
int
ls_stoprex(self)
	void *self
    CODE:
	if(ls_stoprex() < 0 ){
	   STATUS_NATIVE_SET(lserrno);
	   SET_LSF_ERRMSG;
	   RETVAL = 0;
	}
        else
	   RETVAL = 1;
    OUTPUT:
	RETVAL

int
ls_donerex(self)
	void *self
    CODE:
	if(ls_donerex() < 0 ){
	   STATUS_NATIVE_SET(lserrno);
	   SET_LSF_ERRMSG;
	   RETVAL = 0;
	}
        else
	   RETVAL = 1;
    OUTPUT:
	RETVAL

FILE *
ls_conntaskport(self, tid)
	void 	*self
	int	tid
    PREINIT:
	int fd;
	char buf[1000];
	int r;
    CODE:
	if( fd = ls_conntaskport(tid) < 0 ){
	   STATUS_NATIVE_SET(lserrno);
	   SET_LSF_ERRMSG;
	   XSRETURN_UNDEF;
	}
	else{
	   RETVAL = fdopen(fd,"r+");
	}
    OUTPUT:
	RETVAL


MODULE = LSF::Base	PACKAGE = LSF::Base::rusagePtr	PREFIX = ru_

long
ru_utime_sec(self)
	LSF_Base_rusage *self;
    CODE:
	RETVAL = self->ru_utime.tv_sec;
    OUTPUT:
	RETVAL

long
ru_utime_usec(self)
	LSF_Base_rusage *self;
    CODE:
	RETVAL = self->ru_utime.tv_usec;
    OUTPUT:
	RETVAL

long
ru_stime_sec(self)
	LSF_Base_rusage *self;
    CODE:
	RETVAL = self->ru_utime.tv_sec;
    OUTPUT:
	RETVAL

long
ru_stime_usec(self)
	LSF_Base_rusage *self;
    CODE:
	RETVAL = self->ru_utime.tv_usec;
    OUTPUT:
	RETVAL

long 
ru_maxrss(self)
	LSF_Base_rusage *self;
    CODE:
	RETVAL = self->ru_maxrss;
    OUTPUT:
	RETVAL

long 
ru_ixrss(self)
	LSF_Base_rusage *self;
    CODE:
	RETVAL = self->ru_ixrss;
    OUTPUT:
	RETVAL

long 
ru_idrss(self)
	LSF_Base_rusage *self;
    CODE:
	RETVAL = self->ru_idrss;
    OUTPUT:
	RETVAL

long 
ru_isrss(self)
	LSF_Base_rusage *self
    CODE:
	RETVAL = self->ru_isrss;
    OUTPUT:
	RETVAL

long 
ru_minflt(self)
	LSF_Base_rusage *self
    CODE:
	RETVAL = self->ru_minflt;
    OUTPUT:
	RETVAL

long 
ru_majflt(self)
	LSF_Base_rusage *self;
    CODE:
	RETVAL = self->ru_majflt;
    OUTPUT:
	RETVAL

long 
ru_nswap(self)
	LSF_Base_rusage *self;
    CODE:
	RETVAL = self->ru_nswap;
    OUTPUT:
	RETVAL

long 
ru_inblock(self)
	LSF_Base_rusage *self;
    CODE:
	RETVAL = self->ru_inblock;
    OUTPUT:
	RETVAL

long 
ru_oublock(self)
	LSF_Base_rusage *self;
    CODE:
	RETVAL = self->ru_oublock;
    OUTPUT:
	RETVAL

long 
ru_msgsnd(self)
	LSF_Base_rusage *self;
    CODE:
	RETVAL = self->ru_msgsnd;
    OUTPUT:
	RETVAL

long 
ru_msgrcv(self)
	LSF_Base_rusage *self;
    CODE:
	RETVAL = self->ru_msgrcv;
    OUTPUT:
	RETVAL

long 
ru_nsignals(self)
	LSF_Base_rusage *self;
    CODE:
	RETVAL = self->ru_nsignals;
    OUTPUT:
	RETVAL

long 
ru_nvcsw(self)
	LSF_Base_rusage *self;
    CODE:
	RETVAL = self->ru_nvcsw;
    OUTPUT:
	RETVAL

long 
ru_nivcsw(self)
	LSF_Base_rusage *self;
    CODE:
	RETVAL = self->ru_nivcsw;
    OUTPUT:
	RETVAL

MODULE = LSF::Base		PACKAGE = LSF::Base		PREFIX = ls_

 #remote file operations


int
ls_ropen (self, host, fn, flags, mode)
	void	*self
	char*	host
	char*	fn
	int 	flags
	int 	mode
    CODE:
	RETVAL = ls_ropen(host, fn, flags, mode);
	if( RETVAL < 0 ){
	   STATUS_NATIVE_SET(lserrno);
	   SET_LSF_ERRMSG;
           XSRETURN_UNDEF;
	}
    OUTPUT:
	RETVAL

int
ls_rclose(self, rfd)
	void	*self
	int	rfd
    CODE:
	if( ls_rclose(rfd) < 0 ){
	   STATUS_NATIVE_SET(lserrno);
	   SET_LSF_ERRMSG;
           RETVAL = 0;
	}
	RETVAL = 1;
    OUTPUT:
	RETVAL

int
ls_rwrite(self, rfd, buf, len)
	void	*self
	int 	rfd
	SV*	buf
	int	len
    PREINIT:
	char *b;
	STRLEN  l;
    CODE:
	b = (char*)SvPV(buf,l);
	if( len >= l ){
	   RETVAL = ls_rwrite(rfd, b, len);
	   if( RETVAL < 0 ){
	      STATUS_NATIVE_SET(lserrno);
	      SET_LSF_ERRMSG;
	      XSRETURN_UNDEF;
	   }
        }
	else{
	   sv_setpv(GvSV(PL_errgv),
                    "rwrite: buffer is smaller than requested write");
	   XSRETURN_UNDEF;
        }
    OUTPUT:
	RETVAL

int
ls_rread(self, rfd, buf, len)
	void	*self
	int	rfd
	SV*	buf
	int	len
    PREINIT:
	char *b;
    CODE:
	b = safemalloc(len);
	RETVAL = ls_rread(rfd, b, len);
	if( RETVAL < 0 ){
	   safefree(b);
	   STATUS_NATIVE_SET(lserrno);
	   SET_LSF_ERRMSG;
    	   XSRETURN_UNDEF;
	}
	sv_setpvn(buf,b,RETVAL);
	safefree(b);
    OUTPUT:
	RETVAL

int
ls_rlseek(self, rfd, offset, whence)
	void	*self
	int	rfd
	int	offset
	int	whence
    CODE:
	RETVAL = ls_rlseek(rfd, offset, whence);
	if( RETVAL < 0 ){
	   STATUS_NATIVE_SET(lserrno);
	   SET_LSF_ERRMSG;
    	   XSRETURN_UNDEF;
	}
    OUTPUT:
	RETVAL

int
ls_rfstat(self, rfd)
	void	*self
	int 	rfd
    PREINIT:
	struct stat st;
    PPCODE:
	/*make this stat look more like the perl stat function*/
	if( ls_rfstat(rfd, &st) < 0 ){
	    XSRETURN_UNDEF;
	}
	else{
	    XPUSHs(sv_2mortal(newSViv(st.st_dev)));
	    XPUSHs(sv_2mortal(newSViv(st.st_ino)));
	    XPUSHs(sv_2mortal(newSViv(st.st_mode)));
	    XPUSHs(sv_2mortal(newSViv(st.st_nlink)));
	    XPUSHs(sv_2mortal(newSViv(st.st_uid)));
	    XPUSHs(sv_2mortal(newSViv(st.st_gid)));
	    XPUSHs(sv_2mortal(newSViv(st.st_rdev)));
	    XPUSHs(sv_2mortal(newSViv(st.st_size)));
	    XPUSHs(sv_2mortal(newSViv(st.st_atime)));
	    XPUSHs(sv_2mortal(newSViv(st.st_mtime)));
	    XPUSHs(sv_2mortal(newSViv(st.st_ctime)));
	    XPUSHs(sv_2mortal(newSViv(st.st_blksize)));
	    XPUSHs(sv_2mortal(newSViv(st.st_blocks)));
	}
	XSRETURN(13);
	

int
ls_rstat(self, host, fn)
	void	*self
	char*	host
	char*	fn
    PREINIT:
	struct stat st;
    PPCODE:
	/*make this stat look more like the perl stat function*/
	if( ls_rstat(host, fn, &st) < 0 ){
	    XSRETURN_UNDEF;
	}
	else{
	    XPUSHs(sv_2mortal(newSViv(st.st_dev)));
	    XPUSHs(sv_2mortal(newSViv(st.st_ino)));
	    XPUSHs(sv_2mortal(newSViv(st.st_mode)));
	    XPUSHs(sv_2mortal(newSViv(st.st_nlink)));
	    XPUSHs(sv_2mortal(newSViv(st.st_uid)));
	    XPUSHs(sv_2mortal(newSViv(st.st_gid)));
	    XPUSHs(sv_2mortal(newSViv(st.st_rdev)));
	    XPUSHs(sv_2mortal(newSViv(st.st_size)));
	    XPUSHs(sv_2mortal(newSViv(st.st_atime)));
	    XPUSHs(sv_2mortal(newSViv(st.st_mtime)));
	    XPUSHs(sv_2mortal(newSViv(st.st_ctime)));
	    XPUSHs(sv_2mortal(newSViv(st.st_blksize)));
	    XPUSHs(sv_2mortal(newSViv(st.st_blocks)));
	}
	XSRETURN(13);

char*
ls_getmnthost(self, file)
	void	*self
	char*	file
    CODE:
	RETVAL = ls_getmnthost(file);
	if( RETVAL == NULL ){
	   STATUS_NATIVE_SET(lserrno);
	   SET_LSF_ERRMSG;
	   XSRETURN_UNDEF;
	}
    OUTPUT:
	RETVAL

char*
ls_rgetmnthost(self, host, file)
	void	*self
	char*	host
	char*	file
    CODE:
	RETVAL = ls_rgetmnthost(host, file);
	if( RETVAL == NULL ){
	   STATUS_NATIVE_SET(lserrno);
	   SET_LSF_ERRMSG;
	   XSRETURN_UNDEF;
	}
    OUTPUT:
	RETVAL

#if LSF_VERSION < 9

int
ls_rfcontrol(self, command, arg)
	void	*self
	int 	command
	SV* 	arg
    PREINIT:
	char	*hostname;
	int	max;
	int	len;
    CODE:
	switch(command){
	  case RF_CMD_TERMINATE:
	     hostname = (char *)SvPV(arg,len);
  	     RETVAL = ls_rfcontrol(command, (int)hostname);
	     break;
	  default: /*RF_CMD_MAXHOSTS and others*/
	     /*let rfcontrol handle the case of a bad command*/
	     max = SvIV(arg);
	     RETVAL = ls_rfcontrol(command, max);
        }
        if( RETVAL < 0 ){
	   STATUS_NATIVE_SET(lserrno);
	   SET_LSF_ERRMSG;
	   XSRETURN_UNDEF;
        }
    OUTPUT:
	RETVAL

#else

int
ls_rfcontrol(self, command, arg)
	void	*self
	int 	command
	int 	arg
    CODE:
        /*let rfcontrol handle the case of a bad command*/
        RETVAL = ls_rfcontrol(command, arg);
        if( RETVAL < 0 ){
	   STATUS_NATIVE_SET(lserrno);
	   SET_LSF_ERRMSG;
	   XSRETURN_UNDEF;
        }
    OUTPUT:
	RETVAL

int
ls_rfterminate(self, host)
	void *self
	char *host
   CODE:
	RETVAL = ls_rfterminate(host);
        if( RETVAL < 0 ){
	   STATUS_NATIVE_SET(lserrno);
	   SET_LSF_ERRMSG;
	   XSRETURN_UNDEF;
        }
    OUTPUT:
	RETVAL

#endif

int 
ls_lockhost(self, duration)
	void	*self
	long	duration
    CODE:
	if( ls_lockhost((time_t)duration) < 0 ){
	   STATUS_NATIVE_SET(lserrno);
	   SET_LSF_ERRMSG;
	   RETVAL = 0;
	}
	else
	RETVAL = 1;
    OUTPUT:
	RETVAL

int 
ls_unlockhost(self)
	void	*self
    CODE:
	RETVAL = ls_unlockhost();
    OUTPUT:
	RETVAL

int
ls_limcontrol(self, hostname, opCode)
	void	*self
	char*	hostname
	int	opCode
    CODE:
	if( ls_limcontrol(hostname, opCode) < 0 ){
	   STATUS_NATIVE_SET(lserrno);
	   SET_LSF_ERRMSG;
	   RETVAL = 0;
	}
	else
	   RETVAL = 1;
    OUTPUT:
	RETVAL

int
ls_rescontrol(self, host, opCode, data)
	void	*self
	char*	host
	int 	opCode
	int 	data
    CODE:
	if( ls_rescontrol(host, opCode, data) < 0 ){
	   STATUS_NATIVE_SET(lserrno);
	   SET_LSF_ERRMSG;
	   RETVAL = 0;
	}
	else
	   RETVAL = 1;
    OUTPUT:
	RETVAL


 # The function prototype in lsf.h does not match the documentation 
 # for this in the manual pages.
 #
 #void
 #ls_readrexlog(self, fp)
 #	void	*self;
 #	FILE*	fp;
 #     PREINIT:
 # 	LSF_Base_lsfAcctRec *ar;
 #	SV *rv;
 #    PPCODE:
 #	if( (ar = ls_readrexlog(fp)) == NULL ){
 #	   STATUS_NATIVE_SET(lserrno);
 #	   SET_LSF_ERRMSG;
 #	   XSRETURN_UNDEF;
 #	}
 #	rv = newRV_inc(&PL_sv_undef);
 #	sv_setref_iv(rv, "LSF::Base::lsfAcctRecPtr",(IV)ar);
 #	XPUSHs(sv_2mortal(rv));
 #	XSRETURN(1);

#MODULE = LSF::Base	PACKAGE = LSF::Base::lsfAcctRecPtr	PREFIX = ar_
#
#int
#ar_pid(self)
#	LSF_Base_lsfAcctRec *self;
#    CODE:
#	RETVAL = self->pid;
#    OUTPUT:
#	RETVAL
#
#char*
#ar_username(self)
#	LSF_Base_lsfAcctRec *self;
#    CODE:
#	RETVAL = self->username;
#    OUTPUT:
#	RETVAL
#
#int
#ar_exitStatus(self)
#	LSF_Base_lsfAcctRec *self;
#    CODE:
#	RETVAL = self->exitStatus;
#    OUTPUT:
#	RETVAL
#
#int
#ar_dispTime(self)
#	LSF_Base_lsfAcctRec *self;
#    CODE:
#	RETVAL = self->dispTime;
#    OUTPUT:
#	RETVAL
#
#int
#ar_termTime(self)
#	LSF_Base_lsfAcctRec *self;
#    CODE:
#	RETVAL = self->termTime;
#    OUTPUT:
#	RETVAL
#
#char*
#ar_fromHost(self)
#	LSF_Base_lsfAcctRec *self;
#    CODE:
#	RETVAL = self->fromHost;
#    OUTPUT:
#	RETVAL
#
#char*
#ar_execHost(self)
#	LSF_Base_lsfAcctRec *self;
#    CODE:
#	RETVAL = self->execHost;
#    OUTPUT:
#	RETVAL
#
#char*
#ar_cwd(self)
#	LSF_Base_lsfAcctRec *self;
#    CODE:
#	RETVAL = self->cwd;
#    OUTPUT:
#	RETVAL
#
#char*
#ar_cmdln(self)
#	LSF_Base_lsfAcctRec *self;
#    CODE:
#	RETVAL = self->cmdln;
#    OUTPUT:
#	RETVAL
#
#void
#ar_lsfRu(self)
#	LSF_Base_lsfAcctRec *self;
#    PREINIT:
#	SV* rv;
#    PPCODE:
#	rv = newRV_inc(&PL_sv_undef);
#	sv_setref_iv(rv, "LSF::Base::rusagePtr",(IV)&self->lsfRu);
#	XPUSHs(sv_2mortal(rv));
#	XSRETURN(1);

MODULE = LSF::Base		PACKAGE = LSF::Base		PREFIX = ls_

# Error Handling

void
ls_perror(self, usrMsg)
	void	*self
	char*	usrMsg
    CODE:
	ls_perror(usrMsg);


char *
ls_sysmsg(self)
	void	*self
    CODE:
	RETVAL = ls_sysmsg();
    OUTPUT:
	RETVAL

int
ls_errno(self)
	void	*self
    CODE:
	RETVAL = lserrno;
    OUTPUT:
	RETVAL

void	
ls_errlog(self, fp, msg)
	void	*self
	FILE*	fp
	char*	msg
    CODE:
	ls_errlog(fp,msg);

 # Miscellaneous

int
ls_fdbusy(self, fileH)
	void	*self
	FILE *     fileH;
    CODE:
	RETVAL = ls_fdbusy(fileno(fileH));
    OUTPUT:
	RETVAL

