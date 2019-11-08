/*
 * This is the XS glue between libstatgrab and a Perl-API
 * named Unix::Statgrab.
 *
 * This file and the compiled artifacts from it can be distributed
 * under the same license as Perl 5 or the same license as
 * libstatgrab based on your choice.
 *
 *  Copyright (C) 2004-2005 by Tassilo von Parseval
 *  Copyright (C) 2012-2018 by Jens Rehsack
 */
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

#define NEED_newSVpvn_flags

#include "ppport.h"

#include <statgrab.h>

#include "const-c.inc"
#include "config.h"

const char *sg_host_info_names[] = {
    "os_name", "os_release", "os_version", "platform", "hostname",
    "bitwidth", "host_state", "ncpus", "maxcpus", "uptime", "systime"
};

const char *sg_cpu_stat_names[] = {
    "user", "kernel", "idle", "iowait", "swap", "nice", "total",
    "context_switches", "voluntary_context_switches", "involuntary_context_switches",
    "syscalls", "interrupts", "soft_interrupts", "systime"
};

const char *sg_cpu_percent_names[] = {
    "user", "kernel", "idle", "iowait", "swap", "nice", "time_taken"
};

const char *sg_mem_stat_names[] = {
    "total", "free", "used", "cache", "systime"
};

const char *sg_load_stat_names[] = {
    "min1", "min5", "min15", "systime"
};

const char *sg_user_stat_names[] = {
    "login_name", "record_id", "device", "hostname",
    "pid", "login_time", "systime"
};

const char *sg_swap_stat_names[] = {
    "total", "free", "used", "systime"
};

const char *sg_fs_stat_names[] = {
    "device_name", "device_canonical", "fs_type", "mnt_point", "device_type", "size",
    "used", "free", "avail", "total_inodes", "used_inodes", "free_inodes", "avail_inodes",
    "io_size", "block_size", "total_blocks", "free_blocks", "used_blocks", "avail_blocks",
    "systime"
};

const char *sg_disk_io_stat_names[] = {
    "disk_name", "read_bytes", "write_bytes", "systime"
};

const char *sg_network_io_stat_names[] = {
    "interface_name", "tx", "rx", "ipackets", "opackets",
    "ierrors", "oerrors", "collisions", "systime"
};

const char *sg_network_iface_stat_names[] = {
    "interface_name", "speed", "factor", "duplex", "up", "systime"
};

const char *sg_page_stat_names[] = {
    "pages_pagein", "pages_pageout", "systime"
};

const char *sg_process_stat_names[] = {
    "process_name", "proctitle",
    "pid", "parent", "pgid", "sessid", "uid", "euid", "gid", "egid",
    "context_switches", "voluntary_context_switches", "involuntary_context_switches",
    "proc_size", "proc_resident", "start_time", "time_spent", "cpu_percent",
    "nice", "state", "systime"
};

#ifndef lengthof
#define lengthof(x) (sizeof(x)/sizeof((x)[0]))
#endif

#ifdef MATCH_UV_FIT_ULL
#define LUV UV
#else
#define LUV NV
#endif

static STRLEN SAFE_STRLEN(const char *s)
{
    return s ? strlen(s) : 0;
}

#define sv_setpvn_IF(sv,str,len) (str)?sv_setpvn((sv),(str),(len)):((void)0)

#define MAKE_AV_FROM_STRINGS(strings, av) do { \
    size_t i; \
    av = newAV(); \
    av_extend(av, lengthof(strings)); \
    for(i = 0; i < lengthof(strings); ++i) { \
	av_store(av, i, newSVpvn(strings[i], SAFE_STRLEN(strings[i]))); \
    } \
} while(0)

#define FETCH_ALL_ROWS(stats, FETCH_ROW, av, XV) do { \
    size_t i, n; \
    av = newAV(); \
    av_extend(av, n = sg_get_nelements(stats)); \
    for(i = 0; i < n; ++i) { \
	XV *row; \
	FETCH_ROW(stats, i, row); \
	av_store(av, i, newRV_noinc((SV *)row)); \
    } \
} while(0)

MODULE = Unix::Statgrab		PACKAGE = Unix::Statgrab

INCLUDE: const-xs.inc

BOOT:
{
    /* sg_log_init(properties_pfx, env_name, argv0) */
    sg_log_init("libstatgrab", "LIBSTATGRAB_LOG_PROPERTIES", NULL);
    sg_init(1);
}

void
get_error ()
    PROTOTYPE: 
    CODE:
    {
	sg_error_details *self = safemalloc(sizeof(sg_error_details));

	if (SG_ERROR_NONE == sg_get_error_details(self))
	{
	    XSRETURN_UNDEF;
	}

	EXTEND(SP, 1);

	ST(0) = sv_newmortal();
	sv_setref_pv(ST(0), "Unix::Statgrab::sg_error_details", (void*)self);
	XSRETURN(1);
    }

int
drop_privileges ()
    PROTOTYPE:
    CODE:
	RETVAL = sg_drop_privileges() == 0;
    OUTPUT:
	RETVAL

void
get_host_info ()
    PROTOTYPE:
    CODE:
    {
	sg_host_info *self;
	if ((self = sg_get_host_info_r(NULL)) == NULL)
	    XSRETURN_UNDEF;

	EXTEND(SP, 1);

	ST(0) = sv_newmortal();
	sv_setref_pv(ST(0), "Unix::Statgrab::sg_host_info", (void*)self);
	XSRETURN(1);
    }

void
get_cpu_stats ()
    PROTOTYPE:
    CODE:
    {
	sg_cpu_stats *self;
	if ((self = sg_get_cpu_stats_r(NULL)) == NULL) 
	    XSRETURN_UNDEF;

	EXTEND(SP, 1);

	ST(0) = sv_newmortal();
	sv_setref_pv(ST(0), "Unix::Statgrab::sg_cpu_stats", (void*)self);
	XSRETURN(1);
    }

void
get_disk_io_stats ()
    PROTOTYPE:
    CODE:
    {
	sg_disk_io_stats *self;
	if ((self = sg_get_disk_io_stats_r(NULL)) == NULL)
	    XSRETURN_UNDEF;

	EXTEND(SP, 1);

	ST(0) = sv_newmortal();
	sv_setref_pv(ST(0), "Unix::Statgrab::sg_disk_io_stats", (void*)self);
	XSRETURN(1);
    }

void
get_fs_stats ()
    PROTOTYPE:
    CODE:
    {
	sg_fs_stats *self;

	if ((self = sg_get_fs_stats_r(NULL)) == NULL)
	    XSRETURN_UNDEF;

	EXTEND(SP, 1);

	ST(0) = sv_newmortal();
	sv_setref_pv(ST(0), "Unix::Statgrab::sg_fs_stats", (void*)self);
	XSRETURN(1);
    }

void
get_load_stats ()
    PROTOTYPE:
    CODE:
    {
	sg_load_stats *self;
	if ((self = sg_get_load_stats_r(NULL)) == NULL)
	    XSRETURN_UNDEF;
	
	EXTEND(SP, 1);

	ST(0) = sv_newmortal();
	sv_setref_pv(ST(0), "Unix::Statgrab::sg_load_stats", (void*)self);
	XSRETURN(1);
    }

void
get_mem_stats ()
    PROTOTYPE:
    CODE:
    {
	sg_mem_stats *self;
	if ((self = sg_get_mem_stats_r(NULL)) == NULL)
	    XSRETURN_UNDEF;

	EXTEND(SP, 1);

	ST(0) = sv_newmortal();
	sv_setref_pv(ST(0), "Unix::Statgrab::sg_mem_stats", (void*)self);
	XSRETURN(1);
    }

void
get_swap_stats ()
    PROTOTYPE:
    CODE:
    {
	sg_swap_stats *self;
	if ((self = sg_get_swap_stats_r(NULL)) == NULL)
	    XSRETURN_UNDEF;

	EXTEND(SP, 1);

	ST(0) = sv_newmortal();
	sv_setref_pv(ST(0), "Unix::Statgrab::sg_swap_stats", (void*)self);
	XSRETURN(1);
    }

void
get_network_io_stats ()
    PROTOTYPE:
    CODE:
    {
	sg_network_io_stats *self;

	if ((self = sg_get_network_io_stats_r(NULL)) == NULL)
	    XSRETURN_UNDEF;

	EXTEND(SP, 1);

	ST(0) = sv_newmortal();
	sv_setref_pv(ST(0), "Unix::Statgrab::sg_network_io_stats", (void*)self);
	XSRETURN(1);
    }
	
void
get_network_iface_stats ()
    PROTOTYPE:
    CODE:
    {
	sg_network_iface_stats *self;

	if ((self = sg_get_network_iface_stats_r(NULL)) == NULL)
	    XSRETURN_UNDEF;

	EXTEND(SP, 1);

	ST(0) = sv_newmortal();
	sv_setref_pv(ST(0), "Unix::Statgrab::sg_network_iface_stats", (void*)self);
	XSRETURN(1);
    }

void
get_page_stats ()
    PROTOTYPE:
    CODE:
    {
	sg_page_stats *self;
	if ((self = sg_get_page_stats_r(NULL)) == NULL)
	    XSRETURN_UNDEF;

	EXTEND(SP, 1);

	ST(0) = sv_newmortal();
	sv_setref_pv(ST(0), "Unix::Statgrab::sg_page_stats", (void*)self);
	XSRETURN(1);
    }

void
get_user_stats ()
    PROTOTYPE:
    CODE:
    {
	sg_user_stats *self;
	if ((self = sg_get_user_stats_r(NULL)) == NULL)
	    XSRETURN_UNDEF;

	EXTEND(SP, 1);

	ST(0) = sv_newmortal();
	sv_setref_pv(ST(0), "Unix::Statgrab::sg_user_stats", (void*)self);
	XSRETURN(1);
    }

void
get_process_stats ()
    PROTOTYPE:
    CODE:
    {
	sg_process_stats *self;

	if ((self = sg_get_process_stats_r(NULL)) == NULL)
	    XSRETURN_UNDEF;

	EXTEND(SP, 1);

	ST(0) = sv_newmortal();
	sv_setref_pv(ST(0), "Unix::Statgrab::sg_process_stats", (void*)self);
	XSRETURN(1);
    }

MODULE = Unix::Statgrab	    PACKAGE = Unix::Statgrab::sg_error_details

UV
error (self)
	sg_error_details *self;
    CODE:
	RETVAL = self->error;
    OUTPUT:
	RETVAL

const char *
error_name(self)
	sg_error_details *self;
    CODE:
	RETVAL = sg_str_error(self->error);
    OUTPUT:
	RETVAL

IV
errno_value (self)
	sg_error_details *self;
    CODE:
	RETVAL = self->errno_value;
    OUTPUT:
	RETVAL

const char *
error_arg (self)
	sg_error_details *self;
    CODE:
	RETVAL = self->error_arg;
    OUTPUT:
	RETVAL

void
strperror (self)
	sg_error_details *self;
    CODE:
    {
	char *buf = NULL;
	if(NULL == sg_strperror(&buf, self))
	    XSRETURN_UNDEF;

	EXTEND(SP, 1);

	ST(0) = newSVpv(buf, 0);
	free(buf);
	sv_2mortal(ST(0));
	XSRETURN(1);
    }

void
DESTROY (self)
	sg_error_details *self;
    CODE:
    {
	Safefree(self);
    }

MODULE = Unix::Statgrab	    PACKAGE = Unix::Statgrab::sg_host_info

UV
entries (self)
	sg_host_info *self;
    CODE:
	RETVAL = sg_get_nelements(self);
    OUTPUT:
	RETVAL

char *
os_name (self, num = 0)
	sg_host_info *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].os_name;
    OUTPUT:
	RETVAL

char *
os_release (self, num = 0)
	sg_host_info *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].os_release;
    OUTPUT:
	RETVAL

char *
os_version (self, num = 0)
	sg_host_info *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].os_version;
    OUTPUT:
	RETVAL

char *
platform (self, num = 0)
	sg_host_info *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].platform;
    OUTPUT:
	RETVAL

char *
hostname (self, num = 0)
	sg_host_info *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].hostname;
    OUTPUT:
	RETVAL

UV
bitwidth (self, num = 0)
	sg_host_info *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].bitwidth;
    OUTPUT:
	RETVAL

UV
host_state (self, num = 0)
	sg_host_info *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].host_state;
    OUTPUT:
	RETVAL

UV
ncpus (self, num = 0)
	sg_host_info *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].ncpus;
    OUTPUT:
	RETVAL

UV
maxcpus (self, num = 0)
	sg_host_info *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].maxcpus;
    OUTPUT:
	RETVAL

IV
systime (self, num = 0)
	sg_host_info *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].systime;
    OUTPUT:
	RETVAL

IV
uptime (self, num = 0)
	sg_host_info *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].uptime;
    OUTPUT:
	RETVAL

void
colnames(self)
	sg_host_info *self;
    PPCODE:
        AV *retval;
	MAKE_AV_FROM_STRINGS(sg_host_info_names, retval);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

#define HOST_INFO_ARRAY_ROW(self, entry, av) \
    do { \
	av = newAV(); \
	av_extend(av, sizeof(sg_host_info_names)); \
	AvFILLp(av) = -1; \
	av_store(av, ++AvFILLp(av), newSVpvn(self[entry].os_name, SAFE_STRLEN(self[entry].os_name))); \
	av_store(av, ++AvFILLp(av), newSVpvn(self[entry].os_release, SAFE_STRLEN(self[entry].os_release))); \
	av_store(av, ++AvFILLp(av), newSVpvn(self[entry].os_version, SAFE_STRLEN(self[entry].os_version))); \
	av_store(av, ++AvFILLp(av), newSVpvn(self[entry].platform, SAFE_STRLEN(self[entry].platform))); \
	av_store(av, ++AvFILLp(av), newSVpvn(self[entry].hostname, SAFE_STRLEN(self[entry].hostname))); \
	av_store(av, ++AvFILLp(av), newSVuv(self[entry].bitwidth)); \
	av_store(av, ++AvFILLp(av), newSVuv(self[entry].host_state)); \
	av_store(av, ++AvFILLp(av), newSVuv(self[entry].ncpus)); \
	av_store(av, ++AvFILLp(av), newSVuv(self[entry].maxcpus)); \
	av_store(av, ++AvFILLp(av), newSViv(self[entry].uptime)); \
	av_store(av, ++AvFILLp(av), newSViv(self[entry].systime)); \
    } while(0);

#define HOST_INFO_HASH_ROW(self, entry, hv) \
    do { \
	hv = newHV(); \
	hv_store(hv, sg_host_info_names[0], strlen(sg_host_info_names[0]), newSVpvn(self[entry].os_name, SAFE_STRLEN(self[entry].os_name)), 0); \
	hv_store(hv, sg_host_info_names[1], strlen(sg_host_info_names[1]), newSVpvn(self[entry].os_release, SAFE_STRLEN(self[entry].os_release)), 0); \
	hv_store(hv, sg_host_info_names[2], strlen(sg_host_info_names[2]), newSVpvn(self[entry].os_version, SAFE_STRLEN(self[entry].os_version)), 0); \
	hv_store(hv, sg_host_info_names[3], strlen(sg_host_info_names[3]), newSVpvn(self[entry].platform, SAFE_STRLEN(self[entry].platform)), 0); \
	hv_store(hv, sg_host_info_names[4], strlen(sg_host_info_names[4]), newSVpvn(self[entry].hostname, SAFE_STRLEN(self[entry].hostname)), 0); \
	hv_store(hv, sg_host_info_names[5], strlen(sg_host_info_names[5]), newSVuv(self[entry].bitwidth), 0); \
	hv_store(hv, sg_host_info_names[6], strlen(sg_host_info_names[6]), newSVuv(self[entry].host_state), 0); \
	hv_store(hv, sg_host_info_names[7], strlen(sg_host_info_names[7]), newSVuv(self[entry].ncpus), 0); \
	hv_store(hv, sg_host_info_names[8], strlen(sg_host_info_names[8]), newSVuv(self[entry].maxcpus), 0); \
	hv_store(hv, sg_host_info_names[9], strlen(sg_host_info_names[9]), newSViv(self[entry].uptime), 0); \
	hv_store(hv, sg_host_info_names[10], strlen(sg_host_info_names[10]), newSViv(self[entry].systime), 0); \
    } while(0);

void
fetchrow_arrayref(self, num = 0)
	sg_host_info *self;
	UV num;
    PPCODE:
        AV *retval;
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	/* XXX add fill row macro here */
	HOST_INFO_ARRAY_ROW(self, num, retval);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
fetchall_arrayref(self)
	sg_host_info *self;
    PPCODE:
        AV *retval;
	/* XXX add fill row macro here */
	FETCH_ALL_ROWS(self, HOST_INFO_ARRAY_ROW, retval, AV);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
fetchrow_hashref(self, num = 0)
	sg_host_info *self;
	UV num;
    PPCODE:
        HV *retval;
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	/* XXX add fill row macro here */
	HOST_INFO_HASH_ROW(self, num, retval);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
fetchall_hashref(self)
	sg_host_info *self;
    PPCODE:
        AV *retval;
	/* XXX add fill row macro here */
	FETCH_ALL_ROWS(self, HOST_INFO_HASH_ROW, retval, HV);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
DESTROY (self)
	sg_host_info *self;
    CODE:
    {
	sg_free_host_info(self);
    }

MODULE = Unix::Statgrab		PACKAGE = Unix::Statgrab::sg_cpu_stats

UV
entries (self)
	sg_cpu_stats *self;
    CODE:
	RETVAL = sg_get_nelements(self);
    OUTPUT:
	RETVAL

LUV
user (self, num = 0)
	sg_cpu_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].user;
    OUTPUT:
	RETVAL

LUV
kernel (self, num = 0)
	sg_cpu_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].kernel;
    OUTPUT:
	RETVAL

LUV
idle (self, num = 0)
	sg_cpu_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].idle;
    OUTPUT:
	RETVAL

LUV
iowait (self, num = 0)
	sg_cpu_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].iowait;
    OUTPUT:
	RETVAL

LUV
swap (self, num = 0)
	sg_cpu_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].swap;
    OUTPUT:
	RETVAL

LUV
nice (self, num = 0)
	sg_cpu_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].nice;
    OUTPUT:
	RETVAL

LUV
total (self, num = 0)
	sg_cpu_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].total;
    OUTPUT:
	RETVAL

LUV
context_switches (self, num = 0)
	sg_cpu_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].context_switches;
    OUTPUT:
	RETVAL

LUV
voluntary_context_switches (self, num = 0)
	sg_cpu_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].voluntary_context_switches;
    OUTPUT:
	RETVAL

LUV
involuntary_context_switches (self, num = 0)
	sg_cpu_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].involuntary_context_switches;
    OUTPUT:
	RETVAL

LUV
syscalls (self, num = 0)
	sg_cpu_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].syscalls;
    OUTPUT:
	RETVAL

LUV
interrupts (self, num = 0)
	sg_cpu_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].interrupts;
    OUTPUT:
	RETVAL

LUV
soft_interrupts (self, num = 0)
	sg_cpu_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].soft_interrupts;
    OUTPUT:
	RETVAL

IV
systime (self, num = 0)
	sg_cpu_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].systime;
    OUTPUT:
	RETVAL

void
colnames(self)
	sg_cpu_stats *self;
    PPCODE:
        AV *retval;
	MAKE_AV_FROM_STRINGS(sg_cpu_stat_names, retval);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

#define CPU_STATS_ARRAY_ROW(self, entry, av) \
    do { \
	SV **ary; \
	size_t j; \
	av = newAV(); \
	av_extend(av, lengthof(sg_cpu_stat_names)); \
	ary = AvARRAY(av); \
	AvFILLp(av) = lengthof(sg_cpu_stat_names)-1; \
	for( j = 0; j < lengthof(sg_cpu_stat_names); ++j ) \
	    ary[j] = newSV(0); \
	sv_setuv(ary[0], self[entry].user); \
	sv_setuv(ary[1], self[entry].kernel); \
	sv_setuv(ary[2], self[entry].idle); \
	sv_setuv(ary[3], self[entry].iowait); \
	sv_setuv(ary[4], self[entry].swap); \
	sv_setuv(ary[5], self[entry].nice); \
	sv_setuv(ary[6], self[entry].total); \
	sv_setuv(ary[7], self[entry].context_switches); \
	sv_setuv(ary[8], self[entry].voluntary_context_switches); \
	sv_setuv(ary[9], self[entry].involuntary_context_switches); \
	sv_setuv(ary[10], self[entry].syscalls); \
	sv_setuv(ary[11], self[entry].interrupts); \
	sv_setuv(ary[12], self[entry].soft_interrupts); \
	sv_setiv(ary[13], self[entry].systime); \
    } while(0)

#define CPU_STATS_HASH_ROW(self, entry, hv) \
    do { \
	hv = newHV(); \
	hv_store(hv, sg_cpu_stat_names[0], strlen(sg_cpu_stat_names[0]), newSVuv(self[entry].user), 0); \
	hv_store(hv, sg_cpu_stat_names[1], strlen(sg_cpu_stat_names[1]), newSVuv(self[entry].kernel), 0); \
	hv_store(hv, sg_cpu_stat_names[2], strlen(sg_cpu_stat_names[2]), newSVuv(self[entry].idle), 0); \
	hv_store(hv, sg_cpu_stat_names[3], strlen(sg_cpu_stat_names[3]), newSVuv(self[entry].iowait), 0); \
	hv_store(hv, sg_cpu_stat_names[4], strlen(sg_cpu_stat_names[4]), newSVuv(self[entry].swap), 0); \
	hv_store(hv, sg_cpu_stat_names[5], strlen(sg_cpu_stat_names[5]), newSVuv(self[entry].nice), 0); \
	hv_store(hv, sg_cpu_stat_names[6], strlen(sg_cpu_stat_names[6]), newSVuv(self[entry].total), 0); \
	hv_store(hv, sg_cpu_stat_names[7], strlen(sg_cpu_stat_names[7]), newSVuv(self[entry].context_switches), 0); \
	hv_store(hv, sg_cpu_stat_names[8], strlen(sg_cpu_stat_names[8]), newSVuv(self[entry].voluntary_context_switches), 0); \
	hv_store(hv, sg_cpu_stat_names[9], strlen(sg_cpu_stat_names[9]), newSVuv(self[entry].involuntary_context_switches), 0); \
	hv_store(hv, sg_cpu_stat_names[10], strlen(sg_cpu_stat_names[10]), newSVuv(self[entry].syscalls), 0); \
	hv_store(hv, sg_cpu_stat_names[11], strlen(sg_cpu_stat_names[11]), newSVuv(self[entry].interrupts), 0); \
	hv_store(hv, sg_cpu_stat_names[12], strlen(sg_cpu_stat_names[12]), newSVuv(self[entry].soft_interrupts), 0); \
	hv_store(hv, sg_cpu_stat_names[13], strlen(sg_cpu_stat_names[13]), newSViv(self[entry].systime), 0); \
    } while(0)

void
fetchrow_arrayref(self, num = 0)
	sg_cpu_stats *self;
	UV num;
    CODE:
        AV *retval;
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	/* XXX add fill row macro here */
	CPU_STATS_ARRAY_ROW(self, num, retval);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
fetchall_arrayref(self)
	sg_cpu_stats *self;
    CODE:
        AV *retval;
	/* XXX add fill row macro here */
	FETCH_ALL_ROWS(self, CPU_STATS_ARRAY_ROW, retval, AV);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
fetchrow_hashref(self, num = 0)
	sg_cpu_stats *self;
	UV num;
    CODE:
        HV *retval;
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	/* XXX add fill row macro here */
	CPU_STATS_HASH_ROW(self, num, retval);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
fetchall_hashref(self)
	sg_cpu_stats *self;
    CODE:
        AV *retval;
	/* XXX add fill row macro here */
	FETCH_ALL_ROWS(self, CPU_STATS_HASH_ROW, retval, HV);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);


void
DESTROY (self)
	sg_cpu_stats *self;
    CODE:
    {
	sg_free_cpu_stats(self);
    }

void
get_cpu_stats_diff (now, last)
	sg_cpu_stats *now;
	sg_cpu_stats *last;
    CODE:
    {
	sg_cpu_stats *self;
	if ((self = sg_get_cpu_stats_diff_between(now, last, NULL)) == NULL)
	    XSRETURN_UNDEF;

	EXTEND(SP, 1);

	ST(0) = sv_newmortal();
	sv_setref_pv(ST(0), "Unix::Statgrab::sg_cpu_stats", (void*)self);
	XSRETURN(1);
    }

void
get_cpu_percents (of)
	sg_cpu_stats *of;
    CODE:
    {
	sg_cpu_percents *self;
	if ((self = sg_get_cpu_percents_r(of, NULL)) == NULL) 
	    XSRETURN_UNDEF;

	EXTEND(SP, 1);

	ST(0) = sv_newmortal();
	sv_setref_pv(ST(0), "Unix::Statgrab::sg_cpu_percents", (void*)self);
	XSRETURN(1);
    }

MODULE = Unix::Statgrab		PACKAGE = Unix::Statgrab::sg_cpu_percents

UV
entries (self)
	sg_cpu_percents *self;
    CODE:
	RETVAL = sg_get_nelements(self);
    OUTPUT:
	RETVAL

NV
user (self, num = 0)
	sg_cpu_percents *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].user;
    OUTPUT:
	RETVAL

NV
kernel (self, num = 0)
	sg_cpu_percents *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].kernel;
    OUTPUT:
	RETVAL

NV
idle (self, num = 0)
	sg_cpu_percents *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].idle;
    OUTPUT:
	RETVAL

NV
iowait (self, num = 0)
	sg_cpu_percents *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].iowait;
    OUTPUT:
	RETVAL

NV
swap (self, num = 0)
	sg_cpu_percents *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].swap;
    OUTPUT:
	RETVAL

NV
nice (self, num = 0)
	sg_cpu_percents *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].nice;
    OUTPUT:
	RETVAL

UV
time_taken (self, num = 0)
	sg_cpu_percents *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].time_taken;
    OUTPUT:
	RETVAL

void
colnames(self)
	sg_cpu_percents *self;
    PPCODE:
        AV *retval;
	MAKE_AV_FROM_STRINGS(sg_cpu_percent_names, retval);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

#define CPU_PERCENTS_ARRAY_ROW(self, entry, av) \
    do { \
	SV **ary; \
	size_t j; \
	av = newAV(); \
	av_extend(av, lengthof(sg_cpu_percent_names)); \
	ary = AvARRAY(av); \
	AvFILLp(av) = lengthof(sg_cpu_percent_names)-1; \
	for( j = 0; j < lengthof(sg_cpu_percent_names); ++j ) \
	    ary[j] = newSV(0); \
	sv_setnv(ary[0], self[entry].user); \
	sv_setnv(ary[1], self[entry].kernel); \
	sv_setnv(ary[2], self[entry].idle); \
	sv_setnv(ary[3], self[entry].iowait); \
	sv_setnv(ary[4], self[entry].swap); \
	sv_setnv(ary[5], self[entry].nice); \
	sv_setiv(ary[6], self[entry].time_taken); \
    } while(0)

#define CPU_PERCENTS_HASH_ROW(self, entry, hv) \
    do { \
	hv = newHV(); \
	hv_store(hv, sg_cpu_percent_names[0], strlen(sg_cpu_percent_names[0]), newSVnv(self[entry].user), 0); \
	hv_store(hv, sg_cpu_percent_names[1], strlen(sg_cpu_percent_names[1]), newSVnv(self[entry].kernel), 0); \
	hv_store(hv, sg_cpu_percent_names[2], strlen(sg_cpu_percent_names[2]), newSVnv(self[entry].idle), 0); \
	hv_store(hv, sg_cpu_percent_names[3], strlen(sg_cpu_percent_names[3]), newSVnv(self[entry].iowait), 0); \
	hv_store(hv, sg_cpu_percent_names[4], strlen(sg_cpu_percent_names[4]), newSVnv(self[entry].swap), 0); \
	hv_store(hv, sg_cpu_percent_names[5], strlen(sg_cpu_percent_names[5]), newSVnv(self[entry].nice), 0); \
	hv_store(hv, sg_cpu_percent_names[6], strlen(sg_cpu_percent_names[6]), newSViv(self[entry].time_taken), 0); \
    } while(0)

void
fetchrow_arrayref(self, num = 0)
	sg_cpu_percents *self;
	UV num;
    CODE:
        AV *retval;
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	/* XXX add fill row macro here */
	CPU_PERCENTS_ARRAY_ROW(self, num, retval);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
fetchall_arrayref(self)
	sg_cpu_percents *self;
    CODE:
        AV *retval;
	/* XXX add fill row macro here */
	FETCH_ALL_ROWS(self, CPU_PERCENTS_ARRAY_ROW, retval, AV);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
fetchrow_hashref(self, num = 0)
	sg_cpu_percents *self;
	UV num;
    CODE:
        HV *retval;
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	/* XXX add fill row macro here */
	CPU_PERCENTS_HASH_ROW(self, num, retval);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
fetchall_hashref(self)
	sg_cpu_percents *self;
    CODE:
        AV *retval;
	/* XXX add fill row macro here */
	FETCH_ALL_ROWS(self, CPU_PERCENTS_HASH_ROW, retval, HV);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
DESTROY (self)
	sg_cpu_percents *self;
    CODE:
    {
	sg_free_cpu_percents(self);
    }

MODULE = Unix::Statgrab		PACKAGE = Unix::Statgrab::sg_disk_io_stats

UV
entries (self)
	sg_disk_io_stats *self;
    CODE:
	RETVAL = sg_get_nelements(self);
    OUTPUT:
	RETVAL

char *
disk_name (self, num = 0)
	sg_disk_io_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].disk_name;
    OUTPUT:
	RETVAL

LUV
read_bytes (self, num = 0)
	sg_disk_io_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].read_bytes;
    OUTPUT:
	RETVAL

LUV
write_bytes (self, num = 0)
	sg_disk_io_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].write_bytes;
    OUTPUT:
	RETVAL

IV
systime (self, num = 0)
	sg_disk_io_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].systime;
    OUTPUT:
	RETVAL

void
colnames(self)
	sg_disk_io_stats *self;
    PPCODE:
        AV *retval;
	MAKE_AV_FROM_STRINGS(sg_disk_io_stat_names, retval);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

#define DISK_IO_STATS_ARRAY_ROW(self, entry, av) \
    do { \
	SV **ary; \
	size_t j; \
	av = newAV(); \
	av_extend(av, lengthof(sg_disk_io_stat_names)); \
	ary = AvARRAY(av); \
	AvFILLp(av) = lengthof(sg_disk_io_stat_names)-1; \
	for( j = 0; j < lengthof(sg_disk_io_stat_names); ++j ) \
	    ary[j] = newSV(0); \
	sv_setpvn_IF(ary[0], self[entry].disk_name, SAFE_STRLEN(self[entry].disk_name)); \
	sv_setuv(ary[1], self[entry].read_bytes); \
	sv_setuv(ary[2], self[entry].write_bytes); \
	sv_setiv(ary[3], self[entry].systime); \
    } while(0)

#define DISK_IO_STATS_HASH_ROW(self, entry, hv) \
    do { \
	hv = newHV(); \
	hv_store(hv, sg_disk_io_stat_names[0], strlen(sg_disk_io_stat_names[0]), newSVpvn(self[entry].disk_name, SAFE_STRLEN(self[entry].disk_name)), 0); \
	hv_store(hv, sg_disk_io_stat_names[1], strlen(sg_disk_io_stat_names[1]), newSVuv(self[entry].read_bytes), 0); \
	hv_store(hv, sg_disk_io_stat_names[2], strlen(sg_disk_io_stat_names[2]), newSVuv(self[entry].write_bytes), 0); \
	hv_store(hv, sg_disk_io_stat_names[3], strlen(sg_disk_io_stat_names[3]), newSViv(self[entry].systime), 0); \
    } while(0)

void
fetchrow_arrayref(self, num = 0)
	sg_disk_io_stats *self;
	UV num;
    CODE:
        AV *retval;
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	/* XXX add fill row macro here */
	DISK_IO_STATS_ARRAY_ROW(self, num, retval);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
fetchall_arrayref(self)
	sg_disk_io_stats *self;
    CODE:
        AV *retval;
	/* XXX add fill row macro here */
	FETCH_ALL_ROWS(self, DISK_IO_STATS_ARRAY_ROW, retval, AV);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
fetchrow_hashref(self, num = 0)
	sg_disk_io_stats *self;
	UV num;
    CODE:
        HV *retval;
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	/* XXX add fill row macro here */
	DISK_IO_STATS_HASH_ROW(self, num, retval);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
fetchall_hashref(self)
	sg_disk_io_stats *self;
    CODE:
        AV *retval;
	/* XXX add fill row macro here */
	FETCH_ALL_ROWS(self, DISK_IO_STATS_HASH_ROW, retval, HV);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
DESTROY (self)
	sg_disk_io_stats *self;
    CODE:
    {
	sg_free_disk_io_stats(self);
    }

void
get_disk_io_stats_diff (now, last)
	sg_disk_io_stats *now;
	sg_disk_io_stats *last;
    CODE:
    {
	sg_disk_io_stats *self;

	if ((self = sg_get_disk_io_stats_diff_between(now, last, NULL)) == NULL)
	    XSRETURN_UNDEF;

	EXTEND(SP, 1);

	ST(0) = sv_newmortal();
	sv_setref_pv(ST(0), "Unix::Statgrab::sg_disk_io_stats", (void*)self);
	XSRETURN(1);
    }

MODULE = Unix::Statgrab		PACKAGE = Unix::Statgrab::sg_fs_stats

#ifndef HAVE_DEVICE_CANONICAL
#define device_canonical device_name
#endif

UV
entries (self)
	sg_fs_stats *self;
    CODE:
	RETVAL = sg_get_nelements(self);
    OUTPUT:
	RETVAL

char *
device_name (self, num = 0)
	sg_fs_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].device_name;
    OUTPUT:
	RETVAL

char *
device_canonical (self, num = 0)
	sg_fs_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].device_canonical;
    OUTPUT:
	RETVAL

char *
fs_type (self, num = 0)
	sg_fs_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].fs_type;
    OUTPUT:
	RETVAL

char *
mnt_point (self, num = 0)
	sg_fs_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].mnt_point;
    OUTPUT:
	RETVAL

UV
device_type (self, num = 0)
	sg_fs_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].device_type;
    OUTPUT:
	RETVAL

LUV
size (self, num = 0)
	sg_fs_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].size;
    OUTPUT:
	RETVAL

LUV
used (self, num = 0)
	sg_fs_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].used;
    OUTPUT:
	RETVAL

LUV
free (self, num = 0)
	sg_fs_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].free;
    OUTPUT:
	RETVAL

LUV
avail (self, num = 0)
	sg_fs_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].avail;
    OUTPUT:
	RETVAL

LUV
total_inodes (self, num = 0)
	sg_fs_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].total_inodes;
    OUTPUT:
	RETVAL

LUV
used_inodes (self, num = 0)
	sg_fs_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].used_inodes;
    OUTPUT:
	RETVAL

LUV
free_inodes (self, num = 0)
	sg_fs_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].free_inodes;
    OUTPUT:
	RETVAL

LUV
avail_inodes (self, num = 0)
	sg_fs_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].avail_inodes;
    OUTPUT:
	RETVAL

LUV
io_size (self, num = 0)
	sg_fs_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].io_size;
    OUTPUT:
	RETVAL

LUV
block_size (self, num = 0)
	sg_fs_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].block_size;
    OUTPUT:
	RETVAL

LUV
total_blocks (self, num = 0)
	sg_fs_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].total_blocks;
    OUTPUT:
	RETVAL

LUV
free_blocks (self, num = 0)
	sg_fs_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].free_blocks;
    OUTPUT:
	RETVAL

LUV
used_blocks (self, num = 0)
	sg_fs_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].used_blocks;
    OUTPUT:
	RETVAL

LUV
avail_blocks (self, num = 0)
	sg_fs_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].avail_blocks;
    OUTPUT:
	RETVAL

IV
systime (self, num = 0)
	sg_fs_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].systime;
    OUTPUT:
	RETVAL

void
colnames(self)
	sg_fs_stats *self;
    PPCODE:
        AV *retval;
	MAKE_AV_FROM_STRINGS(sg_fs_stat_names, retval);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

#define FS_STATS_ARRAY_ROW(self, entry, av) \
    do { \
	SV **ary; \
	size_t j; \
	av = newAV(); \
	av_extend(av, lengthof(sg_fs_stat_names)); \
	ary = AvARRAY(av); \
	AvFILLp(av) = lengthof(sg_fs_stat_names)-1; \
	for( j = 0; j < lengthof(sg_fs_stat_names); ++j ) \
	    ary[j] = newSV(0); \
	sv_setpvn_IF(ary[0], self[entry].device_name, SAFE_STRLEN(self[entry].device_name)); \
	sv_setpvn_IF(ary[1], self[entry].device_canonical, SAFE_STRLEN(self[entry].device_canonical)); \
	sv_setpvn_IF(ary[2], self[entry].fs_type, SAFE_STRLEN(self[entry].fs_type)); \
	sv_setpvn_IF(ary[3], self[entry].mnt_point, SAFE_STRLEN(self[entry].mnt_point)); \
	sv_setuv(ary[4], self[entry].device_type); \
	sv_setiv(ary[5], self[entry].size); \
	sv_setiv(ary[6], self[entry].used); \
	sv_setiv(ary[7], self[entry].free); \
	sv_setiv(ary[8], self[entry].avail); \
	sv_setiv(ary[9], self[entry].total_inodes); \
	sv_setuv(ary[10], self[entry].used_inodes); \
	sv_setuv(ary[11], self[entry].free_inodes); \
	sv_setuv(ary[12], self[entry].avail_inodes); \
	sv_setuv(ary[13], self[entry].io_size); \
	sv_setuv(ary[14], self[entry].block_size); \
	sv_setiv(ary[15], self[entry].total_blocks); \
	sv_setiv(ary[16], self[entry].free_blocks); \
	sv_setnv(ary[17], self[entry].used_blocks); \
	sv_setiv(ary[18], self[entry].avail_blocks); \
	sv_setiv(ary[19], self[entry].systime); \
    } while(0)

#define FS_STATS_HASH_ROW(self, entry, hv) \
    do { \
	hv = newHV(); \
	hv_store(hv, sg_fs_stat_names[0], strlen(sg_fs_stat_names[0]), newSVpvn(self[entry].device_name, SAFE_STRLEN(self[entry].device_name)), 0); \
	hv_store(hv, sg_fs_stat_names[1], strlen(sg_fs_stat_names[1]), newSVpvn(self[entry].device_canonical, SAFE_STRLEN(self[entry].device_canonical)), 0); \
	hv_store(hv, sg_fs_stat_names[2], strlen(sg_fs_stat_names[2]), newSVpvn(self[entry].fs_type, SAFE_STRLEN(self[entry].fs_type)), 0); \
	hv_store(hv, sg_fs_stat_names[3], strlen(sg_fs_stat_names[3]), newSVpvn(self[entry].mnt_point, SAFE_STRLEN(self[entry].mnt_point)), 0); \
	hv_store(hv, sg_fs_stat_names[4], strlen(sg_fs_stat_names[4]), newSVuv(self[entry].device_type), 0); \
	hv_store(hv, sg_fs_stat_names[5], strlen(sg_fs_stat_names[5]), newSVuv(self[entry].size), 0); \
	hv_store(hv, sg_fs_stat_names[6], strlen(sg_fs_stat_names[6]), newSVuv(self[entry].used), 0); \
	hv_store(hv, sg_fs_stat_names[7], strlen(sg_fs_stat_names[7]), newSVuv(self[entry].free), 0); \
	hv_store(hv, sg_fs_stat_names[8], strlen(sg_fs_stat_names[8]), newSVuv(self[entry].avail), 0); \
	hv_store(hv, sg_fs_stat_names[9], strlen(sg_fs_stat_names[9]), newSVuv(self[entry].total_inodes), 0); \
	hv_store(hv, sg_fs_stat_names[10], strlen(sg_fs_stat_names[10]), newSVuv(self[entry].used_inodes), 0); \
	hv_store(hv, sg_fs_stat_names[11], strlen(sg_fs_stat_names[11]), newSVuv(self[entry].free_inodes), 0); \
	hv_store(hv, sg_fs_stat_names[12], strlen(sg_fs_stat_names[12]), newSVuv(self[entry].avail_inodes), 0); \
	hv_store(hv, sg_fs_stat_names[13], strlen(sg_fs_stat_names[13]), newSVuv(self[entry].io_size), 0); \
	hv_store(hv, sg_fs_stat_names[14], strlen(sg_fs_stat_names[14]), newSVuv(self[entry].block_size), 0); \
	hv_store(hv, sg_fs_stat_names[15], strlen(sg_fs_stat_names[15]), newSVuv(self[entry].total_blocks), 0); \
	hv_store(hv, sg_fs_stat_names[16], strlen(sg_fs_stat_names[16]), newSVuv(self[entry].free_blocks), 0); \
	hv_store(hv, sg_fs_stat_names[17], strlen(sg_fs_stat_names[17]), newSVuv(self[entry].used_blocks), 0); \
	hv_store(hv, sg_fs_stat_names[18], strlen(sg_fs_stat_names[18]), newSVuv(self[entry].avail_blocks), 0); \
	hv_store(hv, sg_fs_stat_names[19], strlen(sg_fs_stat_names[19]), newSViv(self[entry].systime), 0); \
    } while(0)

void
fetchrow_arrayref(self, num = 0)
	sg_fs_stats *self;
	UV num;
    CODE:
        AV *retval;
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	/* XXX add fill row macro here */
	FS_STATS_ARRAY_ROW(self, num, retval);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
fetchall_arrayref(self)
	sg_fs_stats *self;
    CODE:
        AV *retval;
	/* XXX add fill row macro here */
	FETCH_ALL_ROWS(self, FS_STATS_ARRAY_ROW, retval, AV);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
fetchrow_hashref(self, num = 0)
	sg_fs_stats *self;
	UV num;
    CODE:
        HV *retval;
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	/* XXX add fill row macro here */
	FS_STATS_HASH_ROW(self, num, retval);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
fetchall_hashref(self)
	sg_fs_stats *self;
    CODE:
        AV *retval;
	/* XXX add fill row macro here */
	FETCH_ALL_ROWS(self, FS_STATS_HASH_ROW, retval, HV);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
DESTROY (self)
	sg_fs_stats *self;
    CODE:
    {
	sg_free_fs_stats(self);
    }

void
get_fs_stats_diff (now, last)
	sg_fs_stats *now;
	sg_fs_stats *last;
    CODE:
    {
	sg_fs_stats *self;

	if ((self = sg_get_fs_stats_diff_between(now, last, NULL)) == NULL)
	    XSRETURN_UNDEF;

	EXTEND(SP, 1);

	ST(0) = sv_newmortal();
	sv_setref_pv(ST(0), "Unix::Statgrab::sg_fs_stats", (void*)self);
	XSRETURN(1);
    }

#undef device_canonical

MODULE = Unix::Statgrab	    PACKAGE = Unix::Statgrab::sg_load_stats

UV
entries (self)
	sg_load_stats *self;
    CODE:
	RETVAL = sg_get_nelements(self);
    OUTPUT:
	RETVAL

NV
min1 (self, num = 0)
	sg_load_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].min1;
    OUTPUT:
	RETVAL

NV
min5 (self, num = 0)
	sg_load_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].min5;
    OUTPUT:
	RETVAL

NV
min15 (self, num = 0)
	sg_load_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].min15;
    OUTPUT:
	RETVAL

IV
systime (self, num = 0)
	sg_load_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].systime;
    OUTPUT:
	RETVAL

void
colnames(self)
	sg_load_stats *self;
    PPCODE:
        AV *retval;
	MAKE_AV_FROM_STRINGS(sg_load_stat_names, retval);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

#define LOAD_STATS_ARRAY_ROW(self, entry, av) \
    do { \
	SV **ary; \
	size_t j; \
	av = newAV(); \
	av_extend(av, lengthof(sg_load_stat_names)); \
	ary = AvARRAY(av); \
	AvFILLp(av) = lengthof(sg_load_stat_names)-1; \
	for( j = 0; j < lengthof(sg_load_stat_names); ++j ) \
	    ary[j] = newSV(0); \
	sv_setnv(ary[0], self[entry].min1); \
	sv_setnv(ary[1], self[entry].min5); \
	sv_setnv(ary[2], self[entry].min15); \
	sv_setiv(ary[3], self[entry].systime); \
    } while(0)

#define LOAD_STATS_HASH_ROW(self, entry, hv) \
    do { \
	hv = newHV(); \
	hv_store(hv, sg_load_stat_names[0], strlen(sg_load_stat_names[0]), newSVnv(self[entry].min1), 0); \
	hv_store(hv, sg_load_stat_names[1], strlen(sg_load_stat_names[1]), newSVnv(self[entry].min5), 0); \
	hv_store(hv, sg_load_stat_names[2], strlen(sg_load_stat_names[2]), newSVnv(self[entry].min15), 0); \
	hv_store(hv, sg_load_stat_names[3], strlen(sg_load_stat_names[3]), newSViv(self[entry].systime), 0); \
    } while(0)

void
fetchrow_arrayref(self, num = 0)
	sg_load_stats *self;
	UV num;
    CODE:
        AV *retval;
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	/* XXX add fill row macro here */
	LOAD_STATS_ARRAY_ROW(self, num, retval);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
fetchall_arrayref(self)
	sg_load_stats *self;
    CODE:
        AV *retval;
	/* XXX add fill row macro here */
	FETCH_ALL_ROWS(self, LOAD_STATS_ARRAY_ROW, retval, AV);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
fetchrow_hashref(self, num = 0)
	sg_load_stats *self;
	UV num;
    CODE:
        HV *retval;
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	/* XXX add fill row macro here */
	LOAD_STATS_HASH_ROW(self, num, retval);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
fetchall_hashref(self)
	sg_load_stats *self;
    CODE:
        AV *retval;
	/* XXX add fill row macro here */
	FETCH_ALL_ROWS(self, LOAD_STATS_HASH_ROW, retval, HV);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
DESTROY (self)
	sg_load_stats *self;
    CODE:
    {
	sg_free_load_stats(self);
    }

MODULE = Unix::Statgrab	    PACKAGE = Unix::Statgrab::sg_mem_stats

UV
entries (self)
	sg_mem_stats *self;
    CODE:
	RETVAL = sg_get_nelements(self);
    OUTPUT:
	RETVAL

LUV
total (self, num = 0)
	sg_mem_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].total;
    OUTPUT:
	RETVAL

LUV
free (self, num = 0)
	sg_mem_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].free;
    OUTPUT:
	RETVAL

LUV
used (self, num = 0)
	sg_mem_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].used;
    OUTPUT:
	RETVAL

LUV
cache (self, num = 0)
	sg_mem_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].cache;
    OUTPUT:
	RETVAL

IV
systime (self, num = 0)
	sg_mem_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].systime;
    OUTPUT:
	RETVAL

void
colnames(self)
	sg_mem_stats *self;
    PPCODE:
        AV *retval;
	MAKE_AV_FROM_STRINGS(sg_mem_stat_names, retval);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

#define MEM_STATS_ARRAY_ROW(self, entry, av) \
    do { \
	SV **ary; \
	size_t j; \
	av = newAV(); \
	av_extend(av, lengthof(sg_mem_stat_names)); \
	ary = AvARRAY(av); \
	AvFILLp(av) = lengthof(sg_mem_stat_names)-1; \
	for( j = 0; j < lengthof(sg_mem_stat_names); ++j ) \
	    ary[j] = newSV(0); \
	sv_setuv(ary[0], self[entry].total); \
	sv_setuv(ary[1], self[entry].free); \
	sv_setuv(ary[2], self[entry].used); \
	sv_setuv(ary[3], self[entry].cache); \
	sv_setiv(ary[4], self[entry].systime); \
    } while(0)

#define MEM_STATS_HASH_ROW(self, entry, hv) \
    do { \
	hv = newHV(); \
	hv_store(hv, sg_mem_stat_names[0], strlen(sg_mem_stat_names[0]), newSVuv(self[entry].total), 0); \
	hv_store(hv, sg_mem_stat_names[1], strlen(sg_mem_stat_names[1]), newSVuv(self[entry].free), 0); \
	hv_store(hv, sg_mem_stat_names[2], strlen(sg_mem_stat_names[2]), newSVuv(self[entry].used), 0); \
	hv_store(hv, sg_mem_stat_names[3], strlen(sg_mem_stat_names[3]), newSVuv(self[entry].cache), 0); \
	hv_store(hv, sg_mem_stat_names[4], strlen(sg_mem_stat_names[4]), newSViv(self[entry].systime), 0); \
    } while(0)

void
fetchrow_arrayref(self, num = 0)
	sg_mem_stats *self;
	UV num;
    CODE:
        AV *retval;
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	/* XXX add fill row macro here */
	MEM_STATS_ARRAY_ROW(self, num, retval);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
fetchall_arrayref(self)
	sg_mem_stats *self;
    CODE:
        AV *retval;
	/* XXX add fill row macro here */
	FETCH_ALL_ROWS(self, MEM_STATS_ARRAY_ROW, retval, AV);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
fetchrow_hashref(self, num = 0)
	sg_mem_stats *self;
	UV num;
    CODE:
        HV *retval;
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	/* XXX add fill row macro here */
	MEM_STATS_HASH_ROW(self, num, retval);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
fetchall_hashref(self)
	sg_mem_stats *self;
    CODE:
        AV *retval;
	/* XXX add fill row macro here */
	FETCH_ALL_ROWS(self, MEM_STATS_HASH_ROW, retval, HV);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
DESTROY (self)
	sg_mem_stats *self;
    CODE:
    {
	sg_free_mem_stats(self);
    }

MODULE = Unix::Statgrab     PACKAGE = Unix::Statgrab::sg_swap_stats

UV
entries (self)
	sg_swap_stats *self;
    CODE:
	RETVAL = sg_get_nelements(self);
    OUTPUT:
	RETVAL

LUV
total (self, num = 0)
	sg_swap_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].total;
    OUTPUT:
	RETVAL

LUV
free (self, num = 0)
	sg_swap_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].free;
    OUTPUT:
	RETVAL

LUV
used (self, num = 0)
	sg_swap_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].used;
    OUTPUT:
	RETVAL

IV
systime (self, num = 0)
	sg_swap_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].systime;
    OUTPUT:
	RETVAL

void
colnames(self)
	sg_swap_stats *self;
    PPCODE:
        AV *retval;
	MAKE_AV_FROM_STRINGS(sg_swap_stat_names, retval);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

#define SWAP_STATS_ARRAY_ROW(self, entry, av) \
    do { \
	SV **ary; \
	size_t j; \
	av = newAV(); \
	av_extend(av, lengthof(sg_swap_stat_names)); \
	ary = AvARRAY(av); \
	AvFILLp(av) = lengthof(sg_swap_stat_names)-1; \
	for( j = 0; j < lengthof(sg_swap_stat_names); ++j ) \
	    ary[j] = newSV(0); \
	sv_setuv(ary[0], self[entry].total); \
	sv_setuv(ary[1], self[entry].free); \
	sv_setuv(ary[2], self[entry].used); \
	sv_setiv(ary[3], self[entry].systime); \
    } while(0)

#define SWAP_STATS_HASH_ROW(self, entry, hv) \
    do { \
	hv = newHV(); \
	hv_store(hv, sg_swap_stat_names[0], strlen(sg_swap_stat_names[0]), newSVuv(self[entry].total), 0); \
	hv_store(hv, sg_swap_stat_names[1], strlen(sg_swap_stat_names[1]), newSVuv(self[entry].free), 0); \
	hv_store(hv, sg_swap_stat_names[2], strlen(sg_swap_stat_names[2]), newSVuv(self[entry].used), 0); \
	hv_store(hv, sg_swap_stat_names[3], strlen(sg_swap_stat_names[3]), newSViv(self[entry].systime), 0); \
    } while(0)

void
fetchrow_arrayref(self, num = 0)
	sg_swap_stats *self;
	UV num;
    CODE:
        AV *retval;
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	/* XXX add fill row macro here */
	SWAP_STATS_ARRAY_ROW(self, num, retval);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
fetchall_arrayref(self)
	sg_swap_stats *self;
    CODE:
        AV *retval;
	/* XXX add fill row macro here */
	FETCH_ALL_ROWS(self, SWAP_STATS_ARRAY_ROW, retval, AV);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
fetchrow_hashref(self, num = 0)
	sg_swap_stats *self;
	UV num;
    CODE:
        HV *retval;
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	/* XXX add fill row macro here */
	SWAP_STATS_HASH_ROW(self, num, retval);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
fetchall_hashref(self)
	sg_swap_stats *self;
    CODE:
        AV *retval;
	/* XXX add fill row macro here */
	FETCH_ALL_ROWS(self, SWAP_STATS_HASH_ROW, retval, HV);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
DESTROY (self)
	sg_swap_stats *self;
    CODE:
    {
	sg_free_swap_stats(self);
    }

MODULE = Unix::Statgrab     PACKAGE = Unix::Statgrab::sg_network_io_stats

UV
entries (self)
	sg_network_io_stats *self;
    CODE:
	RETVAL = sg_get_nelements(self);
    OUTPUT:
	RETVAL

char *
interface_name (self, num = 0)
	sg_network_io_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].interface_name;
    OUTPUT:
	RETVAL

LUV
tx (self, num = 0)
	sg_network_io_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].tx;
    OUTPUT:
	RETVAL
    
LUV
rx (self, num = 0)
	sg_network_io_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].rx;
    OUTPUT:
	RETVAL
   
LUV
ipackets (self, num = 0)
	sg_network_io_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].ipackets;
    OUTPUT:
	RETVAL

LUV
opackets (self, num = 0)
	sg_network_io_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].opackets;
    OUTPUT:
	RETVAL

LUV
ierrors (self, num = 0)
	sg_network_io_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].ierrors;
    OUTPUT:
	RETVAL

LUV
oerrors (self, num = 0)
	sg_network_io_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].oerrors;
    OUTPUT:
	RETVAL

LUV
collisions (self, num = 0)
	sg_network_io_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].collisions;
    OUTPUT:
	RETVAL

UV
systime (self, num = 0)
	sg_network_io_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].systime;
    OUTPUT:
	RETVAL

void
colnames(self)
	sg_network_io_stats *self;
    PPCODE:
        AV *retval;
	MAKE_AV_FROM_STRINGS(sg_network_io_stat_names, retval);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

#define NETWORK_IO_STATS_ARRAY_ROW(self, entry, av) \
    do { \
	SV **ary; \
	size_t j; \
	av = newAV(); \
	av_extend(av, lengthof(sg_network_io_stat_names)); \
	ary = AvARRAY(av); \
	AvFILLp(av) = lengthof(sg_network_io_stat_names)-1; \
	for( j = 0; j < lengthof(sg_network_io_stat_names); ++j ) \
	    ary[j] = newSV(0); \
	sv_setpvn_IF(ary[0], self[entry].interface_name, SAFE_STRLEN(self[entry].interface_name)); \
	sv_setuv(ary[1], self[entry].tx); \
	sv_setuv(ary[2], self[entry].rx); \
	sv_setuv(ary[3], self[entry].ipackets); \
	sv_setuv(ary[4], self[entry].opackets); \
	sv_setuv(ary[5], self[entry].ierrors); \
	sv_setuv(ary[6], self[entry].oerrors); \
	sv_setuv(ary[7], self[entry].collisions); \
	sv_setiv(ary[8], self[entry].systime); \
    } while(0)

#define NETWORK_IO_STATS_HASH_ROW(self, entry, hv) \
    do { \
	hv = newHV(); \
	hv_store(hv, sg_network_io_stat_names[0], strlen(sg_network_io_stat_names[0]), newSVpvn(self[entry].interface_name, SAFE_STRLEN(self[entry].interface_name)), 0); \
	hv_store(hv, sg_network_io_stat_names[1], strlen(sg_network_io_stat_names[1]), newSVuv(self[entry].tx), 0); \
	hv_store(hv, sg_network_io_stat_names[2], strlen(sg_network_io_stat_names[2]), newSVuv(self[entry].rx), 0); \
	hv_store(hv, sg_network_io_stat_names[3], strlen(sg_network_io_stat_names[3]), newSVuv(self[entry].ipackets), 0); \
	hv_store(hv, sg_network_io_stat_names[4], strlen(sg_network_io_stat_names[4]), newSVuv(self[entry].opackets), 0); \
	hv_store(hv, sg_network_io_stat_names[5], strlen(sg_network_io_stat_names[5]), newSVuv(self[entry].ierrors), 0); \
	hv_store(hv, sg_network_io_stat_names[6], strlen(sg_network_io_stat_names[6]), newSVuv(self[entry].oerrors), 0); \
	hv_store(hv, sg_network_io_stat_names[7], strlen(sg_network_io_stat_names[7]), newSVuv(self[entry].collisions), 0); \
	hv_store(hv, sg_network_io_stat_names[8], strlen(sg_network_io_stat_names[8]), newSViv(self[entry].systime), 0); \
    } while(0)

void
fetchrow_arrayref(self, num = 0)
	sg_network_io_stats *self;
	UV num;
    CODE:
        AV *retval;
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	/* XXX add fill row macro here */
	NETWORK_IO_STATS_ARRAY_ROW(self, num, retval);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
fetchall_arrayref(self)
	sg_network_io_stats *self;
    CODE:
        AV *retval;
	/* XXX add fill row macro here */
	FETCH_ALL_ROWS(self, NETWORK_IO_STATS_ARRAY_ROW, retval, AV);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
fetchrow_hashref(self, num = 0)
	sg_network_io_stats *self;
	UV num;
    CODE:
        HV *retval;
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	/* XXX add fill row macro here */
	NETWORK_IO_STATS_HASH_ROW(self, num, retval);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
fetchall_hashref(self)
	sg_network_io_stats *self;
    CODE:
        AV *retval;
	/* XXX add fill row macro here */
	FETCH_ALL_ROWS(self, NETWORK_IO_STATS_HASH_ROW, retval, HV);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
DESTROY (self)
	sg_network_io_stats *self;
    CODE:
    {
	sg_free_network_io_stats(self);
    }

void
get_network_io_stats_diff (now, last)
	sg_network_io_stats *now;
	sg_network_io_stats *last;
    CODE:
    {
	sg_network_io_stats *self;

	if ((self = sg_get_network_io_stats_diff_between(now, last, NULL)) == NULL)
	    XSRETURN_UNDEF;

	EXTEND(SP, 1);

	ST(0) = sv_newmortal();
	sv_setref_pv(ST(0), "Unix::Statgrab::sg_network_io_stats", (void*)self);
	XSRETURN(1);
    }

MODULE = Unix::Statgrab     PACKAGE = Unix::Statgrab::sg_network_iface_stats

UV
entries (self)
	sg_network_iface_stats *self;
    CODE:
	RETVAL = sg_get_nelements(self);
    OUTPUT:
	RETVAL

char *
interface_name (self, num = 0)
	sg_network_iface_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].interface_name;
    OUTPUT:
	RETVAL

LUV
speed (self, num = 0)
	sg_network_iface_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].speed;
    OUTPUT:
	RETVAL

LUV
factor (self, num = 0)
	sg_network_iface_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].factor;
    OUTPUT:
	RETVAL

UV
duplex (self, num = 0)
	sg_network_iface_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].duplex;
    OUTPUT:
	RETVAL

UV
up (self, num = 0)
	sg_network_iface_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].up;
    OUTPUT:
	RETVAL

IV
systime (self, num = 0)
	sg_network_iface_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].systime;
    OUTPUT:
	RETVAL

void
colnames(self)
	sg_network_iface_stats *self;
    PPCODE:
        AV *retval;
	MAKE_AV_FROM_STRINGS(sg_network_iface_stat_names, retval);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

#define NETWORK_IFACE_STATS_ARRAY_ROW(self, entry, av) \
    do { \
	SV **ary; \
	size_t j; \
	av = newAV(); \
	av_extend(av, lengthof(sg_network_iface_stat_names)); \
	ary = AvARRAY(av); \
	AvFILLp(av) = lengthof(sg_network_iface_stat_names)-1; \
	for( j = 0; j < lengthof(sg_network_iface_stat_names); ++j ) \
	    ary[j] = newSV(0); \
	sv_setpvn_IF(ary[0], self[entry].interface_name, SAFE_STRLEN(self[entry].interface_name)); \
	sv_setuv(ary[1], self[entry].speed); \
	sv_setuv(ary[2], self[entry].factor); \
	sv_setuv(ary[3], self[entry].duplex); \
	sv_setuv(ary[4], self[entry].up); \
	sv_setiv(ary[5], self[entry].systime); \
    } while(0)

#define NETWORK_IFACE_STATS_HASH_ROW(self, entry, hv) \
    do { \
	hv = newHV(); \
	hv_store(hv, sg_network_iface_stat_names[0], strlen(sg_network_iface_stat_names[0]), newSVpvn(self[entry].interface_name, SAFE_STRLEN(self[entry].interface_name)), 0); \
	hv_store(hv, sg_network_iface_stat_names[1], strlen(sg_network_iface_stat_names[1]), newSVuv(self[entry].speed), 0); \
	hv_store(hv, sg_network_iface_stat_names[2], strlen(sg_network_iface_stat_names[2]), newSVuv(self[entry].factor), 0); \
	hv_store(hv, sg_network_iface_stat_names[3], strlen(sg_network_iface_stat_names[3]), newSVuv(self[entry].duplex), 0); \
	hv_store(hv, sg_network_iface_stat_names[4], strlen(sg_network_iface_stat_names[4]), newSVuv(self[entry].up), 0); \
	hv_store(hv, sg_network_iface_stat_names[5], strlen(sg_network_iface_stat_names[5]), newSViv(self[entry].systime), 0); \
    } while(0)

void
fetchrow_arrayref(self, num = 0)
	sg_network_iface_stats *self;
	UV num;
    CODE:
        AV *retval;
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	/* XXX add fill row macro here */
	NETWORK_IFACE_STATS_ARRAY_ROW(self, num, retval);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
fetchall_arrayref(self)
	sg_network_iface_stats *self;
    CODE:
        AV *retval;
	/* XXX add fill row macro here */
	FETCH_ALL_ROWS(self, NETWORK_IFACE_STATS_ARRAY_ROW, retval, AV);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
fetchrow_hashref(self, num = 0)
	sg_network_iface_stats *self;
	UV num;
    CODE:
        HV *retval;
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	/* XXX add fill row macro here */
	NETWORK_IFACE_STATS_HASH_ROW(self, num, retval);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
fetchall_hashref(self)
	sg_network_iface_stats *self;
    CODE:
        AV *retval;
	/* XXX add fill row macro here */
	FETCH_ALL_ROWS(self, NETWORK_IFACE_STATS_HASH_ROW, retval, HV);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
DESTROY (self)
	sg_network_iface_stats *self;
    CODE:
    {
	sg_free_network_iface_stats(self);
    }

MODULE = Unix::Statgrab	    PACKAGE = Unix::Statgrab::sg_page_stats

UV
entries (self)
	sg_page_stats *self;
    CODE:
	RETVAL = sg_get_nelements(self);
    OUTPUT:
	RETVAL

LUV
pages_pagein (self, num = 0)
	sg_page_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].pages_pagein;
    OUTPUT:
	RETVAL

LUV
pages_pageout (self, num = 0)
	sg_page_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].pages_pageout;
    OUTPUT:
	RETVAL

UV
systime (self, num = 0)
	sg_page_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].systime;
    OUTPUT:
	RETVAL
	
void
colnames(self)
	sg_page_stats *self;
    PPCODE:
        AV *retval;
	MAKE_AV_FROM_STRINGS(sg_page_stat_names, retval);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

#define PAGE_STATS_ARRAY_ROW(self, entry, av) \
    do { \
	SV **ary; \
	size_t j; \
	av = newAV(); \
	av_extend(av, lengthof(sg_page_stat_names)); \
	ary = AvARRAY(av); \
	AvFILLp(av) = lengthof(sg_page_stat_names)-1; \
	for( j = 0; j < lengthof(sg_page_stat_names); ++j ) \
	    ary[j] = newSV(0); \
	sv_setuv(ary[0], self[entry].pages_pagein); \
	sv_setuv(ary[1], self[entry].pages_pageout); \
	sv_setiv(ary[2], self[entry].systime); \
    } while(0)

#define PAGE_STATS_HASH_ROW(self, entry, hv) \
    do { \
	hv = newHV(); \
	hv_store(hv, sg_page_stat_names[0], strlen(sg_page_stat_names[0]), newSVuv(self[entry].pages_pagein), 0); \
	hv_store(hv, sg_page_stat_names[1], strlen(sg_page_stat_names[1]), newSVuv(self[entry].pages_pageout), 0); \
	hv_store(hv, sg_page_stat_names[2], strlen(sg_page_stat_names[2]), newSViv(self[entry].systime), 0); \
    } while(0)

void
fetchrow_arrayref(self, num = 0)
	sg_page_stats *self;
	UV num;
    CODE:
        AV *retval;
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	/* XXX add fill row macro here */
	PAGE_STATS_ARRAY_ROW(self, num, retval);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
fetchall_arrayref(self)
	sg_page_stats *self;
    CODE:
        AV *retval;
	/* XXX add fill row macro here */
	FETCH_ALL_ROWS(self, PAGE_STATS_ARRAY_ROW, retval, AV);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
fetchrow_hashref(self, num = 0)
	sg_page_stats *self;
	UV num;
    CODE:
        HV *retval;
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	/* XXX add fill row macro here */
	PAGE_STATS_HASH_ROW(self, num, retval);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
fetchall_hashref(self)
	sg_page_stats *self;
    CODE:
        AV *retval;
	/* XXX add fill row macro here */
	FETCH_ALL_ROWS(self, PAGE_STATS_HASH_ROW, retval, HV);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
DESTROY (self)
	sg_page_stats *self;
    CODE:
    {
	sg_free_page_stats(self);
    }

void
get_page_stats_diff (now, last)
	sg_page_stats *now;
	sg_page_stats *last;
    CODE:
    {
	sg_page_stats *self;
	if ((self = sg_get_page_stats_diff_between(now, last, NULL)) == NULL)
	    XSRETURN_UNDEF;

	EXTEND(SP, 1);

	ST(0) = sv_newmortal();
	sv_setref_pv(ST(0), "Unix::Statgrab::sg_page_stats", (void*)self);
	XSRETURN(1);
    }

MODULE = Unix::Statgrab     PACKAGE = Unix::Statgrab::sg_user_stats

UV
entries (self)
	sg_user_stats *self;
    CODE:
	RETVAL = sg_get_nelements(self);
    OUTPUT:
	RETVAL

char *
login_name (self, num = 0)
	sg_user_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].login_name;
    OUTPUT:
	RETVAL

void
record_id (self, num = 0)
	sg_user_stats *self;
	UV num;
    CODE:
    { 
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;

	EXTEND(SP, 1);

	ST(0) = newSVpvn_flags(self[num].record_id, self[num].record_id_size, SVs_TEMP);
	XSRETURN(1);
    }

char *
device (self, num = 0)
	sg_user_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].device;
    OUTPUT:
	RETVAL

char *
hostname (self, num = 0)
	sg_user_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].hostname;
    OUTPUT:
	RETVAL

IV
pid (self, num = 0)
	sg_user_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].pid;
    OUTPUT:
	RETVAL

IV
login_time (self, num = 0)
	sg_user_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].login_time;
    OUTPUT:
	RETVAL

IV
systime (self, num = 0)
	sg_user_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].systime;
    OUTPUT:
	RETVAL

void
colnames(self)
	sg_user_stats *self;
    PPCODE:
        AV *retval;
	MAKE_AV_FROM_STRINGS(sg_user_stat_names, retval);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

#define USER_STATS_ARRAY_ROW(self, entry, av) \
    do { \
	SV **ary; \
	size_t j; \
	av = newAV(); \
	av_extend(av, lengthof(sg_user_stat_names)); \
	ary = AvARRAY(av); \
	AvFILLp(av) = lengthof(sg_user_stat_names)-1; \
	for( j = 0; j < lengthof(sg_user_stat_names); ++j ) \
	    ary[j] = newSV(0); \
	sv_setpvn_IF(ary[0], self[entry].login_name, SAFE_STRLEN(self[entry].login_name)); \
	sv_setpvn_IF(ary[1], self[entry].record_id, self[entry].record_id_size); \
	sv_setpvn_IF(ary[2], self[entry].device, SAFE_STRLEN(self[entry].device)); \
	sv_setpvn_IF(ary[3], self[entry].hostname, SAFE_STRLEN(self[entry].hostname)); \
	sv_setiv(ary[4], self[entry].pid); \
	sv_setiv(ary[5], self[entry].login_time); \
	sv_setiv(ary[6], self[entry].systime); \
    } while(0)

#define USER_STATS_HASH_ROW(self, entry, hv) \
    do { \
	hv = newHV(); \
	hv_store(hv, sg_user_stat_names[0], strlen(sg_user_stat_names[0]), newSVpvn(self[entry].login_name, SAFE_STRLEN(self[entry].login_name)), 0); \
	hv_store(hv, sg_user_stat_names[1], strlen(sg_user_stat_names[1]), newSVpvn(self[entry].record_id, self[entry].record_id_size), 0); \
	hv_store(hv, sg_user_stat_names[2], strlen(sg_user_stat_names[2]), newSVpvn(self[entry].device, SAFE_STRLEN(self[entry].device)), 0); \
	hv_store(hv, sg_user_stat_names[3], strlen(sg_user_stat_names[3]), newSVpvn(self[entry].hostname, SAFE_STRLEN(self[entry].hostname)), 0); \
	hv_store(hv, sg_user_stat_names[4], strlen(sg_user_stat_names[4]), newSViv(self[entry].pid), 0); \
	hv_store(hv, sg_user_stat_names[5], strlen(sg_user_stat_names[5]), newSViv(self[entry].login_time), 0); \
	hv_store(hv, sg_user_stat_names[6], strlen(sg_user_stat_names[6]), newSViv(self[entry].systime), 0); \
    } while(0)

void
fetchrow_arrayref(self, num = 0)
	sg_user_stats *self;
	UV num;
    CODE:
        AV *retval;
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	/* XXX add fill row macro here */
	USER_STATS_ARRAY_ROW(self, num, retval);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
fetchall_arrayref(self)
	sg_user_stats *self;
    CODE:
        AV *retval;
	/* XXX add fill row macro here */
	FETCH_ALL_ROWS(self, USER_STATS_ARRAY_ROW, retval, AV);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
fetchrow_hashref(self, num = 0)
	sg_user_stats *self;
	UV num;
    CODE:
        HV *retval;
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	/* XXX add fill row macro here */
	USER_STATS_HASH_ROW(self, num, retval);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
fetchall_hashref(self)
	sg_user_stats *self;
    CODE:
        AV *retval;
	/* XXX add fill row macro here */
	FETCH_ALL_ROWS(self, USER_STATS_HASH_ROW, retval, HV);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
DESTROY (self)
	sg_user_stats *self;
    CODE:
    {
	sg_free_user_stats(self);
    }

MODULE = Unix::Statgrab	    PACKAGE = Unix::Statgrab::sg_process_stats

UV
entries (self)
	sg_process_stats *self;
    CODE:
	RETVAL = sg_get_nelements(self);
    OUTPUT:
	RETVAL
	
char *
process_name (self, num = 0)
	sg_process_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].process_name;
    OUTPUT:
	RETVAL

char *
proctitle (self, num = 0)
	sg_process_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].proctitle;
    OUTPUT:
	RETVAL

IV
pid (self, num = 0)
	sg_process_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].pid;
    OUTPUT:
	RETVAL

IV
parent (self, num = 0)
	sg_process_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].parent;
    OUTPUT:
	RETVAL

IV
pgid (self, num = 0)
	sg_process_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].pgid;
    OUTPUT:
	RETVAL

IV
sessid (self, num = 0)
	sg_process_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].sessid;
    OUTPUT:
	RETVAL

IV
uid (self, num = 0)
	sg_process_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].uid;
    OUTPUT:
	RETVAL

IV 
euid (self, num = 0)
	sg_process_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].euid;
    OUTPUT:
	RETVAL

IV
gid (self, num = 0)
	sg_process_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].gid;
    OUTPUT:
	RETVAL

IV
egid (self, num = 0)
	sg_process_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].egid;
    OUTPUT:
	RETVAL

LUV
context_switches (self, num = 0)
	sg_process_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].context_switches;
    OUTPUT:
	RETVAL

LUV
voluntary_context_switches (self, num = 0)
	sg_process_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].voluntary_context_switches;
    OUTPUT:
	RETVAL

LUV
involuntary_context_switches (self, num = 0)
	sg_process_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].involuntary_context_switches;
    OUTPUT:
	RETVAL

LUV
proc_size (self, num = 0)
	sg_process_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].proc_size;
    OUTPUT:
	RETVAL

LUV
proc_resident (self, num = 0)
	sg_process_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].proc_resident;
    OUTPUT:
	RETVAL

IV
start_time (self, num = 0)
	sg_process_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].start_time;
    OUTPUT:
	RETVAL

IV
time_spent (self, num = 0)
	sg_process_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].time_spent;
    OUTPUT:
	RETVAL

LUV
cpu_percent (self, num = 0)
	sg_process_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].cpu_percent;
    OUTPUT:
	RETVAL

IV
nice (self, num = 0)
	sg_process_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].nice;
    OUTPUT:
	RETVAL

UV
state (self, num = 0)
	sg_process_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].state;
    OUTPUT:
	RETVAL

IV
systime (self, num = 0)
	sg_process_stats *self;
	UV num;
    CODE:
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	RETVAL = self[num].systime;
    OUTPUT:
	RETVAL

void
colnames(self)
	sg_process_stats *self;
    PPCODE:
        AV *retval;
	MAKE_AV_FROM_STRINGS(sg_process_stat_names, retval);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

#define PROCESS_STATS_ARRAY_ROW(self, entry, av) \
    do { \
	SV **ary; \
	size_t j; \
	av = newAV(); \
	av_extend(av, lengthof(sg_process_stat_names)); \
	ary = AvARRAY(av); \
	AvFILLp(av) = lengthof(sg_process_stat_names)-1; \
	for( j = 0; j < lengthof(sg_process_stat_names); ++j ) \
	    ary[j] = newSV(0); \
	sv_setpvn_IF(ary[0], self[entry].process_name, SAFE_STRLEN(self[entry].process_name)); \
	sv_setpvn_IF(ary[1], self[entry].proctitle, SAFE_STRLEN(self[entry].proctitle)); \
	sv_setiv(ary[2], self[entry].pid); \
	sv_setiv(ary[3], self[entry].parent); \
	sv_setiv(ary[4], self[entry].pgid); \
	sv_setiv(ary[5], self[entry].sessid); \
	sv_setiv(ary[6], self[entry].uid); \
	sv_setiv(ary[7], self[entry].euid); \
	sv_setiv(ary[8], self[entry].gid); \
	sv_setiv(ary[9], self[entry].egid); \
	sv_setuv(ary[10], self[entry].context_switches); \
	sv_setuv(ary[11], self[entry].voluntary_context_switches); \
	sv_setuv(ary[12], self[entry].involuntary_context_switches); \
	sv_setuv(ary[13], self[entry].proc_size); \
	sv_setuv(ary[14], self[entry].proc_resident); \
	sv_setiv(ary[15], self[entry].start_time); \
	sv_setiv(ary[16], self[entry].time_spent); \
	sv_setnv(ary[17], self[entry].cpu_percent); \
	sv_setiv(ary[18], self[entry].nice); \
	sv_setuv(ary[19], self[entry].state); \
	sv_setiv(ary[20], self[entry].systime); \
    } while(0)

#define PROCESS_STATS_HASH_ROW(self, entry, hv) \
    do { \
	hv = newHV(); \
	hv_store(hv, sg_process_stat_names[0], strlen(sg_process_stat_names[0]), newSVpvn(self[entry].process_name, SAFE_STRLEN(self[entry].process_name)), 0); \
	hv_store(hv, sg_process_stat_names[1], strlen(sg_process_stat_names[1]), newSVpvn(self[entry].proctitle, SAFE_STRLEN(self[entry].proctitle)), 0); \
	hv_store(hv, sg_process_stat_names[2], strlen(sg_process_stat_names[2]), newSViv(self[entry].pid), 0); \
	hv_store(hv, sg_process_stat_names[3], strlen(sg_process_stat_names[3]), newSViv(self[entry].parent), 0); \
	hv_store(hv, sg_process_stat_names[4], strlen(sg_process_stat_names[4]), newSViv(self[entry].pgid), 0); \
	hv_store(hv, sg_process_stat_names[5], strlen(sg_process_stat_names[5]), newSViv(self[entry].sessid), 0); \
	hv_store(hv, sg_process_stat_names[6], strlen(sg_process_stat_names[6]), newSViv(self[entry].uid), 0); \
	hv_store(hv, sg_process_stat_names[7], strlen(sg_process_stat_names[7]), newSViv(self[entry].euid), 0); \
	hv_store(hv, sg_process_stat_names[8], strlen(sg_process_stat_names[8]), newSViv(self[entry].gid), 0); \
	hv_store(hv, sg_process_stat_names[9], strlen(sg_process_stat_names[9]), newSViv(self[entry].egid), 0); \
	hv_store(hv, sg_process_stat_names[10], strlen(sg_process_stat_names[10]), newSVuv(self[entry].context_switches), 0); \
	hv_store(hv, sg_process_stat_names[11], strlen(sg_process_stat_names[11]), newSVuv(self[entry].voluntary_context_switches), 0); \
	hv_store(hv, sg_process_stat_names[12], strlen(sg_process_stat_names[12]), newSVuv(self[entry].involuntary_context_switches), 0); \
	hv_store(hv, sg_process_stat_names[13], strlen(sg_process_stat_names[13]), newSVuv(self[entry].proc_size), 0); \
	hv_store(hv, sg_process_stat_names[14], strlen(sg_process_stat_names[14]), newSVuv(self[entry].proc_resident), 0); \
	hv_store(hv, sg_process_stat_names[15], strlen(sg_process_stat_names[15]), newSViv(self[entry].start_time), 0); \
	hv_store(hv, sg_process_stat_names[16], strlen(sg_process_stat_names[16]), newSViv(self[entry].time_spent), 0); \
	hv_store(hv, sg_process_stat_names[17], strlen(sg_process_stat_names[17]), newSVnv(self[entry].cpu_percent), 0); \
	hv_store(hv, sg_process_stat_names[18], strlen(sg_process_stat_names[18]), newSViv(self[entry].nice), 0); \
	hv_store(hv, sg_process_stat_names[19], strlen(sg_process_stat_names[19]), newSVuv(self[entry].state), 0); \
	hv_store(hv, sg_process_stat_names[20], strlen(sg_process_stat_names[20]), newSViv(self[entry].systime), 0); \
    } while(0)

void
fetchrow_arrayref(self, num = 0)
	sg_process_stats *self;
	UV num;
    CODE:
        AV *retval;
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	/* XXX add fill row macro here */
	PROCESS_STATS_ARRAY_ROW(self, num, retval);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
fetchall_arrayref(self)
	sg_process_stats *self;
    CODE:
        AV *retval;
	/* XXX add fill row macro here */
	FETCH_ALL_ROWS(self, PROCESS_STATS_ARRAY_ROW, retval, AV);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
fetchrow_hashref(self, num = 0)
	sg_process_stats *self;
	UV num;
    CODE:
        HV *retval;
	if (num >= sg_get_nelements(self))
	    XSRETURN_UNDEF;
	/* XXX add fill row macro here */
	PROCESS_STATS_HASH_ROW(self, num, retval);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
fetchall_hashref(self)
	sg_process_stats *self;
    CODE:
        AV *retval;
	/* XXX add fill row macro here */
	FETCH_ALL_ROWS(self, PROCESS_STATS_HASH_ROW, retval, HV);
	ST(0) = sv_2mortal (newRV_noinc ((SV *)retval));
	XSRETURN(1);

void
DESTROY (self)
	sg_process_stats *self;
    CODE:
    {
	sg_free_process_stats(self);
    }
