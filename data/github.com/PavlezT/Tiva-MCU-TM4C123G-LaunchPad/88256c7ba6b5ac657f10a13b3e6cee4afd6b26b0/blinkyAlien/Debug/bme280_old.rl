L 1 "../bme280_old.c"
N/**\mainpage
N * Copyright (C) 2016 - 2017 Bosch Sensortec GmbH
N *
N * Redistribution and use in source and binary forms, with or without
N * modification, are permitted provided that the following conditions are met:
N *
N * Redistributions of source code must retain the above copyright
N * notice, this list of conditions and the following disclaimer.
N *
N * Redistributions in binary form must reproduce the above copyright
N * notice, this list of conditions and the following disclaimer in the
N * documentation and/or other materials provided with the distribution.
N *
N * Neither the name of the copyright holder nor the names of the
N * contributors may be used to endorse or promote products derived from
N * this software without specific prior written permission.
N *
N * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
N * CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
N * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
N * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
N * DISCLAIMED. IN NO EVENT SHALL COPYRIGHT HOLDER
N * OR CONTRIBUTORS BE LIABLE FOR ANY
N * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
N * OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO,
N * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
N * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
N * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
N * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
N * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
N * ANY WAY OUT OF THE USE OF THIS
N * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE
N *
N * The information provided is believed to be accurate and reliable.
N * The copyright holder assumes no responsibility
N * for the consequences of use
N * of such information nor for any infringement of patents or
N * other rights of third parties which may result from its use.
N * No license is granted by implication or otherwise under any patent or
N * patent rights of the copyright holder.
N *
N * File		bme280.c
N * Date		14 Feb 2018
N * Version	3.3.4
N *
N */
N
N/*! @file bme280.c
N    @brief Sensor driver for BME280 sensor */
N#include "bme280_old.h"
L 1 "..\bme280_old.h" 1
N/**
N * Copyright (C) 2016 - 2017 Bosch Sensortec GmbH
N *
N * Redistribution and use in source and binary forms, with or without
N * modification, are permitted provided that the following conditions are met:
N *
N * Redistributions of source code must retain the above copyright
N * notice, this list of conditions and the following disclaimer.
N *
N * Redistributions in binary form must reproduce the above copyright
N * notice, this list of conditions and the following disclaimer in the
N * documentation and/or other materials provided with the distribution.
N *
N * Neither the name of the copyright holder nor the names of the
N * contributors may be used to endorse or promote products derived from
N * this software without specific prior written permission.
N *
N * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
N * CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
N * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
N * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
N * DISCLAIMED. IN NO EVENT SHALL COPYRIGHT HOLDER
N * OR CONTRIBUTORS BE LIABLE FOR ANY
N * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
N * OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO,
N * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
N * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
N * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
N * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
N * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
N * ANY WAY OUT OF THE USE OF THIS
N * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE
N *
N * The information provided is believed to be accurate and reliable.
N * The copyright holder assumes no responsibility
N * for the consequences of use
N * of such information nor for any infringement of patents or
N * other rights of third parties which may result from its use.
N * No license is granted by implication or otherwise under any patent or
N * patent rights of the copyright holder.
N *
N * @file	bme280.h
N * @date	14 Feb 2018
N * @version	3.3.4
N * @brief
N *
N */
N/*! @file bme280.h
N    @brief Sensor driver for BME280 sensor */
N/*!
N * @defgroup BME280 SENSOR API
N * @{*/
N#ifndef BME280_H_
N#define BME280_H_
N
N/*! CPP guard */
N#ifdef __cplusplus
Sextern "C" {
N#endif
N
N/* Header includes */
N#include "bme280_defs.h"
L 1 "..\bme280_defs.h" 1
N/**
N * Copyright (C) 2016 - 2017 Bosch Sensortec GmbH
N *
N * Redistribution and use in source and binary forms, with or without
N * modification, are permitted provided that the following conditions are met:
N *
N * Redistributions of source code must retain the above copyright
N * notice, this list of conditions and the following disclaimer.
N *
N * Redistributions in binary form must reproduce the above copyright
N * notice, this list of conditions and the following disclaimer in the
N * documentation and/or other materials provided with the distribution.
N *
N * Neither the name of the copyright holder nor the names of the
N * contributors may be used to endorse or promote products derived from
N * this software without specific prior written permission.
N *
N * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
N * CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
N * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
N * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
N * DISCLAIMED. IN NO EVENT SHALL COPYRIGHT HOLDER
N * OR CONTRIBUTORS BE LIABLE FOR ANY
N * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
N * OR CONSEQUENTIAL DAMAGES(INCLUDING, BUT NOT LIMITED TO,
N * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
N * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
N * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
N * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
N * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
N * ANY WAY OUT OF THE USE OF THIS
N * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE
N *
N * The information provided is believed to be accurate and reliable.
N * The copyright holder assumes no responsibility
N * for the consequences of use
N * of such information nor for any infringement of patents or
N * other rights of third parties which may result from its use.
N * No license is granted by implication or otherwise under any patent or
N * patent rights of the copyright holder.
N *
N * @file	bme280_defs.h
N * @date	14 Feb 2018
N * @version	3.3.4
N * @brief
N *
N */
N
N/*! @file bme280_defs.h
N    @brief Sensor driver for BME280 sensor */
N/*!
N * @defgroup BME280 SENSOR API
N * @brief
N * @{*/
N#ifndef BME280_DEFS_H_
N#define BME280_DEFS_H_
N
N/********************************************************/
N/* header includes */
N#ifdef __KERNEL__
S#include <linux/types.h>
S#include <linux/kernel.h>
N#else
N#include <stdint.h>
L 1 "C:/ti/ccsv8/tools/compiler/ti-cgt-arm_5.2.6/include/stdint.h" 1
N/*****************************************************************************/
N/* STDINT.H v5.2.6                                                           */
N/*                                                                           */
N/* Copyright (c) 2002-2015 Texas Instruments Incorporated                    */
N/* http://www.ti.com/                                                        */
N/*                                                                           */
N/*  Redistribution and  use in source  and binary forms, with  or without    */
N/*  modification,  are permitted provided  that the  following conditions    */
N/*  are met:                                                                 */
N/*                                                                           */
N/*     Redistributions  of source  code must  retain the  above copyright    */
N/*     notice, this list of conditions and the following disclaimer.         */
N/*                                                                           */
N/*     Redistributions in binary form  must reproduce the above copyright    */
N/*     notice, this  list of conditions  and the following  disclaimer in    */
N/*     the  documentation  and/or   other  materials  provided  with  the    */
N/*     distribution.                                                         */
N/*                                                                           */
N/*     Neither the  name of Texas Instruments Incorporated  nor the names    */
N/*     of its  contributors may  be used to  endorse or  promote products    */
N/*     derived  from   this  software  without   specific  prior  written    */
N/*     permission.                                                           */
N/*                                                                           */
N/*  THIS SOFTWARE  IS PROVIDED BY THE COPYRIGHT  HOLDERS AND CONTRIBUTORS    */
N/*  "AS IS"  AND ANY  EXPRESS OR IMPLIED  WARRANTIES, INCLUDING,  BUT NOT    */
N/*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR    */
N/*  A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT    */
N/*  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,    */
N/*  SPECIAL,  EXEMPLARY,  OR CONSEQUENTIAL  DAMAGES  (INCLUDING, BUT  NOT    */
N/*  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,    */
N/*  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY    */
N/*  THEORY OF  LIABILITY, WHETHER IN CONTRACT, STRICT  LIABILITY, OR TORT    */
N/*  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE    */
N/*  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.     */
N/*                                                                           */
N/*****************************************************************************/
N#ifndef _STDINT_H_
N#define _STDINT_H_
N
N/* 7.18.1.1 Exact-width integer types */
N
N    typedef   signed char   int8_t;
N    typedef unsigned char  uint8_t;
N    typedef          short  int16_t;
N    typedef unsigned short uint16_t;
N    typedef          int    int32_t;
N    typedef unsigned int   uint32_t;
N
N
N    typedef          long long  int64_t;
N    typedef unsigned long long uint64_t;
N
N/* 7.18.1.2 Minimum-width integer types */
N
N    typedef  int8_t   int_least8_t;
N    typedef uint8_t  uint_least8_t;
N
N    typedef  int16_t  int_least16_t;
N    typedef uint16_t uint_least16_t;
N    typedef  int32_t  int_least32_t;
N    typedef uint32_t uint_least32_t;
N
N
N    typedef  int64_t  int_least64_t;
N    typedef uint64_t uint_least64_t;
N
N/* 7.18.1.3 Fastest minimum-width integer types */
N
N    typedef  int32_t  int_fast8_t;
N    typedef uint32_t uint_fast8_t;
N    typedef  int32_t  int_fast16_t;
N    typedef uint32_t uint_fast16_t;
N
N    typedef  int32_t  int_fast32_t;
N    typedef uint32_t uint_fast32_t;
N
N
N    typedef  int64_t  int_fast64_t;
N    typedef uint64_t uint_fast64_t;
N
N/* 7.18.1.4 Integer types capable of holding object pointers */
N    typedef          int intptr_t;
N    typedef unsigned int uintptr_t;
N
N/* 7.18.1.5 Greatest-width integer types */
N    typedef          long long intmax_t;
N    typedef unsigned long long uintmax_t;
N
N/* 
N   According to footnotes in the 1999 C standard, "C++ implementations
N   should define these macros only when __STDC_LIMIT_MACROS is defined
N   before <stdint.h> is included." 
N*/
N#if !defined(__cplusplus) || defined(__STDC_LIMIT_MACROS)
X#if !0L || 0L
N
N/* 7.18.2 Limits of specified width integer types */
N
N    #define  INT8_MAX   0x7f
N    #define  INT8_MIN   (-INT8_MAX-1)
N    #define UINT8_MAX   0xff
N
N    #define  INT16_MAX  0x7fff
N    #define  INT16_MIN  (-INT16_MAX-1)
N    #define UINT16_MAX  0xffff
N
N    #define  INT32_MAX  0x7fffffff
N    #define  INT32_MIN  (-INT32_MAX-1)
N    #define UINT32_MAX  0xffffffff
N
N
N    #define  INT64_MAX  0x7fffffffffffffff
N    #define  INT64_MIN  (-INT64_MAX-1)
N    #define UINT64_MAX  0xffffffffffffffff
N
N    #define  INT_LEAST8_MAX   (INT8_MAX)
N    #define  INT_LEAST8_MIN   (INT8_MIN)
N    #define UINT_LEAST8_MAX   (UINT8_MAX)
N
N    #define  INT_LEAST16_MAX  (INT16_MAX)
N    #define  INT_LEAST16_MIN  (INT16_MIN)
N    #define UINT_LEAST16_MAX  (UINT16_MAX)
N    #define  INT_LEAST32_MAX  (INT32_MAX)
N    #define  INT_LEAST32_MIN  (INT32_MIN)
N    #define UINT_LEAST32_MAX  (UINT32_MAX)
N
N
N    #define  INT_LEAST64_MAX  (INT64_MAX)
N    #define  INT_LEAST64_MIN  (INT64_MIN)
N    #define UINT_LEAST64_MAX  (UINT64_MAX)
N
N    #define  INT_FAST8_MAX   (INT32_MAX)
N    #define  INT_FAST8_MIN   (INT32_MIN)
N    #define UINT_FAST8_MAX   (UINT32_MAX)
N    #define  INT_FAST16_MAX  (INT32_MAX)
N    #define  INT_FAST16_MIN  (INT32_MIN)
N    #define UINT_FAST16_MAX  (UINT32_MAX)
N
N    #define  INT_FAST32_MAX  (INT32_MAX)
N    #define  INT_FAST32_MIN  (INT32_MIN)
N    #define UINT_FAST32_MAX  (UINT32_MAX)
N
N
N    #define  INT_FAST64_MAX  (INT64_MAX)
N    #define  INT_FAST64_MIN  (INT64_MIN)
N    #define UINT_FAST64_MAX  (UINT64_MAX)
N
N    #define INTPTR_MAX   (INT32_MAX)
N    #define INTPTR_MIN   (INT32_MIN)
N    #define UINTPTR_MAX  (UINT32_MAX)
N
N    #define INTMAX_MIN   (INT64_MIN)
N    #define INTMAX_MAX   (INT64_MAX)
N    #define UINTMAX_MAX  (UINT64_MAX)
N
N/* 7.18.3 Limits of other integer types */
N
N    #define PTRDIFF_MAX (INT32_MAX)
N    #define PTRDIFF_MIN (INT32_MIN)
N
N    #define SIG_ATOMIC_MIN (INT32_MIN)
N    #define SIG_ATOMIC_MAX (INT32_MAX)
N
N    #define SIZE_MAX (UINT32_MAX)
N
N#ifndef WCHAR_MAX
N#if !defined(__TI_WCHAR_T_BITS__) || __TI_WCHAR_T_BITS__ == 16
X#if !1L || 16 == 16
N#define WCHAR_MAX 0xffffu
N#else 
S#define WCHAR_MAX 0xffffffffu
N#endif
N#endif
N
N#ifndef WCHAR_MIN
N#define WCHAR_MIN 0
N#endif
N
N    #define WINT_MIN (INT32_MIN)
N    #define WINT_MAX (INT32_MAX)
N
N/* 7.18.4.1 Macros for minimum-width integer constants */
N
N/*
N   There is a defect report filed against the C99 standard concerning how 
N   the (U)INTN_C macros should be implemented.  Please refer to --
N   http://wwwold.dkuug.dk/JTC1/SC22/WG14/www/docs/dr_209.htm 
N   for more information.  These macros are implemented according to the
N   suggestion given at this web site.
N*/
N
N    #define  INT8_C(value)  ((int_least8_t)(value))
N    #define UINT8_C(value)  ((uint_least8_t)(value))
N    #define  INT16_C(value) ((int_least16_t)(value))
N    #define UINT16_C(value) ((uint_least16_t)(value))
N    #define  INT32_C(value) ((int_least32_t)(value))
N    #define UINT32_C(value) ((uint_least32_t)(value))
N
N
N    #define  INT64_C(value) ((int_least64_t)(value))
N    #define UINT64_C(value) ((uint_least64_t)(value))
N
N/* 7.18.4.2 Macros for greatest-width integer constants */
N
N    #define  INTMAX_C(value) ((intmax_t)(value))
N    #define UINTMAX_C(value) ((uintmax_t)(value))
N
N#endif /* !defined(__cplusplus) || defined(__STDC_LIMIT_MACROS) */
N
N#endif /* _STDINT_H_ */
L 65 "..\bme280_defs.h" 2
N#include <stddef.h>
L 1 "C:/ti/ccsv8/tools/compiler/ti-cgt-arm_5.2.6/include/stddef.h" 1
N/*****************************************************************************/
N/* stddef.h   v5.2.6                                                         */
N/*                                                                           */
N/* Copyright (c) 1993-2015 Texas Instruments Incorporated                    */
N/* http://www.ti.com/                                                        */
N/*                                                                           */
N/*  Redistribution and  use in source  and binary forms, with  or without    */
N/*  modification,  are permitted provided  that the  following conditions    */
N/*  are met:                                                                 */
N/*                                                                           */
N/*     Redistributions  of source  code must  retain the  above copyright    */
N/*     notice, this list of conditions and the following disclaimer.         */
N/*                                                                           */
N/*     Redistributions in binary form  must reproduce the above copyright    */
N/*     notice, this  list of conditions  and the following  disclaimer in    */
N/*     the  documentation  and/or   other  materials  provided  with  the    */
N/*     distribution.                                                         */
N/*                                                                           */
N/*     Neither the  name of Texas Instruments Incorporated  nor the names    */
N/*     of its  contributors may  be used to  endorse or  promote products    */
N/*     derived  from   this  software  without   specific  prior  written    */
N/*     permission.                                                           */
N/*                                                                           */
N/*  THIS SOFTWARE  IS PROVIDED BY THE COPYRIGHT  HOLDERS AND CONTRIBUTORS    */
N/*  "AS IS"  AND ANY  EXPRESS OR IMPLIED  WARRANTIES, INCLUDING,  BUT NOT    */
N/*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR    */
N/*  A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT    */
N/*  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,    */
N/*  SPECIAL,  EXEMPLARY,  OR CONSEQUENTIAL  DAMAGES  (INCLUDING, BUT  NOT    */
N/*  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,    */
N/*  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY    */
N/*  THEORY OF  LIABILITY, WHETHER IN CONTRACT, STRICT  LIABILITY, OR TORT    */
N/*  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE    */
N/*  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.     */
N/*                                                                           */
N/*****************************************************************************/
N
N#ifndef _STDDEF
N#define _STDDEF
N
N#pragma diag_push
N#pragma CHECK_MISRA("-19.7") /* macros required for implementation */
N#pragma CHECK_MISRA("-20.1") /* standard headers must define standard names */
N#pragma CHECK_MISRA("-20.2") /* standard headers must define standard names */
N
N#ifdef __cplusplus
Sextern "C" namespace std {
N#endif
N
N#ifndef NULL
N#define NULL 0
N#endif
N
Ntypedef __PTRDIFF_T_TYPE__ ptrdiff_t;
Xtypedef int ptrdiff_t;
N
N#ifndef _SIZE_T
N#define _SIZE_T
Ntypedef __SIZE_T_TYPE__ size_t;
Xtypedef unsigned size_t;
N#endif
N
N#ifndef __cplusplus
N#ifndef _WCHAR_T
N#define _WCHAR_T
N
Ntypedef __WCHAR_T_TYPE__ wchar_t;
Xtypedef unsigned short wchar_t;
N
N#endif /* _WCHAR_T */
N#endif /* ! __cplusplus */
N
N#pragma diag_push
N#pragma CHECK_MISRA("-19.10") /* need types as macro arguments */
N
N#ifdef __TI_LLVM__
S#  define offsetof(_type, _ident) __builtin_offsetof(_type, _ident)
N#else
N#    ifdef __cplusplus
S#      define offsetof(_type, _ident) \
S         ((std::size_t)__intaddr__(&(((_type *)0)->_ident)))
X#      define offsetof(_type, _ident)          ((std::size_t)__intaddr__(&(((_type *)0)->_ident)))
N#    else
N#      define offsetof(_type, _ident) \
N         ((size_t)__intaddr__(&(((_type *)0)->_ident)))
X#      define offsetof(_type, _ident)          ((size_t)__intaddr__(&(((_type *)0)->_ident)))
N#    endif
N#endif
N
N#ifdef __cplusplus
S} /* extern "C" namespace std */
N#endif  /* __cplusplus */
N
N#pragma diag_pop
N
N#endif  /* _STDDEF */
N
N#if defined(__cplusplus) && !defined(_CPP_STYLE_HEADER)
X#if 0L && !0L
Susing std::ptrdiff_t;
Susing std::size_t;
S/*using std::wchar_t;*/
N#endif /* _CPP_STYLE_HEADER */
N
N#pragma diag_push
N#pragma CHECK_MISRA("-19.15") /* code outside guard; see below comment */
N
N/*-----------------------------------------------------------------------*/
N/* Define _win_t, for compiling GCC libraries with the TI compiler.      */
N/* GCC's library (newlib) expects wint_t to be defined here, in stddef.h,*/
N/* which is arguably incorrect, but we go along for compatibility.       */
N/* This is outside the _STDDEF guard in case this file has already       */
N/* been included without __need_wint_t.                                  */
N/*-----------------------------------------------------------------------*/
N#if defined(__need_wint_t) && !defined(_WINT_T) 
X#if 0L && !0L 
S#if (__STDC_VERSION__ >= 199901L || !__TI_STRICT_ANSI_MODE__)
S
S#undef __need_wint_t
S#define _WINT_T
S#ifdef __cplusplus
S   extern "C" namespace std {
S#endif /* __cplusplus */
S
Stypedef unsigned int wint_t;
S
S#ifdef __cplusplus
S   } /* extern "C" namespace std { */
S#endif /* __cplusplus */
S
S#if defined(__cplusplus) && !defined(_CPP_STYLE_HEADER)
Susing std::wint_t;
S#endif /* _CPP_STYLE_HEADER */
S
S#endif /* __STDC_VERSION__ >= 199901L  */
N#endif
N
N#pragma diag_pop
L 66 "..\bme280_defs.h" 2
N#endif
N
N/********************************************************/
N/*! @name		Common macros		        */
N/********************************************************/
N
N#if !defined(UINT8_C) && !defined(INT8_C)
X#if !1L && !1L
S#define INT8_C(x)       S8_C(x)
S#define UINT8_C(x)      U8_C(x)
N#endif
N
N#if !defined(UINT16_C) && !defined(INT16_C)
X#if !1L && !1L
S#define INT16_C(x)      S16_C(x)
S#define UINT16_C(x)     U16_C(x)
N#endif
N
N#if !defined(INT32_C) && !defined(UINT32_C)
X#if !1L && !1L
S#define INT32_C(x)      S32_C(x)
S#define UINT32_C(x)     U32_C(x)
N#endif
N
N#if !defined(INT64_C) && !defined(UINT64_C)
X#if !1L && !1L
S#define INT64_C(x)      S64_C(x)
S#define UINT64_C(x)     U64_C(x)
N#endif
N
N/**@}*/
N
N/**\name C standard macros */
N#ifndef NULL
S#ifdef __cplusplus
S#define NULL   0
S#else
S#define NULL   ((void *) 0)
S#endif
N#endif
N/********************************************************/
N
N#ifndef BME280_FLOAT_ENABLE
N/* #define BME280_FLOAT_ENABLE */
N#endif
N
N#ifndef BME280_FLOAT_ENABLE
N#ifndef BME280_64BIT_ENABLE
N#define BME280_64BIT_ENABLE
N#endif
N#endif
N
N#ifndef TRUE
N#define TRUE                UINT8_C(1)
N#endif
N#ifndef FALSE
N#define FALSE               UINT8_C(0)
N#endif
N
N/**\name I2C addresses */
N#define BME280_I2C_ADDR_PRIM	UINT8_C(0x76)
N#define BME280_I2C_ADDR_SEC		UINT8_C(0x77)
N
N/**\name BME280 chip identifier */
N#define BME280_CHIP_ID  UINT8_C(0x60)
N
N/**\name Register Address */
N#define BME280_CHIP_ID_ADDR					UINT8_C(0xD0)
N#define BME280_RESET_ADDR					UINT8_C(0xE0)
N#define BME280_TEMP_PRESS_CALIB_DATA_ADDR	UINT8_C(0xFA)//0x88)
N#define BME280_HUMIDITY_CALIB_DATA_ADDR		UINT8_C(0xFD)//0xE1)
N#define BME280_PWR_CTRL_ADDR				UINT8_C(0xF4)
N#define BME280_CTRL_HUM_ADDR				UINT8_C(0xF2)
N#define BME280_CTRL_MEAS_ADDR				UINT8_C(0xF4)
N#define BME280_CONFIG_ADDR					UINT8_C(0xF5)
N#define BME280_DATA_ADDR					UINT8_C(0xF7)
N
N/**\name API success code */
N#define BME280_OK					INT8_C(0)
N
N/**\name API error codes */
N#define BME280_E_NULL_PTR			INT8_C(-1)
N#define BME280_E_DEV_NOT_FOUND		INT8_C(-2)
N#define BME280_E_INVALID_LEN		INT8_C(-3)
N#define BME280_E_COMM_FAIL			INT8_C(-4)
N#define BME280_E_SLEEP_MODE_FAIL	INT8_C(-5)
N
N/**\name API warning codes */
N#define BME280_W_INVALID_OSR_MACRO      INT8_C(1)
N
N/**\name Macros related to size */
N#define BME280_TEMP_PRESS_CALIB_DATA_LEN	UINT8_C(26)
N#define BME280_HUMIDITY_CALIB_DATA_LEN		UINT8_C(7)
N#define BME280_P_T_H_DATA_LEN				UINT8_C(8)
N
N/**\name Sensor power modes */
N#define	BME280_SLEEP_MODE		UINT8_C(0x00)
N#define	BME280_FORCED_MODE		UINT8_C(0x01)
N#define	BME280_NORMAL_MODE		UINT8_C(0x03)
N
N/**\name Macro to combine two 8 bit data's to form a 16 bit data */
N#define BME280_CONCAT_BYTES(msb, lsb)     (((uint16_t)msb << 8) | (uint16_t)lsb)
N
N#define BME280_SET_BITS(reg_data, bitname, data) \
N				((reg_data & ~(bitname##_MSK)) | \
N				((data << bitname##_POS) & bitname##_MSK))
X#define BME280_SET_BITS(reg_data, bitname, data) 				((reg_data & ~(bitname##_MSK)) | 				((data << bitname##_POS) & bitname##_MSK))
N#define BME280_SET_BITS_POS_0(reg_data, bitname, data) \
N				((reg_data & ~(bitname##_MSK)) | \
N				(data & bitname##_MSK))
X#define BME280_SET_BITS_POS_0(reg_data, bitname, data) 				((reg_data & ~(bitname##_MSK)) | 				(data & bitname##_MSK))
N
N#define BME280_GET_BITS(reg_data, bitname)  ((reg_data & (bitname##_MSK)) >> \
N							(bitname##_POS))
X#define BME280_GET_BITS(reg_data, bitname)  ((reg_data & (bitname##_MSK)) >> 							(bitname##_POS))
N#define BME280_GET_BITS_POS_0(reg_data, bitname)  (reg_data & (bitname##_MSK))
N
N/**\name Macros for bit masking */
N#define BME280_SENSOR_MODE_MSK	UINT8_C(0x03)
N#define BME280_SENSOR_MODE_POS	UINT8_C(0x00)
N
N#define BME280_CTRL_HUM_MSK		UINT8_C(0x07)
N#define BME280_CTRL_HUM_POS		UINT8_C(0x00)
N
N#define BME280_CTRL_PRESS_MSK	UINT8_C(0x1C)
N#define BME280_CTRL_PRESS_POS	UINT8_C(0x02)
N
N#define BME280_CTRL_TEMP_MSK	UINT8_C(0xE0)
N#define BME280_CTRL_TEMP_POS	UINT8_C(0x05)
N
N#define BME280_FILTER_MSK		UINT8_C(0x1C)
N#define BME280_FILTER_POS		UINT8_C(0x02)
N
N#define BME280_STANDBY_MSK		UINT8_C(0xE0)
N#define BME280_STANDBY_POS		UINT8_C(0x05)
N
N/**\name Sensor component selection macros
N   These values are internal for API implementation. Don't relate this to
N   data sheet.*/
N#define BME280_PRESS		UINT8_C(1)
N#define BME280_TEMP			UINT8_C(1 << 1)
N#define BME280_HUM			UINT8_C(1 << 2)
N#define BME280_ALL			UINT8_C(0x07)
N
N/**\name Settings selection macros */
N#define BME280_OSR_PRESS_SEL		UINT8_C(1)
N#define BME280_OSR_TEMP_SEL			UINT8_C(1 << 1)
N#define BME280_OSR_HUM_SEL			UINT8_C(1 << 2)
N#define BME280_FILTER_SEL			UINT8_C(1 << 3)
N#define BME280_STANDBY_SEL			UINT8_C(1 << 4)
N#define BME280_ALL_SETTINGS_SEL		UINT8_C(0x1F)
N
N/**\name Oversampling macros */
N#define BME280_NO_OVERSAMPLING		UINT8_C(0x00)
N#define BME280_OVERSAMPLING_1X		UINT8_C(0x01)
N#define BME280_OVERSAMPLING_2X		UINT8_C(0x02)
N#define BME280_OVERSAMPLING_4X		UINT8_C(0x03)
N#define BME280_OVERSAMPLING_8X		UINT8_C(0x04)
N#define BME280_OVERSAMPLING_16X		UINT8_C(0x05)
N
N/**\name Standby duration selection macros */
N#define BME280_STANDBY_TIME_1_MS              (0x00)
N#define BME280_STANDBY_TIME_62_5_MS           (0x01)
N#define BME280_STANDBY_TIME_125_MS			  (0x02)
N#define BME280_STANDBY_TIME_250_MS            (0x03)
N#define BME280_STANDBY_TIME_500_MS            (0x04)
N#define BME280_STANDBY_TIME_1000_MS           (0x05)
N#define BME280_STANDBY_TIME_10_MS             (0x06)
N#define BME280_STANDBY_TIME_20_MS             (0x07)
N
N/**\name Filter coefficient selection macros */
N#define BME280_FILTER_COEFF_OFF               (0x00)
N#define BME280_FILTER_COEFF_2                 (0x01)
N#define BME280_FILTER_COEFF_4                 (0x02)
N#define BME280_FILTER_COEFF_8                 (0x03)
N#define BME280_FILTER_COEFF_16                (0x04)
N
N/*!
N * @brief Interface selection Enums
N */
Nenum bme280_intf {
N	/*! SPI interface */
N	BME280_SPI_INTF,
N	/*! I2C interface */
N	BME280_I2C_INTF
N};
N
N/*!
N * @brief Type definitions
N */
Ntypedef int8_t (*bme280_com_fptr_t)(uint8_t dev_id, uint8_t reg_addr,
N		uint8_t *data, uint16_t len);
N
Ntypedef void (*bme280_delay_fptr_t)(uint32_t period);
N
N/*!
N * @brief Calibration data
N */
Nstruct bme280_calib_data {
N /**
N * @ Trim Variables
N */
N/**@{*/
N	uint16_t dig_T1;
N	int16_t dig_T2;
N	int16_t dig_T3;
N	uint16_t dig_P1;
N	int16_t dig_P2;
N	int16_t dig_P3;
N	int16_t dig_P4;
N	int16_t dig_P5;
N	int16_t dig_P6;
N	int16_t dig_P7;
N	int16_t dig_P8;
N	int16_t dig_P9;
N	uint8_t  dig_H1;
N	int16_t dig_H2;
N	uint8_t  dig_H3;
N	int16_t dig_H4;
N	int16_t dig_H5;
N	int8_t  dig_H6;
N	int32_t t_fine;
N/**@}*/
N};
N
N/*!
N * @brief bme280 sensor structure which comprises of temperature, pressure and
N * humidity data
N */
N#ifdef BME280_FLOAT_ENABLE
Sstruct bme280_data {
S	/*! Compensated pressure */
S	double pressure;
S	/*! Compensated temperature */
S	double temperature;
S	/*! Compensated humidity */
S	double humidity;
S};
N#else
Nstruct bme280_data {
N	/*! Compensated pressure */
N	uint32_t pressure;
N	/*! Compensated temperature */
N	int32_t temperature;
N	/*! Compensated humidity */
N	uint32_t humidity;
N};
N#endif /* BME280_USE_FLOATING_POINT */
N
N/*!
N * @brief bme280 sensor structure which comprises of uncompensated temperature,
N * pressure and humidity data
N */
Nstruct bme280_uncomp_data {
N	/*! un-compensated pressure */
N	uint32_t pressure;
N	/*! un-compensated temperature */
N	uint32_t temperature;
N	/*! un-compensated humidity */
N	uint32_t humidity;
N};
N
N/*!
N * @brief bme280 sensor settings structure which comprises of mode,
N * oversampling and filter settings.
N */
Nstruct bme280_settings {
N	/*! pressure oversampling */
N	uint8_t osr_p;
N	/*! temperature oversampling */
N	uint8_t osr_t;
N	/*! humidity oversampling */
N	uint8_t osr_h;
N	/*! filter coefficient */
N	uint8_t filter;
N	/*! standby time */
N	uint8_t standby_time;
N};
N
N/*!
N * @brief bme280 device structure
N */
Nstruct bme280_dev {
N	/*! Chip Id */
N	uint8_t chip_id;
N	/*! Device Id */
N	uint8_t dev_id;
N	/*! SPI/I2C interface */
N	enum bme280_intf intf;
N	/*! Read function pointer */
N	bme280_com_fptr_t read;
N	/*! Write function pointer */
N	bme280_com_fptr_t write;
N	/*! Delay function pointer */
N	bme280_delay_fptr_t delay_ms;
N	/*! Trim data */
N	struct bme280_calib_data calib_data;
N	/*! Sensor settings */
N	struct bme280_settings settings;
N};
N
N#endif /* BME280_DEFS_H_ */
N/** @}*/
N/** @}*/
L 63 "..\bme280_old.h" 2
N
N/*!
N *  @brief This API is the entry point.
N *  It reads the chip-id and calibration data from the sensor.
N *
N *  @param[in,out] dev : Structure instance of bme280_dev
N *
N *  @return Result of API execution status
N *  @retval zero -> Success / +ve value -> Warning / -ve value -> Error
N */
Nint8_t bme280_init(struct bme280_dev *dev);
N
N/*!
N * @brief This API writes the given data to the register address
N * of the sensor.
N *
N * @param[in] reg_addr : Register address from where the data to be written.
N * @param[in] reg_data : Pointer to data buffer which is to be written
N * in the sensor.
N * @param[in] len : No of bytes of data to write..
N * @param[in] dev : Structure instance of bme280_dev.
N *
N * @return Result of API execution status
N * @retval zero -> Success / +ve value -> Warning / -ve value -> Error
N */
Nint8_t bme280_set_regs(uint8_t *reg_addr, const uint8_t *reg_data, uint8_t len, const struct bme280_dev *dev);
N
N/*!
N * @brief This API reads the data from the given register address of the sensor.
N *
N * @param[in] reg_addr : Register address from where the data to be read
N * @param[out] reg_data : Pointer to data buffer to store the read data.
N * @param[in] len : No of bytes of data to be read.
N * @param[in] dev : Structure instance of bme280_dev.
N *
N * @return Result of API execution status
N * @retval zero -> Success / +ve value -> Warning / -ve value -> Error
N */
Nint8_t bme280_get_regs(uint8_t reg_addr, uint8_t *reg_data, uint16_t len, const struct bme280_dev *dev);
N
N/*!
N * @brief This API sets the oversampling, filter and standby duration
N * (normal mode) settings in the sensor.
N *
N * @param[in] dev : Structure instance of bme280_dev.
N * @param[in] desired_settings : Variable used to select the settings which
N * are to be set in the sensor.
N *
N * @note : Below are the macros to be used by the user for selecting the
N * desired settings. User can do OR operation of these macros for configuring
N * multiple settings.
N *
N * Macros		  |   Functionality
N * -----------------------|----------------------------------------------
N * BME280_OSR_PRESS_SEL    |   To set pressure oversampling.
N * BME280_OSR_TEMP_SEL     |   To set temperature oversampling.
N * BME280_OSR_HUM_SEL    |   To set humidity oversampling.
N * BME280_FILTER_SEL     |   To set filter setting.
N * BME280_STANDBY_SEL  |   To set standby duration setting.
N *
N * @return Result of API execution status
N * @retval zero -> Success / +ve value -> Warning / -ve value -> Error.
N */
Nint8_t bme280_set_sensor_settings(uint8_t desired_settings, const struct bme280_dev *dev);
N
N/*!
N * @brief This API gets the oversampling, filter and standby duration
N * (normal mode) settings from the sensor.
N *
N * @param[in,out] dev : Structure instance of bme280_dev.
N *
N * @return Result of API execution status
N * @retval zero -> Success / +ve value -> Warning / -ve value -> Error.
N */
Nint8_t bme280_get_sensor_settings(struct bme280_dev *dev);
N
N/*!
N * @brief This API sets the power mode of the sensor.
N *
N * @param[in] dev : Structure instance of bme280_dev.
N * @param[in] sensor_mode : Variable which contains the power mode to be set.
N *
N *    sensor_mode           |   Macros
N * ---------------------|-------------------
N *     0                | BME280_SLEEP_MODE
N *     1                | BME280_FORCED_MODE
N *     3                | BME280_NORMAL_MODE
N *
N * @return Result of API execution status
N * @retval zero -> Success / +ve value -> Warning / -ve value -> Error
N */
Nint8_t bme280_set_sensor_mode(uint8_t sensor_mode,
N				const struct bme280_dev *dev);
N
N/*!
N * @brief This API gets the power mode of the sensor.
N *
N * @param[in] dev : Structure instance of bme280_dev.
N * @param[out] sensor_mode : Pointer variable to store the power mode.
N *
N *   sensor_mode            |   Macros
N * ---------------------|-------------------
N *     0                | BME280_SLEEP_MODE
N *     1                | BME280_FORCED_MODE
N *     3                | BME280_NORMAL_MODE
N *
N * @return Result of API execution status
N * @retval zero -> Success / +ve value -> Warning / -ve value -> Error
N */
Nint8_t bme280_get_sensor_mode(uint8_t *sensor_mode, const struct bme280_dev *dev);
N
N/*!
N * @brief This API performs the soft reset of the sensor.
N *
N * @param[in] dev : Structure instance of bme280_dev.
N *
N * @return Result of API execution status
N * @retval zero -> Success / +ve value -> Warning / -ve value -> Error.
N */
Nint8_t bme280_soft_reset(const struct bme280_dev *dev);
N
N/*!
N * @brief This API reads the pressure, temperature and humidity data from the
N * sensor, compensates the data and store it in the bme280_data structure
N * instance passed by the user.
N *
N * @param[in] sensor_comp : Variable which selects which data to be read from
N * the sensor.
N *
N * sensor_comp |   Macros
N * ------------|-------------------
N *     1       | BME280_PRESS
N *     2       | BME280_TEMP
N *     4       | BME280_HUM
N *     7       | BME280_ALL
N *
N * @param[out] comp_data : Structure instance of bme280_data.
N * @param[in] dev : Structure instance of bme280_dev.
N *
N * @return Result of API execution status
N * @retval zero -> Success / +ve value -> Warning / -ve value -> Error
N */
Nint8_t bme280_get_sensor_data(uint8_t sensor_comp, struct bme280_data *comp_data, struct bme280_dev *dev);
N
N/*!
N *  @brief This API is used to parse the pressure, temperature and
N *  humidity data and store it in the bme280_uncomp_data structure instance.
N *
N *  @param[in] reg_data     : Contains register data which needs to be parsed
N *  @param[out] uncomp_data : Contains the uncompensated pressure, temperature
N *  and humidity data.
N */
Nvoid bme280_parse_sensor_data(const uint8_t *reg_data, struct bme280_uncomp_data *uncomp_data);
N
N/*!
N * @brief This API is used to compensate the pressure and/or
N * temperature and/or humidity data according to the component selected by the
N * user.
N *
N * @param[in] sensor_comp : Used to select pressure and/or temperature and/or
N * humidity.
N * @param[in] uncomp_data : Contains the uncompensated pressure, temperature and
N * humidity data.
N * @param[out] comp_data : Contains the compensated pressure and/or temperature
N * and/or humidity data.
N * @param[in] calib_data : Pointer to the calibration data structure.
N *
N * @return Result of API execution status.
N * @retval zero -> Success / -ve value -> Error
N */
Nint8_t bme280_compensate_data(uint8_t sensor_comp, const struct bme280_uncomp_data *uncomp_data,
N				     struct bme280_data *comp_data, struct bme280_calib_data *calib_data);
N
N#ifdef __cplusplus
S}
N#endif /* End of CPP guard */
N#endif /* BME280_H_ */
N/** @}*/
L 51 "../bme280_old.c" 2
N
N/**\name Internal macros */
N/* To identify osr settings selected by user */
N#define OVERSAMPLING_SETTINGS		UINT8_C(0x07)
N/* To identify filter and standby settings selected by user */
N#define FILTER_STANDBY_SETTINGS		UINT8_C(0x18)
N
N/*!
N * @brief This internal API puts the device to sleep mode.
N *
N * @param[in] dev : Structure instance of bme280_dev.
N *
N * @return Result of API execution status.
N * @retval zero -> Success / +ve value -> Warning / -ve value -> Error
N */
Nstatic int8_t put_device_to_sleep(const struct bme280_dev *dev);
N
N/*!
N * @brief This internal API writes the power mode in the sensor.
N *
N * @param[in] dev : Structure instance of bme280_dev.
N * @param[in] sensor_mode : Variable which contains the power mode to be set.
N *
N * @return Result of API execution status.
N * @retval zero -> Success / +ve value -> Warning / -ve value -> Error
N */
Nstatic int8_t write_power_mode(uint8_t sensor_mode, const struct bme280_dev *dev);
N
N/*!
N * @brief This internal API is used to validate the device pointer for
N * null conditions.
N *
N * @param[in] dev : Structure instance of bme280_dev.
N *
N * @return Result of API execution status
N * @retval zero -> Success / +ve value -> Warning / -ve value -> Error
N */
Nstatic int8_t null_ptr_check(const struct bme280_dev *dev);
N
N/*!
N * @brief This internal API interleaves the register address between the
N * register data buffer for burst write operation.
N *
N * @param[in] reg_addr : Contains the register address array.
N * @param[out] temp_buff : Contains the temporary buffer to store the
N * register data and register address.
N * @param[in] reg_data : Contains the register data to be written in the
N * temporary buffer.
N * @param[in] len : No of bytes of data to be written for burst write.
N */
Nstatic void interleave_reg_addr(const uint8_t *reg_addr, uint8_t *temp_buff, const uint8_t *reg_data, uint8_t len);
N
N/*!
N * @brief This internal API reads the calibration data from the sensor, parse
N * it and store in the device structure.
N *
N * @param[in] dev : Structure instance of bme280_dev.
N *
N * @return Result of API execution status
N * @retval zero -> Success / +ve value -> Warning / -ve value -> Error
N */
Nstatic int8_t get_calib_data(struct bme280_dev *dev);
N
N/*!
N *  @brief This internal API is used to parse the temperature and
N *  pressure calibration data and store it in the device structure.
N *
N *  @param[out] dev : Structure instance of bme280_dev to store the calib data.
N *  @param[in] reg_data : Contains the calibration data to be parsed.
N */
Nstatic void parse_temp_press_calib_data(const uint8_t *reg_data, struct bme280_dev *dev);
N
N/*!
N *  @brief This internal API is used to parse the humidity calibration data
N *  and store it in device structure.
N *
N *  @param[out] dev : Structure instance of bme280_dev to store the calib data.
N *  @param[in] reg_data : Contains calibration data to be parsed.
N */
Nstatic void parse_humidity_calib_data(const uint8_t *reg_data, struct bme280_dev *dev);
N
N#ifdef BME280_FLOAT_ENABLE
S/*!
S * @brief This internal API is used to compensate the raw pressure data and
S * return the compensated pressure data in double data type.
S *
S * @param[in] uncomp_data : Contains the uncompensated pressure data.
S * @param[in] calib_data : Pointer to the calibration data structure.
S *
S * @return Compensated pressure data.
S * @retval Compensated pressure data in double.
S */
Sstatic double compensate_pressure(const struct bme280_uncomp_data *uncomp_data,
S						const struct bme280_calib_data *calib_data);
S
S/*!
S * @brief This internal API is used to compensate the raw humidity data and
S * return the compensated humidity data in double data type.
S *
S * @param[in] uncomp_data : Contains the uncompensated humidity data.
S * @param[in] calib_data : Pointer to the calibration data structure.
S *
S * @return Compensated humidity data.
S * @retval Compensated humidity data in double.
S */
Sstatic double compensate_humidity(const struct bme280_uncomp_data *uncomp_data,
S						const struct bme280_calib_data *calib_data);
S
S/*!
S * @brief This internal API is used to compensate the raw temperature data and
S * return the compensated temperature data in double data type.
S *
S * @param[in] uncomp_data : Contains the uncompensated temperature data.
S * @param[in] calib_data : Pointer to calibration data structure.
S *
S * @return Compensated temperature data.
S * @retval Compensated temperature data in double.
S */
Sstatic  double compensate_temperature(const struct bme280_uncomp_data *uncomp_data,
S						struct bme280_calib_data *calib_data);
S
N#else
N
N/*!
N * @brief This internal API is used to compensate the raw temperature data and
N * return the compensated temperature data in integer data type.
N *
N * @param[in] uncomp_data : Contains the uncompensated temperature data.
N * @param[in] calib_data : Pointer to calibration data structure.
N *
N * @return Compensated temperature data.
N * @retval Compensated temperature data in integer.
N */
Nstatic int32_t compensate_temperature(const struct bme280_uncomp_data *uncomp_data,
N						struct bme280_calib_data *calib_data);
N
N/*!
N * @brief This internal API is used to compensate the raw pressure data and
N * return the compensated pressure data in integer data type.
N *
N * @param[in] uncomp_data : Contains the uncompensated pressure data.
N * @param[in] calib_data : Pointer to the calibration data structure.
N *
N * @return Compensated pressure data.
N * @retval Compensated pressure data in integer.
N */
Nstatic uint32_t compensate_pressure(const struct bme280_uncomp_data *uncomp_data,
N						const struct bme280_calib_data *calib_data);
N
N/*!
N * @brief This internal API is used to compensate the raw humidity data and
N * return the compensated humidity data in integer data type.
N *
N * @param[in] uncomp_data : Contains the uncompensated humidity data.
N * @param[in] calib_data : Pointer to the calibration data structure.
N *
N * @return Compensated humidity data.
N * @retval Compensated humidity data in integer.
N */
Nstatic uint32_t compensate_humidity(const struct bme280_uncomp_data *uncomp_data,
N						const struct bme280_calib_data *calib_data);
N
N#endif
N
N/*!
N * @brief This internal API is used to identify the settings which the user
N * wants to modify in the sensor.
N *
N * @param[in] sub_settings : Contains the settings subset to identify particular
N * group of settings which the user is interested to change.
N * @param[in] desired_settings : Contains the user specified settings.
N *
N * @return Indicates whether user is interested to modify the settings which
N * are related to sub_settings.
N * @retval True -> User wants to modify this group of settings
N * @retval False -> User does not want to modify this group of settings
N */
Nstatic uint8_t are_settings_changed(uint8_t sub_settings, uint8_t desired_settings);
N
N/*!
N * @brief This API sets the humidity oversampling settings of the sensor.
N *
N * @param[in] dev : Structure instance of bme280_dev.
N *
N * @return Result of API execution status
N * @retval zero -> Success / +ve value -> Warning / -ve value -> Error
N */
Nstatic int8_t set_osr_humidity_settings(const struct bme280_settings *settings, const struct bme280_dev *dev);
N
N/*!
N * @brief This internal API sets the oversampling settings for pressure,
N * temperature and humidity in the sensor.
N *
N * @param[in] desired_settings : Variable used to select the settings which
N * are to be set.
N * @param[in] dev : Structure instance of bme280_dev.
N *
N * @return Result of API execution status
N * @retval zero -> Success / +ve value -> Warning / -ve value -> Error
N */
Nstatic int8_t set_osr_settings(uint8_t desired_settings, const struct bme280_settings *settings,
N				const struct bme280_dev *dev);
N
N/*!
N * @brief This API sets the pressure and/or temperature oversampling settings
N * in the sensor according to the settings selected by the user.
N *
N * @param[in] dev : Structure instance of bme280_dev.
N * @param[in] desired_settings: variable to select the pressure and/or
N * temperature oversampling settings.
N *
N * @return Result of API execution status
N * @retval zero -> Success / +ve value -> Warning / -ve value -> Error
N */
Nstatic int8_t set_osr_press_temp_settings(uint8_t desired_settings, const struct bme280_settings *settings,
N						const struct bme280_dev *dev);
N
N/*!
N * @brief This internal API fills the pressure oversampling settings provided by
N * the user in the data buffer so as to write in the sensor.
N *
N * @param[in] dev : Structure instance of bme280_dev.
N * @param[out] reg_data : Variable which is filled according to the pressure
N * oversampling data provided by the user.
N */
Nstatic void fill_osr_press_settings(uint8_t *reg_data, const struct bme280_settings *settings);
N
N/*!
N * @brief This internal API fills the temperature oversampling settings provided
N * by the user in the data buffer so as to write in the sensor.
N *
N * @param[in] dev : Structure instance of bme280_dev.
N * @param[out] reg_data : Variable which is filled according to the temperature
N * oversampling data provided by the user.
N */
Nstatic void fill_osr_temp_settings(uint8_t *reg_data, const struct bme280_settings *settings);
N
N/*!
N * @brief This internal API sets the filter and/or standby duration settings
N * in the sensor according to the settings selected by the user.
N *
N * @param[in] dev : Structure instance of bme280_dev.
N * @param[in] desired_settings : variable to select the filter and/or
N * standby duration settings.
N *
N * @return Result of API execution status
N * @retval zero -> Success / +ve value -> Warning / -ve value -> Error
N */
Nstatic int8_t set_filter_standby_settings(uint8_t desired_settings, const struct bme280_settings *settings,
N						const struct bme280_dev *dev);
N
N/*!
N * @brief This internal API fills the filter settings provided by the user
N * in the data buffer so as to write in the sensor.
N *
N * @param[in] dev : Structure instance of bme280_dev.
N * @param[out] reg_data : Variable which is filled according to the filter
N * settings data provided by the user.
N */
Nstatic void fill_filter_settings(uint8_t *reg_data, const struct bme280_settings *settings);
N
N/*!
N * @brief This internal API fills the standby duration settings provided by the
N * user in the data buffer so as to write in the sensor.
N *
N * @param[in] dev : Structure instance of bme280_dev.
N * @param[out] reg_data : Variable which is filled according to the standby
N * settings data provided by the user.
N */
Nstatic void fill_standby_settings(uint8_t *reg_data, const struct bme280_settings *settings);
N
N/*!
N * @brief This internal API parse the oversampling(pressure, temperature
N * and humidity), filter and standby duration settings and store in the
N * device structure.
N *
N * @param[out] dev : Structure instance of bme280_dev.
N * @param[in] reg_data : Register data to be parsed.
N */
Nstatic void parse_device_settings(const uint8_t *reg_data, struct bme280_settings *settings);
N
N/*!
N * @brief This internal API reloads the already existing device settings in the
N * sensor after soft reset.
N *
N * @param[in] dev : Structure instance of bme280_dev.
N * @param[in] settings : Pointer variable which contains the settings to
N * be set in the sensor.
N *
N * @return Result of API execution status
N * @retval zero -> Success / +ve value -> Warning / -ve value -> Error
N */
Nstatic int8_t reload_device_settings(const struct bme280_settings *settings, const struct bme280_dev *dev);
N
N/****************** Global Function Definitions *******************************/
N
N/*!
N *  @brief This API is the entry point.
N *  It reads the chip-id and calibration data from the sensor.
N */
Nint8_t bme280_init(struct bme280_dev *dev)
N{
N	int8_t rslt;
N	/* chip id read try count */
N	uint8_t try_count = 5;
N	uint8_t chip_id = 0;
N
N	/* Check for null pointer in the device structure*/
N	rslt = null_ptr_check(dev);
N	/* Proceed if null check is fine */
N	if (rslt ==  BME280_OK) {
X	if (rslt ==  ((int_least8_t)(0))) {
N		while (try_count) {
N			/* Read the chip-id of bme280 sensor */
N			rslt = bme280_get_regs(BME280_CHIP_ID_ADDR, &chip_id, 1, dev);
X			rslt = bme280_get_regs(((uint_least8_t)(0xD0)), &chip_id, 1, dev);
N			/* Check for chip id validity */
N			chip_id = 0x60;
N			if ((rslt == 0) && (chip_id == BME280_CHIP_ID)) {//BME280_OK
X			if ((rslt == 0) && (chip_id == ((uint_least8_t)(0x60)))) {
N				dev->chip_id = chip_id;
N				/* Reset the sensor */
N				rslt = bme280_soft_reset(dev);
N				if (rslt == BME280_OK) {
X				if (rslt == ((int_least8_t)(0))) {
N					/* Read the calibration data */
N					rslt = get_calib_data(dev);
N				}
N				break;
N			}
N			/* Wait for 1 ms */
N			dev->delay_ms(1);
N			--try_count;
N		}
N		/* Chip id check failed */
N		if (!try_count)
N			rslt = BME280_E_DEV_NOT_FOUND;
X			rslt = ((int_least8_t)(-2));
N	}
N
N	return rslt;
R "../bme280_old.c" 364 48 (ULP 5.4) Detected an assignment to a type with size less than int. To avoid unnecessary sign extension, use int-sized types for local varaibles and convert to smaller types for static storage.
R "../bme280_old.c" 379 4 (ULP 5.4) Detected an assignment to a type with size less than int. To avoid unnecessary sign extension, use int-sized types for local varaibles and convert to smaller types for static storage.
N}
N
N/*!
N * @brief This API reads the data from the given register address of the sensor.
N */
Nint8_t bme280_get_regs(uint8_t reg_addr, uint8_t *reg_data, uint16_t len, const struct bme280_dev *dev)
N{
N	int8_t rslt;
N
N	/* Check for null pointer in the device structure*/
N	rslt = null_ptr_check(dev);
N	/* Proceed if null check is fine */
N	if (rslt == BME280_OK) {
X	if (rslt == ((int_least8_t)(0))) {
N		/* If interface selected is SPI */
N		if (dev->intf != 1)//BME280_I2C_INTF)
N			reg_addr = reg_addr | 0x80;
N		/* Read the data  */
N		rslt = dev->read(dev->dev_id, reg_addr, reg_data, len);
N		/* Check for communication error */
N//		if (rslt != 0)//BME280_OK)
N//			rslt = BME280_E_COMM_FAIL;
N	}
N
N	return rslt;
R "../bme280_old.c" 402 13 (ULP 5.4) Detected an assignment to a type with size less than int. To avoid unnecessary sign extension, use int-sized types for local varaibles and convert to smaller types for static storage.
N}
N
N/*!
N * @brief This API writes the given data to the register address
N * of the sensor.
N */
Nint8_t bme280_set_regs(uint8_t *reg_addr, const uint8_t *reg_data, uint8_t len, const struct bme280_dev *dev)
N{
N	int8_t rslt;
N	uint8_t temp_buff[20]; /* Typically not to write more than 10 registers */
N
N	if (len > 10)
N		len = 10;
N
N	uint16_t temp_len;
N	uint8_t reg_addr_cnt;
N
N	/* Check for null pointer in the device structure*/
N	rslt = null_ptr_check(dev);
N	/* Check for arguments validity */
N	if ((rslt ==  BME280_OK) && (reg_addr != NULL) && (reg_data != NULL)) {
X	if ((rslt ==  ((int_least8_t)(0))) && (reg_addr != 0) && (reg_data != 0)) {
N		if (len != 0) {
N			temp_buff[0] = reg_data[0];
N			/* If interface selected is SPI */
N			if (dev->intf != BME280_I2C_INTF) {
N				for (reg_addr_cnt = 0; reg_addr_cnt < len; reg_addr_cnt++)
N					reg_addr[reg_addr_cnt] = reg_addr[reg_addr_cnt] & 0x7F;
N			}
N			/* Burst write mode */
N			if (len > 1) {
N				/* Interleave register address w.r.t data for
N				burst write*/
N				interleave_reg_addr(reg_addr, temp_buff, reg_data, len);
N				temp_len = ((len * 2) - 1);
N			} else {
N				temp_len = len;
N			}
N			rslt = dev->write(dev->dev_id, reg_addr[0], temp_buff, temp_len);
N			/* Check for communication error */
N			if (rslt != BME280_OK)
X			if (rslt != ((int_least8_t)(0)))
N				rslt = BME280_E_COMM_FAIL;
X				rslt = ((int_least8_t)(-4));
N		} else {
N			rslt = BME280_E_INVALID_LEN;
X			rslt = ((int_least8_t)(-3));
N		}
N	} else {
N		rslt = BME280_E_NULL_PTR;
X		rslt = ((int_least8_t)(-1));
N	}
N
N
N	return rslt;
R "../bme280_old.c" 436 60 (ULP 5.4) Detected an assignment to a type with size less than int. To avoid unnecessary sign extension, use int-sized types for local varaibles and convert to smaller types for static storage.
R "../bme280_old.c" 444 14 (ULP 5.4) Detected an assignment to a type with size less than int. To avoid unnecessary sign extension, use int-sized types for local varaibles and convert to smaller types for static storage.
R "../bme280_old.c" 446 14 (ULP 5.4) Detected an assignment to a type with size less than int. To avoid unnecessary sign extension, use int-sized types for local varaibles and convert to smaller types for static storage.
N}
N
N/*!
N * @brief This API sets the oversampling, filter and standby duration
N * (normal mode) settings in the sensor.
N */
Nint8_t bme280_set_sensor_settings(uint8_t desired_settings, const struct bme280_dev *dev)
N{
N	int8_t rslt;
N	uint8_t sensor_mode;
N
N	/* Check for null pointer in the device structure*/
N	rslt = null_ptr_check(dev);
N	/* Proceed if null check is fine */
N	if (rslt == BME280_OK) {
X	if (rslt == ((int_least8_t)(0))) {
N		rslt = bme280_get_sensor_mode(&sensor_mode, dev);
N		if ((rslt == BME280_OK) && (sensor_mode != BME280_SLEEP_MODE))
X		if ((rslt == ((int_least8_t)(0))) && (sensor_mode != ((uint_least8_t)(0x00))))
N			rslt = put_device_to_sleep(dev);
N		if (rslt == BME280_OK) {
X		if (rslt == ((int_least8_t)(0))) {
N			/* Check if user wants to change oversampling
N			   settings */
N			if (are_settings_changed(OVERSAMPLING_SETTINGS, desired_settings))
X			if (are_settings_changed(((uint_least8_t)(0x07)), desired_settings))
N				rslt = set_osr_settings(desired_settings, &dev->settings, dev);
N			/* Check if user wants to change filter and/or
N			   standby settings */
N			if ((rslt == BME280_OK) && are_settings_changed(FILTER_STANDBY_SETTINGS, desired_settings))
X			if ((rslt == ((int_least8_t)(0))) && are_settings_changed(((uint_least8_t)(0x18)), desired_settings))
N				rslt = set_filter_standby_settings(desired_settings, &dev->settings, dev);
N		}
N	}
N
N	return rslt;
R "../bme280_old.c" 476 33 (ULP 5.4) Detected an assignment to a type with size less than int. To avoid unnecessary sign extension, use int-sized types for local varaibles and convert to smaller types for static storage.
N}
N
N/*!
N * @brief This API gets the oversampling, filter and standby duration
N * (normal mode) settings from the sensor.
N */
Nint8_t bme280_get_sensor_settings(struct bme280_dev *dev)
N{
N	int8_t rslt;
N	uint8_t reg_data[4];
N
N	/* Check for null pointer in the device structure*/
N	rslt = null_ptr_check(dev);
N	/* Proceed if null check is fine */
N	if (rslt == BME280_OK) {
X	if (rslt == ((int_least8_t)(0))) {
N		rslt = bme280_get_regs(BME280_CTRL_HUM_ADDR, reg_data, 4, dev);
X		rslt = bme280_get_regs(((uint_least8_t)(0xF2)), reg_data, 4, dev);
N		if (rslt == BME280_OK)
X		if (rslt == ((int_least8_t)(0)))
N			parse_device_settings(reg_data, &dev->settings);
N	}
N
N	return rslt;
N}
N
N/*!
N * @brief This API sets the power mode of the sensor.
N */
Nint8_t bme280_set_sensor_mode(uint8_t sensor_mode, const struct bme280_dev *dev)
N{
N	int8_t rslt;
N	uint8_t last_set_mode;
N
N	/* Check for null pointer in the device structure*/
N	rslt = null_ptr_check(dev);
N
N	if (rslt == BME280_OK) {
X	if (rslt == ((int_least8_t)(0))) {
N		rslt = bme280_get_sensor_mode(&last_set_mode, dev);
N		/* If the sensor is not in sleep mode put the device to sleep
N		   mode */
N		if ((rslt == BME280_OK) && (last_set_mode != BME280_SLEEP_MODE))
X		if ((rslt == ((int_least8_t)(0))) && (last_set_mode != ((uint_least8_t)(0x00))))
N			rslt = put_device_to_sleep(dev);
N		/* Set the power mode */
N		if (rslt == BME280_OK)
X		if (rslt == ((int_least8_t)(0)))
N			rslt = write_power_mode(sensor_mode, dev);
N	}
N
N	return rslt;
R "../bme280_old.c" 527 33 (ULP 5.4) Detected an assignment to a type with size less than int. To avoid unnecessary sign extension, use int-sized types for local varaibles and convert to smaller types for static storage.
N}
N
N/*!
N * @brief This API gets the power mode of the sensor.
N */
Nint8_t bme280_get_sensor_mode(uint8_t *sensor_mode, const struct bme280_dev *dev)
N{
N	int8_t rslt;
N
N	/* Check for null pointer in the device structure*/
N	rslt = null_ptr_check(dev);
N
N	if (rslt == BME280_OK) {
X	if (rslt == ((int_least8_t)(0))) {
N		/* Read the power mode register */
N		rslt = bme280_get_regs(BME280_PWR_CTRL_ADDR, sensor_mode, 1, dev);
X		rslt = bme280_get_regs(((uint_least8_t)(0xF4)), sensor_mode, 1, dev);
N		/* Assign the power mode in the device structure */
N		*sensor_mode = BME280_GET_BITS_POS_0(*sensor_mode, BME280_SENSOR_MODE);
X		*sensor_mode = (*sensor_mode & (((uint_least8_t)(0x03))));
N	}
N
N	return rslt;
N}
N
N/*!
N * @brief This API performs the soft reset of the sensor.
N */
Nint8_t bme280_soft_reset(const struct bme280_dev *dev)
N{
N	int8_t rslt;
N	uint8_t reg_addr = BME280_RESET_ADDR;
X	uint8_t reg_addr = ((uint_least8_t)(0xE0));
N	/* 0xB6 is the soft reset command */
N	uint8_t soft_rst_cmd = 0xB6;
N
N	/* Check for null pointer in the device structure*/
N	rslt = null_ptr_check(dev);
N	/* Proceed if null check is fine */
N	if (rslt == BME280_OK) {
X	if (rslt == ((int_least8_t)(0))) {
N		/* Write the soft reset command in the sensor */
N		rslt = bme280_set_regs(&reg_addr, &soft_rst_cmd, 1, dev);
N		/* As per data sheet, startup time is 2 ms. */
N		dev->delay_ms(2);
N	}
N
N	return rslt;
R "../bme280_old.c" 575 26 (ULP 5.4) Detected an assignment to a type with size less than int. To avoid unnecessary sign extension, use int-sized types for local varaibles and convert to smaller types for static storage.
R "../bme280_old.c" 575 37 (ULP 5.4) Detected an assignment to a type with size less than int. To avoid unnecessary sign extension, use int-sized types for local varaibles and convert to smaller types for static storage.
N}
N
N/*!
N * @brief This API reads the pressure, temperature and humidity data from the
N * sensor, compensates the data and store it in the bme280_data structure
N * instance passed by the user.
N */
Nint8_t bme280_get_sensor_data(uint8_t sensor_comp, struct bme280_data *comp_data, struct bme280_dev *dev)
N{
N	int8_t rslt;
N	/* Array to store the pressure, temperature and humidity data read from
N	the sensor */
N	uint8_t reg_data[BME280_P_T_H_DATA_LEN] = {0};
X	uint8_t reg_data[((uint_least8_t)(8))] = {0};
N	struct bme280_uncomp_data uncomp_data = {0};
N
N	/* Check for null pointer in the device structure*/
N	rslt = null_ptr_check(dev);
N
N	if ((rslt == BME280_OK) && (comp_data != NULL)) {
X	if ((rslt == ((int_least8_t)(0))) && (comp_data != 0)) {
N		/* Read the pressure and temperature data from the sensor */
N		rslt = bme280_get_regs(BME280_DATA_ADDR, reg_data, BME280_P_T_H_DATA_LEN, dev);
X		rslt = bme280_get_regs(((uint_least8_t)(0xF7)), reg_data, ((uint_least8_t)(8)), dev);
N
N		if (rslt == BME280_OK) {
X		if (rslt == ((int_least8_t)(0))) {
N			/* Parse the read data from the sensor */
N			bme280_parse_sensor_data(reg_data, &uncomp_data);
N			/* Compensate the pressure and/or temperature and/or
N			   humidity data from the sensor */
N			rslt = bme280_compensate_data(sensor_comp, &uncomp_data, comp_data, &dev->calib_data);
N		}
N	} else {
N		rslt = BME280_E_NULL_PTR;
X		rslt = ((int_least8_t)(-1));
N	}
N
N	return rslt;
N}
N
N/*!
N *  @brief This API is used to parse the pressure, temperature and
N *  humidity data and store it in the bme280_uncomp_data structure instance.
N */
Nvoid bme280_parse_sensor_data(const uint8_t *reg_data, struct bme280_uncomp_data *uncomp_data)
N{
N	/* Variables to store the sensor data */
N	uint32_t data_xlsb;
N	uint32_t data_lsb;
N	uint32_t data_msb;
N
N	/* Store the parsed register values for pressure data */
N	data_msb = (uint32_t)reg_data[0] << 12;
N	data_lsb = (uint32_t)reg_data[1] << 4;
N	data_xlsb = (uint32_t)reg_data[2] >> 4;
N	uncomp_data->pressure = data_msb | data_lsb | data_xlsb;
N
N	/* Store the parsed register values for temperature data */
N	data_msb = (uint32_t)reg_data[3] << 12;
N	data_lsb = (uint32_t)reg_data[4] << 4;
N	data_xlsb = (uint32_t)reg_data[5] >> 4;
N	uncomp_data->temperature = data_msb | data_lsb | data_xlsb;
N
N	/* Store the parsed register values for temperature data */
N	data_lsb = (uint32_t)reg_data[6] << 8;
N	data_msb = (uint32_t)reg_data[7];
N	uncomp_data->humidity = data_msb | data_lsb;
N}
N
N
N/*!
N * @brief This API is used to compensate the pressure and/or
N * temperature and/or humidity data according to the component selected
N * by the user.
N */
Nint8_t bme280_compensate_data(uint8_t sensor_comp, const struct bme280_uncomp_data *uncomp_data,
N				     struct bme280_data *comp_data, struct bme280_calib_data *calib_data)
N{
N	int8_t rslt = BME280_OK;
X	int8_t rslt = ((int_least8_t)(0));
N
N	if ((uncomp_data != NULL) && (comp_data != NULL) && (calib_data != NULL)) {
X	if ((uncomp_data != 0) && (comp_data != 0) && (calib_data != 0)) {
N		/* Initialize to zero */
N		comp_data->temperature = 0;
N		comp_data->pressure = 0;
N		comp_data->humidity = 0;
N		/* If pressure or temperature component is selected */
N		if (sensor_comp & (BME280_PRESS | BME280_TEMP | BME280_HUM)) {
X		if (sensor_comp & (((uint_least8_t)(1)) | ((uint_least8_t)(1 << 1)) | ((uint_least8_t)(1 << 2)))) {
N			/* Compensate the temperature data */
N			comp_data->temperature = compensate_temperature(uncomp_data, calib_data);
N		}
N		if (sensor_comp & BME280_PRESS) {
X		if (sensor_comp & ((uint_least8_t)(1))) {
N			/* Compensate the pressure data */
N			comp_data->pressure = compensate_pressure(uncomp_data, calib_data);
N		}
N		if (sensor_comp & BME280_HUM) {
X		if (sensor_comp & ((uint_least8_t)(1 << 2))) {
N			/* Compensate the humidity data */
N			comp_data->humidity = compensate_humidity(uncomp_data, calib_data);
N		}
N	} else {
N		rslt = BME280_E_NULL_PTR;
X		rslt = ((int_least8_t)(-1));
N	}
N
N	return rslt;
N}
N
N/*!
N * @brief This internal API sets the oversampling settings for pressure,
N * temperature and humidity in the sensor.
N */
Nstatic int8_t set_osr_settings(uint8_t desired_settings, const struct bme280_settings *settings,
N				const struct bme280_dev *dev)
N{
N	int8_t rslt = BME280_W_INVALID_OSR_MACRO;
X	int8_t rslt = ((int_least8_t)(1));
N
N	if (desired_settings & BME280_OSR_HUM_SEL)
X	if (desired_settings & ((uint_least8_t)(1 << 2)))
N		rslt = set_osr_humidity_settings(settings, dev);
N	if (desired_settings & (BME280_OSR_PRESS_SEL | BME280_OSR_TEMP_SEL))
X	if (desired_settings & (((uint_least8_t)(1)) | ((uint_least8_t)(1 << 1))))
N		rslt = set_osr_press_temp_settings(desired_settings, settings, dev);
N
N	return rslt;
N}
N
N/*!
N * @brief This API sets the humidity oversampling settings of the sensor.
N */
Nstatic int8_t set_osr_humidity_settings(const struct bme280_settings *settings, const struct bme280_dev *dev)
N{
N	int8_t rslt;
N	uint8_t ctrl_hum;
N	uint8_t ctrl_meas;
N	uint8_t reg_addr = BME280_CTRL_HUM_ADDR;
X	uint8_t reg_addr = ((uint_least8_t)(0xF2));
N
N	ctrl_hum = settings->osr_h & BME280_CTRL_HUM_MSK;
X	ctrl_hum = settings->osr_h & ((uint_least8_t)(0x07));
N	/* Write the humidity control value in the register */
N	rslt = bme280_set_regs(&reg_addr, &ctrl_hum, 1, dev);
N	/* Humidity related changes will be only effective after a
N	   write operation to ctrl_meas register */
N	if (rslt == BME280_OK) {
X	if (rslt == ((int_least8_t)(0))) {
N		reg_addr = BME280_CTRL_MEAS_ADDR;
X		reg_addr = ((uint_least8_t)(0xF4));
N		rslt = bme280_get_regs(reg_addr, &ctrl_meas, 1, dev);
N		if (rslt == BME280_OK)
X		if (rslt == ((int_least8_t)(0)))
N			rslt = bme280_set_regs(&reg_addr, &ctrl_meas, 1, dev);
N	}
N
N	return rslt;
R "../bme280_old.c" 709 11 (ULP 5.4) Detected an assignment to a type with size less than int. To avoid unnecessary sign extension, use int-sized types for local varaibles and convert to smaller types for static storage.
R "../bme280_old.c" 711 25 (ULP 5.4) Detected an assignment to a type with size less than int. To avoid unnecessary sign extension, use int-sized types for local varaibles and convert to smaller types for static storage.
R "../bme280_old.c" 711 36 (ULP 5.4) Detected an assignment to a type with size less than int. To avoid unnecessary sign extension, use int-sized types for local varaibles and convert to smaller types for static storage.
R "../bme280_old.c" 716 36 (ULP 5.4) Detected an assignment to a type with size less than int. To avoid unnecessary sign extension, use int-sized types for local varaibles and convert to smaller types for static storage.
R "../bme280_old.c" 718 27 (ULP 5.4) Detected an assignment to a type with size less than int. To avoid unnecessary sign extension, use int-sized types for local varaibles and convert to smaller types for static storage.
R "../bme280_old.c" 718 38 (ULP 5.4) Detected an assignment to a type with size less than int. To avoid unnecessary sign extension, use int-sized types for local varaibles and convert to smaller types for static storage.
N}
N
N/*!
N * @brief This API sets the pressure and/or temperature oversampling settings
N * in the sensor according to the settings selected by the user.
N */
Nstatic int8_t set_osr_press_temp_settings(uint8_t desired_settings, const struct bme280_settings *settings,
N						const struct bme280_dev *dev)
N{
N	int8_t rslt;
N	uint8_t reg_addr = BME280_CTRL_MEAS_ADDR;
X	uint8_t reg_addr = ((uint_least8_t)(0xF4));
N	uint8_t reg_data;
N
N	rslt = bme280_get_regs(reg_addr, &reg_data, 1, dev);
N
N	if (rslt == BME280_OK) {
X	if (rslt == ((int_least8_t)(0))) {
N		if (desired_settings & BME280_OSR_PRESS_SEL)
X		if (desired_settings & ((uint_least8_t)(1)))
N			fill_osr_press_settings(&reg_data, settings);
N		if (desired_settings & BME280_OSR_TEMP_SEL)
X		if (desired_settings & ((uint_least8_t)(1 << 1)))
N			fill_osr_temp_settings(&reg_data, settings);
N		/* Write the oversampling settings in the register */
N		rslt = bme280_set_regs(&reg_addr, &reg_data, 1, dev);
N	}
N
N	return rslt;
R "../bme280_old.c" 735 35 (ULP 5.4) Detected an assignment to a type with size less than int. To avoid unnecessary sign extension, use int-sized types for local varaibles and convert to smaller types for static storage.
R "../bme280_old.c" 739 28 (ULP 5.4) Detected an assignment to a type with size less than int. To avoid unnecessary sign extension, use int-sized types for local varaibles and convert to smaller types for static storage.
R "../bme280_old.c" 741 27 (ULP 5.4) Detected an assignment to a type with size less than int. To avoid unnecessary sign extension, use int-sized types for local varaibles and convert to smaller types for static storage.
R "../bme280_old.c" 743 26 (ULP 5.4) Detected an assignment to a type with size less than int. To avoid unnecessary sign extension, use int-sized types for local varaibles and convert to smaller types for static storage.
R "../bme280_old.c" 743 37 (ULP 5.4) Detected an assignment to a type with size less than int. To avoid unnecessary sign extension, use int-sized types for local varaibles and convert to smaller types for static storage.
N}
N
N/*!
N * @brief This internal API sets the filter and/or standby duration settings
N * in the sensor according to the settings selected by the user.
N */
Nstatic int8_t set_filter_standby_settings(uint8_t desired_settings, const struct bme280_settings *settings,
N						const struct bme280_dev *dev)
N{
N	int8_t rslt;
N	uint8_t reg_addr = BME280_CONFIG_ADDR;
X	uint8_t reg_addr = ((uint_least8_t)(0xF5));
N	uint8_t reg_data;
N
N	rslt = bme280_get_regs(reg_addr, &reg_data, 1, dev);
N
N	if (rslt == BME280_OK) {
X	if (rslt == ((int_least8_t)(0))) {
N		if (desired_settings & BME280_FILTER_SEL)
X		if (desired_settings & ((uint_least8_t)(1 << 3)))
N			fill_filter_settings(&reg_data, settings);
N		if (desired_settings & BME280_STANDBY_SEL)
X		if (desired_settings & ((uint_least8_t)(1 << 4)))
N			fill_standby_settings(&reg_data, settings);
N		/* Write the oversampling settings in the register */
N		rslt = bme280_set_regs(&reg_addr, &reg_data, 1, dev);
N	}
N
N	return rslt;
R "../bme280_old.c" 760 35 (ULP 5.4) Detected an assignment to a type with size less than int. To avoid unnecessary sign extension, use int-sized types for local varaibles and convert to smaller types for static storage.
R "../bme280_old.c" 764 25 (ULP 5.4) Detected an assignment to a type with size less than int. To avoid unnecessary sign extension, use int-sized types for local varaibles and convert to smaller types for static storage.
R "../bme280_old.c" 766 26 (ULP 5.4) Detected an assignment to a type with size less than int. To avoid unnecessary sign extension, use int-sized types for local varaibles and convert to smaller types for static storage.
R "../bme280_old.c" 768 26 (ULP 5.4) Detected an assignment to a type with size less than int. To avoid unnecessary sign extension, use int-sized types for local varaibles and convert to smaller types for static storage.
R "../bme280_old.c" 768 37 (ULP 5.4) Detected an assignment to a type with size less than int. To avoid unnecessary sign extension, use int-sized types for local varaibles and convert to smaller types for static storage.
N}
N
N/*!
N * @brief This internal API fills the filter settings provided by the user
N * in the data buffer so as to write in the sensor.
N */
Nstatic void fill_filter_settings(uint8_t *reg_data, const struct bme280_settings *settings)
N{
N	*reg_data = BME280_SET_BITS(*reg_data, BME280_FILTER, settings->filter);
X	*reg_data = ((*reg_data & ~(((uint_least8_t)(0x1C)))) | ((settings->filter << ((uint_least8_t)(0x02))) & ((uint_least8_t)(0x1C))));
N}
N
N/*!
N * @brief This internal API fills the standby duration settings provided by
N * the user in the data buffer so as to write in the sensor.
N */
Nstatic void fill_standby_settings(uint8_t *reg_data, const struct bme280_settings *settings)
N{
N	*reg_data = BME280_SET_BITS(*reg_data, BME280_STANDBY, settings->standby_time);
X	*reg_data = ((*reg_data & ~(((uint_least8_t)(0xE0)))) | ((settings->standby_time << ((uint_least8_t)(0x05))) & ((uint_least8_t)(0xE0))));
N}
N
N/*!
N * @brief This internal API fills the pressure oversampling settings provided by
N * the user in the data buffer so as to write in the sensor.
N */
Nstatic void fill_osr_press_settings(uint8_t *reg_data, const struct bme280_settings *settings)
N{
N	*reg_data = BME280_SET_BITS(*reg_data, BME280_CTRL_PRESS, settings->osr_p);
X	*reg_data = ((*reg_data & ~(((uint_least8_t)(0x1C)))) | ((settings->osr_p << ((uint_least8_t)(0x02))) & ((uint_least8_t)(0x1C))));
N}
N
N/*!
N * @brief This internal API fills the temperature oversampling settings
N * provided by the user in the data buffer so as to write in the sensor.
N */
Nstatic void fill_osr_temp_settings(uint8_t *reg_data, const struct bme280_settings *settings)
N{
N	*reg_data = BME280_SET_BITS(*reg_data, BME280_CTRL_TEMP, settings->osr_t);
X	*reg_data = ((*reg_data & ~(((uint_least8_t)(0xE0)))) | ((settings->osr_t << ((uint_least8_t)(0x05))) & ((uint_least8_t)(0xE0))));
N}
N
N/*!
N * @brief This internal API parse the oversampling(pressure, temperature
N * and humidity), filter and standby duration settings and store in the
N * device structure.
N */
Nstatic void parse_device_settings(const uint8_t *reg_data, struct bme280_settings *settings)
N{
N	settings->osr_h = BME280_GET_BITS_POS_0(reg_data[0], BME280_CTRL_HUM);
X	settings->osr_h = (reg_data[0] & (((uint_least8_t)(0x07))));
N	settings->osr_p = BME280_GET_BITS(reg_data[2], BME280_CTRL_PRESS);
X	settings->osr_p = ((reg_data[2] & (((uint_least8_t)(0x1C)))) >> (((uint_least8_t)(0x02))));
N	settings->osr_t = BME280_GET_BITS(reg_data[2], BME280_CTRL_TEMP);
X	settings->osr_t = ((reg_data[2] & (((uint_least8_t)(0xE0)))) >> (((uint_least8_t)(0x05))));
N	settings->filter = BME280_GET_BITS(reg_data[3], BME280_FILTER);
X	settings->filter = ((reg_data[3] & (((uint_least8_t)(0x1C)))) >> (((uint_least8_t)(0x02))));
N	settings->standby_time = BME280_GET_BITS(reg_data[3], BME280_STANDBY);
X	settings->standby_time = ((reg_data[3] & (((uint_least8_t)(0xE0)))) >> (((uint_least8_t)(0x05))));
N}
N/*!
N * @brief This internal API writes the power mode in the sensor.
N */
Nstatic int8_t write_power_mode(uint8_t sensor_mode, const struct bme280_dev *dev)
N{
N	int8_t rslt;
N	uint8_t reg_addr = BME280_PWR_CTRL_ADDR;
X	uint8_t reg_addr = ((uint_least8_t)(0xF4));
N	/* Variable to store the value read from power mode register */
N	uint8_t sensor_mode_reg_val;
N
N	/* Read the power mode register */
N	rslt = bme280_get_regs(reg_addr, &sensor_mode_reg_val, 1, dev);
N	/* Set the power mode */
N	if (rslt == BME280_OK) {
X	if (rslt == ((int_least8_t)(0))) {
N		sensor_mode_reg_val = BME280_SET_BITS_POS_0(sensor_mode_reg_val, BME280_SENSOR_MODE, sensor_mode);
X		sensor_mode_reg_val = ((sensor_mode_reg_val & ~(((uint_least8_t)(0x03)))) | (sensor_mode & ((uint_least8_t)(0x03))));
N		/* Write the power mode in the register */
N		rslt = bme280_set_regs(&reg_addr, &sensor_mode_reg_val, 1, dev);
N	}
N
N	return rslt;
R "../bme280_old.c" 834 35 (ULP 5.4) Detected an assignment to a type with size less than int. To avoid unnecessary sign extension, use int-sized types for local varaibles and convert to smaller types for static storage.
R "../bme280_old.c" 837 23 (ULP 5.4) Detected an assignment to a type with size less than int. To avoid unnecessary sign extension, use int-sized types for local varaibles and convert to smaller types for static storage.
R "../bme280_old.c" 839 26 (ULP 5.4) Detected an assignment to a type with size less than int. To avoid unnecessary sign extension, use int-sized types for local varaibles and convert to smaller types for static storage.
R "../bme280_old.c" 839 37 (ULP 5.4) Detected an assignment to a type with size less than int. To avoid unnecessary sign extension, use int-sized types for local varaibles and convert to smaller types for static storage.
N}
N
N/*!
N * @brief This internal API puts the device to sleep mode.
N */
Nstatic int8_t put_device_to_sleep(const struct bme280_dev *dev)
N{
N	int8_t rslt;
N	uint8_t reg_data[4];
N	struct bme280_settings settings;
N
N	rslt = bme280_get_regs(BME280_CTRL_HUM_ADDR, reg_data, 4, dev);
X	rslt = bme280_get_regs(((uint_least8_t)(0xF2)), reg_data, 4, dev);
N	if (rslt == BME280_OK) {
X	if (rslt == ((int_least8_t)(0))) {
N		parse_device_settings(reg_data, &settings);
N		rslt = bme280_soft_reset(dev);
N		if (rslt == BME280_OK)
X		if (rslt == ((int_least8_t)(0)))
N			rslt = reload_device_settings(&settings, dev);
N	}
N
N	return rslt;
N}
N
N/*!
N * @brief This internal API reloads the already existing device settings in
N * the sensor after soft reset.
N */
Nstatic int8_t reload_device_settings(const struct bme280_settings *settings, const struct bme280_dev *dev)
N{
N	int8_t rslt;
N
N	rslt = set_osr_settings(BME280_ALL_SETTINGS_SEL, settings, dev);
X	rslt = set_osr_settings(((uint_least8_t)(0x1F)), settings, dev);
N	if (rslt == BME280_OK)
X	if (rslt == ((int_least8_t)(0)))
N		rslt = set_filter_standby_settings(BME280_ALL_SETTINGS_SEL, settings, dev);
X		rslt = set_filter_standby_settings(((uint_least8_t)(0x1F)), settings, dev);
N
N	return rslt;
N}
N
N#ifdef BME280_FLOAT_ENABLE
S/*!
S * @brief This internal API is used to compensate the raw temperature data and
S * return the compensated temperature data in double data type.
S */
Sstatic double compensate_temperature(const struct bme280_uncomp_data *uncomp_data,
S						struct bme280_calib_data *calib_data)
S{
S	double var1;
S	double var2;
S	double temperature;
S	double temperature_min = -40;
S	double temperature_max = 85;
S
S	var1 = ((double)uncomp_data->temperature) / 16384.0 - ((double)calib_data->dig_T1) / 1024.0;
S	var1 = var1 * ((double)calib_data->dig_T2);
S	var2 = (((double)uncomp_data->temperature) / 131072.0 - ((double)calib_data->dig_T1) / 8192.0);
S	var2 = (var2 * var2) * ((double)calib_data->dig_T3);
S	calib_data->t_fine = (int32_t)(var1 + var2);
S	temperature = (var1 + var2) / 5120.0;
S
S	if (temperature < temperature_min)
S		temperature = temperature_min;
S	else if (temperature > temperature_max)
S		temperature = temperature_max;
S
S	return temperature;
S}
S
S/*!
S * @brief This internal API is used to compensate the raw pressure data and
S * return the compensated pressure data in double data type.
S */
Sstatic double compensate_pressure(const struct bme280_uncomp_data *uncomp_data,
S						const struct bme280_calib_data *calib_data)
S{
S	double var1;
S	double var2;
S	double var3;
S	double pressure;
S	double pressure_min = 30000.0;
S	double pressure_max = 110000.0;
S
S	var1 = ((double)calib_data->t_fine / 2.0) - 64000.0;
S	var2 = var1 * var1 * ((double)calib_data->dig_P6) / 32768.0;
S	var2 = var2 + var1 * ((double)calib_data->dig_P5) * 2.0;
S	var2 = (var2 / 4.0) + (((double)calib_data->dig_P4) * 65536.0);
S	var3 = ((double)calib_data->dig_P3) * var1 * var1 / 524288.0;
S	var1 = (var3 + ((double)calib_data->dig_P2) * var1) / 524288.0;
S	var1 = (1.0 + var1 / 32768.0) * ((double)calib_data->dig_P1);
S	/* avoid exception caused by division by zero */
S	if (var1) {
S		pressure = 1048576.0 - (double) uncomp_data->pressure;
S		pressure = (pressure - (var2 / 4096.0)) * 6250.0 / var1;
S		var1 = ((double)calib_data->dig_P9) * pressure * pressure / 2147483648.0;
S		var2 = pressure * ((double)calib_data->dig_P8) / 32768.0;
S		pressure = pressure + (var1 + var2 + ((double)calib_data->dig_P7)) / 16.0;
S
S		if (pressure < pressure_min)
S			pressure = pressure_min;
S		else if (pressure > pressure_max)
S			pressure = pressure_max;
S	} else { /* Invalid case */
S		pressure = pressure_min;
S	}
S
S	return pressure;
S}
S
S/*!
S * @brief This internal API is used to compensate the raw humidity data and
S * return the compensated humidity data in double data type.
S */
Sstatic double compensate_humidity(const struct bme280_uncomp_data *uncomp_data,
S						const struct bme280_calib_data *calib_data)
S{
S	double humidity;
S	double humidity_min = 0.0;
S	double humidity_max = 100.0;
S	double var1;
S	double var2;
S	double var3;
S	double var4;
S	double var5;
S	double var6;
S
S	var1 = ((double)calib_data->t_fine) - 76800.0;
S	var2 = (((double)calib_data->dig_H4) * 64.0 + (((double)calib_data->dig_H5) / 16384.0) * var1);
S	var3 = uncomp_data->humidity - var2;
S	var4 = ((double)calib_data->dig_H2) / 65536.0;
S	var5 = (1.0 + (((double)calib_data->dig_H3) / 67108864.0) * var1);
S	var6 = 1.0 + (((double)calib_data->dig_H6) / 67108864.0) * var1 * var5;
S	var6 = var3 * var4 * (var5 * var6);
S	humidity = var6 * (1.0 - ((double)calib_data->dig_H1) * var6 / 524288.0);
S
S	if (humidity > humidity_max)
S		humidity = humidity_max;
S	else if (humidity < humidity_min)
S		humidity = humidity_min;
S
S	return humidity;
S}
S
N#else
N/*!
N * @brief This internal API is used to compensate the raw temperature data and
N * return the compensated temperature data in integer data type.
N */
Nstatic int32_t compensate_temperature(const struct bme280_uncomp_data *uncomp_data,
N						struct bme280_calib_data *calib_data)
N{
N	int32_t var1;
N	int32_t var2;
N	int32_t temperature;
N	int32_t temperature_min = -4000;
N	int32_t temperature_max = 8500;
N
N	var1 = (int32_t)((uncomp_data->temperature / 8) - ((int32_t)calib_data->dig_T1 * 2));
N	var1 = (var1 * ((int32_t)calib_data->dig_T2)) / 2048;
N	var2 = (int32_t)((uncomp_data->temperature / 16) - ((int32_t)calib_data->dig_T1));
N	var2 = (((var2 * var2) / 4096) * ((int32_t)calib_data->dig_T3)) / 16384;
N	calib_data->t_fine = var1 + var2;
N	temperature = (calib_data->t_fine * 5 + 128) / 256;
N
N	if (temperature < temperature_min)
N		temperature = temperature_min;
N	else if (temperature > temperature_max)
N		temperature = temperature_max;
N
N	return temperature;
N}
N#ifdef BME280_64BIT_ENABLE
N/*!
N * @brief This internal API is used to compensate the raw pressure data and
N * return the compensated pressure data in integer data type with higher
N * accuracy.
N */
Nstatic uint32_t compensate_pressure(const struct bme280_uncomp_data *uncomp_data,
N						const struct bme280_calib_data *calib_data)
N{
N	int64_t var1;
N	int64_t var2;
N	int64_t var3;
N	int64_t var4;
N	uint32_t pressure;
N	uint32_t pressure_min = 3000000;
N	uint32_t pressure_max = 11000000;
N
N	var1 = ((int64_t)calib_data->t_fine) - 128000;
N	var2 = var1 * var1 * (int64_t)calib_data->dig_P6;
N	var2 = var2 + ((var1 * (int64_t)calib_data->dig_P5) * 131072);
N	var2 = var2 + (((int64_t)calib_data->dig_P4) * 34359738368);
N	var1 = ((var1 * var1 * (int64_t)calib_data->dig_P3) / 256) + ((var1 * ((int64_t)calib_data->dig_P2) * 4096));
N	var3 = ((int64_t)1) * 140737488355328;
N	var1 = (var3 + var1) * ((int64_t)calib_data->dig_P1) / 8589934592;
N
N	/* To avoid divide by zero exception */
N	if (var1 != 0) {
N		var4 = 1048576 - uncomp_data->pressure;
N		var4 = (((var4 * 2147483648) - var2) * 3125) / var1;
N		var1 = (((int64_t)calib_data->dig_P9) * (var4 / 8192) * (var4 / 8192)) / 33554432;
N		var2 = (((int64_t)calib_data->dig_P8) * var4) / 524288;
N		var4 = ((var4 + var1 + var2) / 256) + (((int64_t)calib_data->dig_P7) * 16);
N		pressure = (uint32_t)(((var4 / 2) * 100) / 128);
N
N		if (pressure < pressure_min)
N			pressure = pressure_min;
N		else if (pressure > pressure_max)
N			pressure = pressure_max;
N	} else {
N		pressure = pressure_min;
N	}
N
N	return pressure;
N}
N#else
S/*!
S * @brief This internal API is used to compensate the raw pressure data and
S * return the compensated pressure data in integer data type.
S */
Sstatic uint32_t compensate_pressure(const struct bme280_uncomp_data *uncomp_data,
S						const struct bme280_calib_data *calib_data)
S{
S	int32_t var1;
S	int32_t var2;
S	int32_t var3;
S	int32_t var4;
S	uint32_t var5;
S	uint32_t pressure;
S	uint32_t pressure_min = 30000;
S	uint32_t pressure_max = 110000;
S
S	var1 = (((int32_t)calib_data->t_fine) / 2) - (int32_t)64000;
S	var2 = (((var1 / 4) * (var1 / 4)) / 2048) * ((int32_t)calib_data->dig_P6);
S	var2 = var2 + ((var1 * ((int32_t)calib_data->dig_P5)) * 2);
S	var2 = (var2 / 4) + (((int32_t)calib_data->dig_P4) * 65536);
S	var3 = (calib_data->dig_P3 * (((var1 / 4) * (var1 / 4)) / 8192)) / 8;
S	var4 = (((int32_t)calib_data->dig_P2) * var1) / 2;
S	var1 = (var3 + var4) / 262144;
S	var1 = (((32768 + var1)) * ((int32_t)calib_data->dig_P1)) / 32768;
S	 /* avoid exception caused by division by zero */
S	if (var1) {
S		var5 = (uint32_t)((uint32_t)1048576) - uncomp_data->pressure;
S		pressure = ((uint32_t)(var5 - (uint32_t)(var2 / 4096))) * 3125;
S		if (pressure < 0x80000000)
S			pressure = (pressure << 1) / ((uint32_t)var1);
S		else
S			pressure = (pressure / (uint32_t)var1) * 2;
S
S		var1 = (((int32_t)calib_data->dig_P9) * ((int32_t)(((pressure / 8) * (pressure / 8)) / 8192))) / 4096;
S		var2 = (((int32_t)(pressure / 4)) * ((int32_t)calib_data->dig_P8)) / 8192;
S		pressure = (uint32_t)((int32_t)pressure + ((var1 + var2 + calib_data->dig_P7) / 16));
S
S		if (pressure < pressure_min)
S			pressure = pressure_min;
S		else if (pressure > pressure_max)
S			pressure = pressure_max;
S	} else {
S		pressure = pressure_min;
S	}
S
S	return pressure;
S}
N#endif
N
N/*!
N * @brief This internal API is used to compensate the raw humidity data and
N * return the compensated humidity data in integer data type.
N */
Nstatic uint32_t compensate_humidity(const struct bme280_uncomp_data *uncomp_data,
N						const struct bme280_calib_data *calib_data)
N{
N	int32_t var1;
N	int32_t var2;
N	int32_t var3;
N	int32_t var4;
N	int32_t var5;
N	uint32_t humidity;
N	uint32_t humidity_max = 102400;
N
N	var1 = calib_data->t_fine - ((int32_t)76800);
N	var2 = (int32_t)(uncomp_data->humidity * 16384);
N	var3 = (int32_t)(((int32_t)calib_data->dig_H4) * 1048576);
N	var4 = ((int32_t)calib_data->dig_H5) * var1;
N	var5 = (((var2 - var3) - var4) + (int32_t)16384) / 32768;
N	var2 = (var1 * ((int32_t)calib_data->dig_H6)) / 1024;
N	var3 = (var1 * ((int32_t)calib_data->dig_H3)) / 2048;
N	var4 = ((var2 * (var3 + (int32_t)32768)) / 1024) + (int32_t)2097152;
N	var2 = ((var4 * ((int32_t)calib_data->dig_H2)) + 8192) / 16384;
N	var3 = var5 * var2;
N	var4 = ((var3 / 32768) * (var3 / 32768)) / 128;
N	var5 = var3 - ((var4 * ((int32_t)calib_data->dig_H1)) / 16);
N	var5 = (var5 < 0 ? 0 : var5);
N	var5 = (var5 > 419430400 ? 419430400 : var5);
N	humidity = (uint32_t)(var5 / 4096);
N
N	if (humidity > humidity_max)
N		humidity = humidity_max;
N
N	return humidity;
N}
N#endif
N
N/*!
N * @brief This internal API reads the calibration data from the sensor, parse
N * it and store in the device structure.
N */
Nstatic int8_t get_calib_data(struct bme280_dev *dev)
N{
N	int8_t rslt;
N	uint8_t reg_addr = BME280_TEMP_PRESS_CALIB_DATA_ADDR;
X	uint8_t reg_addr = ((uint_least8_t)(0xFA));
N	/* Array to store calibration data */
N	uint8_t calib_data[BME280_TEMP_PRESS_CALIB_DATA_LEN] = {0};
X	uint8_t calib_data[((uint_least8_t)(26))] = {0};
N
N	/* Read the calibration data from the sensor */
N	rslt = bme280_get_regs(reg_addr, calib_data, BME280_TEMP_PRESS_CALIB_DATA_LEN, dev);
X	rslt = bme280_get_regs(reg_addr, calib_data, ((uint_least8_t)(26)), dev);
N
N	if (rslt == BME280_OK) {
X	if (rslt == ((int_least8_t)(0))) {
N		/* Parse temperature and pressure calibration data and store
N		   it in device structure */
N		parse_temp_press_calib_data(calib_data, dev);
N
N		reg_addr = BME280_HUMIDITY_CALIB_DATA_ADDR;
X		reg_addr = ((uint_least8_t)(0xFD));
N		/* Read the humidity calibration data from the sensor */
N		rslt = bme280_get_regs(reg_addr, calib_data, BME280_HUMIDITY_CALIB_DATA_LEN, dev);
X		rslt = bme280_get_regs(reg_addr, calib_data, ((uint_least8_t)(7)), dev);
N		if (rslt == BME280_OK) {
X		if (rslt == ((int_least8_t)(0))) {
N			/* Parse humidity calibration data and store it in
N			   device structure */
N			parse_humidity_calib_data(calib_data, dev);
N		}
N	}
N
N	return rslt;
N}
N
N/*!
N * @brief This internal API interleaves the register address between the
N * register data buffer for burst write operation.
N */
Nstatic void interleave_reg_addr(const uint8_t *reg_addr, uint8_t *temp_buff, const uint8_t *reg_data, uint8_t len)
N{
N	uint8_t index;
N
N	for (index = 1; index < len; index++) {
N		temp_buff[(index * 2) - 1] = reg_addr[index];
N		temp_buff[index * 2] = reg_data[index];
N	}
R "../bme280_old.c" 1183 36 (ULP 5.4) Detected an assignment to a type with size less than int. To avoid unnecessary sign extension, use int-sized types for local varaibles and convert to smaller types for static storage.
N}
N
N/*!
N *  @brief This internal API is used to parse the temperature and
N *  pressure calibration data and store it in device structure.
N */
Nstatic void parse_temp_press_calib_data(const uint8_t *reg_data, struct bme280_dev *dev)
N{
N	struct bme280_calib_data *calib_data = &dev->calib_data;
N
N	calib_data->dig_T1 = BME280_CONCAT_BYTES(reg_data[1], reg_data[0]);
X	calib_data->dig_T1 = (((uint16_t)reg_data[1] << 8) | (uint16_t)reg_data[0]);
N	calib_data->dig_T2 = (int16_t)BME280_CONCAT_BYTES(reg_data[3], reg_data[2]);
X	calib_data->dig_T2 = (int16_t)(((uint16_t)reg_data[3] << 8) | (uint16_t)reg_data[2]);
N	calib_data->dig_T3 = (int16_t)BME280_CONCAT_BYTES(reg_data[5], reg_data[4]);
X	calib_data->dig_T3 = (int16_t)(((uint16_t)reg_data[5] << 8) | (uint16_t)reg_data[4]);
N	calib_data->dig_P1 = BME280_CONCAT_BYTES(reg_data[7], reg_data[6]);
X	calib_data->dig_P1 = (((uint16_t)reg_data[7] << 8) | (uint16_t)reg_data[6]);
N	calib_data->dig_P2 = (int16_t)BME280_CONCAT_BYTES(reg_data[9], reg_data[8]);
X	calib_data->dig_P2 = (int16_t)(((uint16_t)reg_data[9] << 8) | (uint16_t)reg_data[8]);
N	calib_data->dig_P3 = (int16_t)BME280_CONCAT_BYTES(reg_data[11], reg_data[10]);
X	calib_data->dig_P3 = (int16_t)(((uint16_t)reg_data[11] << 8) | (uint16_t)reg_data[10]);
N	calib_data->dig_P4 = (int16_t)BME280_CONCAT_BYTES(reg_data[13], reg_data[12]);
X	calib_data->dig_P4 = (int16_t)(((uint16_t)reg_data[13] << 8) | (uint16_t)reg_data[12]);
N	calib_data->dig_P5 = (int16_t)BME280_CONCAT_BYTES(reg_data[15], reg_data[14]);
X	calib_data->dig_P5 = (int16_t)(((uint16_t)reg_data[15] << 8) | (uint16_t)reg_data[14]);
N	calib_data->dig_P6 = (int16_t)BME280_CONCAT_BYTES(reg_data[17], reg_data[16]);
X	calib_data->dig_P6 = (int16_t)(((uint16_t)reg_data[17] << 8) | (uint16_t)reg_data[16]);
N	calib_data->dig_P7 = (int16_t)BME280_CONCAT_BYTES(reg_data[19], reg_data[18]);
X	calib_data->dig_P7 = (int16_t)(((uint16_t)reg_data[19] << 8) | (uint16_t)reg_data[18]);
N	calib_data->dig_P8 = (int16_t)BME280_CONCAT_BYTES(reg_data[21], reg_data[20]);
X	calib_data->dig_P8 = (int16_t)(((uint16_t)reg_data[21] << 8) | (uint16_t)reg_data[20]);
N	calib_data->dig_P9 = (int16_t)BME280_CONCAT_BYTES(reg_data[23], reg_data[22]);
X	calib_data->dig_P9 = (int16_t)(((uint16_t)reg_data[23] << 8) | (uint16_t)reg_data[22]);
N	calib_data->dig_H1 = reg_data[25];
N
N}
N
N/*!
N *  @brief This internal API is used to parse the humidity calibration data
N *  and store it in device structure.
N */
Nstatic void parse_humidity_calib_data(const uint8_t *reg_data, struct bme280_dev *dev)
N{
N	struct bme280_calib_data *calib_data = &dev->calib_data;
N	int16_t dig_H4_lsb;
N	int16_t dig_H4_msb;
N	int16_t dig_H5_lsb;
N	int16_t dig_H5_msb;
N
N	calib_data->dig_H2 = (int16_t)BME280_CONCAT_BYTES(reg_data[1], reg_data[0]);
X	calib_data->dig_H2 = (int16_t)(((uint16_t)reg_data[1] << 8) | (uint16_t)reg_data[0]);
N	calib_data->dig_H3 = reg_data[2];
N
N	dig_H4_msb = (int16_t)(int8_t)reg_data[3] * 16;
N	dig_H4_lsb = (int16_t)(reg_data[4] & 0x0F);
N	calib_data->dig_H4 = dig_H4_msb | dig_H4_lsb;
N
N	dig_H5_msb = (int16_t)(int8_t)reg_data[5] * 16;
N	dig_H5_lsb = (int16_t)(reg_data[4] >> 4);
N	calib_data->dig_H5 = dig_H5_msb | dig_H5_lsb;
N	calib_data->dig_H6 = (int8_t)reg_data[6];
R "../bme280_old.c" 1228 13 (ULP 5.4) Detected an assignment to a type with size less than int. To avoid unnecessary sign extension, use int-sized types for local varaibles and convert to smaller types for static storage.
R "../bme280_old.c" 1229 13 (ULP 5.4) Detected an assignment to a type with size less than int. To avoid unnecessary sign extension, use int-sized types for local varaibles and convert to smaller types for static storage.
R "../bme280_old.c" 1232 13 (ULP 5.4) Detected an assignment to a type with size less than int. To avoid unnecessary sign extension, use int-sized types for local varaibles and convert to smaller types for static storage.
R "../bme280_old.c" 1233 13 (ULP 5.4) Detected an assignment to a type with size less than int. To avoid unnecessary sign extension, use int-sized types for local varaibles and convert to smaller types for static storage.
N}
N
N/*!
N * @brief This internal API is used to identify the settings which the user
N * wants to modify in the sensor.
N */
Nstatic uint8_t are_settings_changed(uint8_t sub_settings, uint8_t desired_settings)
N{
N	uint8_t settings_changed = FALSE;
X	uint8_t settings_changed = ((uint_least8_t)(0));
N
N	if (sub_settings & desired_settings) {
N		/* User wants to modify this particular settings */
N		settings_changed = TRUE;
X		settings_changed = ((uint_least8_t)(1));
N	} else {
N		/* User don't want to modify this particular settings */
N		settings_changed = FALSE;
X		settings_changed = ((uint_least8_t)(0));
N	}
N
N	return settings_changed;
N}
N
N/*!
N * @brief This internal API is used to validate the device structure pointer for
N * null conditions.
N */
Nstatic int8_t null_ptr_check(const struct bme280_dev *dev)
N{
N	int8_t rslt;
N
N	if ((dev == NULL) || (dev->read == NULL) || (dev->write == NULL) || (dev->delay_ms == NULL)) {
X	if ((dev == 0) || (dev->read == 0) || (dev->write == 0) || (dev->delay_ms == 0)) {
N		/* Device structure pointer is not valid */
N		rslt = BME280_E_NULL_PTR;
X		rslt = ((int_least8_t)(-1));
N	} else {
N		/* Device structure is fine */
N		rslt = BME280_OK;
X		rslt = ((int_least8_t)(0));
N	}
N
N	return rslt;
N}
