
/* Informix Change Data Capture - Andrew Ford 2018
 *
 * Modern versions of Informix include the ability to subscribe to events
 * like row insert, update, delete via the Change Data Capture API.
 *
 * The API is a stream of binary data encoded with events and event data and
 * therefore it isn't really accessible. Not everyone is going to sit down
 * and write an ESQL/C or Java application to decode a stream of bytes and
 * parse out the data within.
 *
 * That's where this module comes in. My hope is that this Python
 * implementation will let more people take advantage of this nice feature,
 * and I had never written a Python C extension before and wondered what it
 * was all about after being completely mystified by the InformixDB Python
 * module that I've used for many years.
 *
 * Anywhoozles, I hope you enjoy this and I hope to get the DECIMAL
 * memory leak figured out. Oh, I bet it has something to do with Python
 * having a lddecimal function and ESQL/C having an lddecimal function and
 * one is getting used when I'm expecting the other to be used.
 *
 */


#include <Python.h>
#include "structmember.h"

/* From the InformixDB module:
 * Python and Informix both have a datetime.h, the Informix header is
 * included above because it comes first in the include path. We manually
 * include the Python one here (needs a few preprocessor tricks...)
 */
#include "datetime.h"
#define DATETIME_INCLUDE datetime.h
#define PYDTINC2(f) #f
#define PYDTINC1(f) PYDTINC2(f)
#include PYDTINC1(PYTHON_INCLUDE/DATETIME_INCLUDE)

EXEC SQL INCLUDE sqltypes;
EXEC SQL INCLUDE sqlca;

#define DEFAULT_ID                 1
#define DEFAULT_TIMEOUT            60
#define DEFAULT_MAX_RECORDS        100
#define DEFAULT_SYSCDCDB           "syscdcv1"
#define CONNNAME_LEN               63
#define CONNSTRING_LEN             511
#define ERRSTR_LEN                 63

#define MIN_LO_BUFFER_SZ           65536
#define MAX_CDC_TABS               64
#define MAX_CDC_COLS               64
#define MAX_SQL_STMT_LEN           8191

#define PACKET_SCHEME              66

#define HEADER_SZ_OFFSET           0
#define PAYLOAD_SZ_OFFSET          (HEADER_SZ_OFFSET + 4)
#define PACKET_SCHEME_OFFSET       (PAYLOAD_SZ_OFFSET + 4)
#define RECORD_NUMBER_OFFSET       (PACKET_SCHEME_OFFSET + 4)
#define RECORD_HEADER_OFFSET       (RECORD_NUMBER_OFFSET + 4)
#define CHANGE_HEADER_SZ           20

/*
 * Informix CDC Record types
*/

#define CDC_REC_BEGINTX            1
#define CDC_REC_COMMTX             2
#define CDC_REC_RBTX               3
#define CDC_REC_INSERT             40
#define CDC_REC_DELETE             41
#define CDC_REC_UPDBEF             42
#define CDC_REC_UPDAFT             43
#define CDC_REC_DISCARD            62
#define CDC_REC_TRUNCATE           119
#define CDC_REC_TABSCHEM           200
#define CDC_REC_TIMEOUT            201
#define CDC_REC_ERROR              202

EXEC SQL define TABLENAME_LEN      (255 + 1 + 255 + 1 + 255 + 1 + 255);
EXEC SQL define COLARG_LEN         1024;
EXEC SQL define CDC_MAJ_VER        1;
EXEC SQL define CDC_MIN_VER        1;
EXEC SQL define FULLROWLOG_OFF     0;
EXEC SQL define FULLROWLOG_ON      1;

/*
 * Structs for maintaining copies of sqlda structs
*/

typedef struct {
    int col_type;
    int col_xid;
    int col_size;
    char *col_name;
} column_t;

typedef struct {
    char tabname[TABLENAME_LEN+1];
    int num_cols;
    int num_var_cols;
    column_t columns[MAX_CDC_COLS];
} table_t;

/*
 * Nice macros for setting python objects to default values when parsing
 * args. Not currently passing any python objects, but leaving if I do one day.
 * http://pythonextensionpatterns.readthedocs.io/en/latest/parsing_arguments.html
 */

#define PY_DEFAULT_ARGUMENT_INIT(name, value, ret) \
    PyObject *name = NULL; \
    static PyObject *default_##name = NULL; \
    if (! default_##name) { \
        default_##name = value; \
        if (! default_##name) { \
            PyErr_SetString(PyExc_RuntimeError, \
                            "Can not create default value for " #name); \
            return ret; \
        } \
    }

#define PY_DEFAULT_ARGUMENT_SET(name) if (! name) name = default_##name; \
    Py_INCREF(name)

/*
 * InformixCdc objects
 */

static PyObject *ErrorObject;

/*
 * The InformixCdcObject
*/

typedef struct {
    PyObject_HEAD
    int id;
    char name[CONNNAME_LEN+1];
    int is_connected;
    $integer session_id;
    char dbservername[256];
    int timeout;
    int max_records;
    char syscdcdb[256];
    int lo_read_sz;
    char *lo_buffer;
    char *next_record_start;
    int bytes_in_buffer;
    int endianness;
    int next_table_id;
    table_t tables[MAX_CDC_TABS];
    int use_savepoints;
    bigint last_seq_number;
} InformixCdcObject;

/*
 * Function prototypes for extracting data from CDC Records
*/

static int2 ld2(const char *p);
static int4 ld4(const char *p);
static bigint ld8(const char *p);
static double lddbl(const char *p, const int endianness);
static float ldfloat(const char *p, const int endianness);
static int get_platform_endianness(void);
static bigint InformixCdc_query_restart_seq_number(const InformixCdcObject *self);
static bigint InformixCdc_query_last_seq_number(const InformixCdcObject *self);

static PyTypeObject InformixCdc_Type;

#define InformixCdcObject_Check(v)      (Py_TYPE(v) == &InformixCdc_Type)

static int
mock_ifx_lo_read(bigint sess, char *buf, int read_sz, int *err)
{
    static FILE* f = NULL;
    char infile[200] = "./tests/data/ifx_lo_read.in";
    int ret = 0;

    if (f == NULL) {
        f = fopen(infile, "rb");
        if (f == NULL) {
            printf("can't open mock ifx_lo_read file: %s\n", infile);
            return -1;
        }
        printf("using mock_ifx_lo_read");
    }

    *err = 0;
    ret = fread(buf, 1, read_sz, f);
    if (ret == 0) {
        fseek(f, 511, SEEK_SET);
        ret = fread(buf, 1, read_sz, f);
    }
    return ret;
}

#ifdef OWRITESBLOB
static int
write_testing_sblob(char *buf, int sz)
{
    static FILE* f = NULL;
    char outfile[200] = "./tests/data/ifx_lo_read.in";

    if (f == NULL) {
        f = fopen(outfile, "wb");
        if (f == NULL) {
            printf("can't open mock ifx_lo_read file: %s\n", outfile);
            return -1; }
    }

    return fwrite(buf, 1, sz, f);
}
#endif

static void
InformixCdc_dealloc(InformixCdcObject* self)
{
    int tabid;
    int col;

    PyMem_Free(self->lo_buffer);

    for (tabid=0; tabid < self->next_table_id; tabid++) {
        for (col=0; col < self->tables[tabid].num_cols; col++) {
            PyMem_Free(self->tables[tabid].columns[col].col_name);
        }
    }

    if (self->use_savepoints) {
        EXEC SQL FREE informixcdc_opntxns_ins;
        EXEC SQL FREE informixcdc_opntxns_upd;
        EXEC SQL FREE informixcdc_opntxns_del;
    }

    Py_TYPE(self)->tp_free((PyObject*)self);
}

static PyObject *
InformixCdc_new(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
    InformixCdcObject *self = NULL;

    self = (InformixCdcObject *)type->tp_alloc(type, 0);
    if (self == NULL) {
        goto except;
    }

    self->id = -1;
    self->name[0] = '\0';
    self->is_connected = -1;
    self->session_id = -1;
    self->dbservername[0] = '\0';
    self->timeout = -1;
    self->max_records = -1;
    self->syscdcdb[0] = '\0';
    self->lo_read_sz = -1;
    self->lo_buffer = NULL;
    self->next_record_start = NULL;
    self->bytes_in_buffer = -1;
    self->endianness = -1;
    self->next_table_id = -1;
    self->use_savepoints = -1;
    self->last_seq_number = -1;

    assert(! PyErr_Occurred());
    assert(self);
    goto finally;
except:
    assert(PyErr_Occurred());
    Py_XDECREF(self);
    self = NULL;
finally:
    return (PyObject *)self;
}

static int
InformixCdc_init(InformixCdcObject *self, PyObject *args, PyObject *kwds)
{
    int ret = -1;
    const char *dbservername = NULL;
    const char *syscdcdb = NULL;
    char err_str[ERRSTR_LEN+1];
    int lo_buffer_sz = MIN_LO_BUFFER_SZ / 1024;
    PyObject *py_use_savepoints = Py_True;
    Py_INCREF(py_use_savepoints);


    // if __init__ is called more than once, just noop
    if (self->lo_buffer) {
        return 0;
    }

    self->id = DEFAULT_ID;
    self->timeout = DEFAULT_TIMEOUT;
    self->max_records = DEFAULT_MAX_RECORDS;
    self->last_seq_number = 0;

    static const char *kwlist[] = { "dbservername", "id", "timeout",
                                    "max_records", "syscdcdb", "lo_buffer_sz",
                                    "use_savepoints", NULL };
    if (! PyArg_ParseTupleAndKeywords(args, kwds, "s|iiisiO!:init",
                                      (char**)(kwlist), &dbservername, &self->id,
                                      &self->timeout, &self->max_records,
                                      &syscdcdb, &lo_buffer_sz,
                                      &PyBool_Type, &py_use_savepoints)) {
        return -1;
    }
    sprintf(self->name, "cdc%p", self);
    strncpy(self->dbservername, dbservername, sizeof(self->dbservername));
    if (self->id <= 0 || self->id > 32767) {
        PyErr_SetString(PyExc_ValueError, "id must be between 1 and 32767");
        goto except;
    }
    if (self->timeout < 0) {
        PyErr_SetString(PyExc_ValueError, "timeout must be greater than 0");
        goto except;
    }
    if (self->max_records <= 0) {
        PyErr_SetString(PyExc_ValueError, "max_records must be greater than 0");
        goto except;
    }
    if (syscdcdb) {
        strncpy(self->syscdcdb, syscdcdb, sizeof(self->syscdcdb));
    }
    else {
        strcpy(self->syscdcdb, DEFAULT_SYSCDCDB);
    }
    lo_buffer_sz *= 1024; // lo_buffer_sz is in bytes from here on out
    if (lo_buffer_sz < MIN_LO_BUFFER_SZ) {
        snprintf(err_str, sizeof(err_str),
                 "lo_buffer_sz must be at least %dK", MIN_LO_BUFFER_SZ / 1024);
        PyErr_SetString(PyExc_ValueError, err_str);
        goto except;
    }
    self->lo_read_sz = lo_buffer_sz / 2;
    self->lo_buffer = PyMem_Malloc(lo_buffer_sz);
    if (! self->lo_buffer) {
        PyErr_SetString(PyExc_MemoryError, "cannot allocate lo_buffer");
        goto except;
    }
    self->is_connected = 0;
    self->next_record_start = self->lo_buffer;
    self->bytes_in_buffer = 0;
    self->endianness = get_platform_endianness();
    self->next_table_id = 0;
    self->use_savepoints = py_use_savepoints == Py_True ? 1 : 0;

    assert(! PyErr_Occurred());
    assert(py_use_savepoints);
    Py_DECREF(py_use_savepoints);
    ret = 0;
    goto finally;
except:
    assert(PyErr_Occurred());
    Py_XDECREF(py_use_savepoints);
    ret = -1;
finally:
    return ret;
}

static PyMemberDef InformixCdc_members[] = {
    {
        "dbservername",
        T_STRING,
        offsetof(InformixCdcObject, dbservername),
        READONLY,
        PyDoc_STR("dbservername of Informix perform CDC against"),
    },
    {
        "timeout",
        T_INT,
        offsetof(InformixCdcObject, timeout),
        READONLY,
        PyDoc_STR("timeout in seconds to block while waiting for CDC events"),
    },
    {
        "max_records",
        T_INT,
        offsetof(InformixCdcObject, max_records),
        READONLY,
        PyDoc_STR("max records CDC API can return in one event message"),
    },
    {
        "syscdcdb",
        T_STRING,
        offsetof(InformixCdcObject, syscdcdb),
        READONLY,
        PyDoc_STR("syscdc database name"),
    },
    {
        "session_id",
        T_INT,
        offsetof(InformixCdcObject, session_id),
        READONLY,
        PyDoc_STR("session id returned by Informix for CDC API"),
    },
    {
        NULL
    }
};

static PyObject *
InformixCdc_getis_connected(InformixCdcObject *self, void *closure)
{
    PyObject *is_connected = NULL;

    is_connected = PyBool_FromLong(self->is_connected);
    if (is_connected == NULL) {
        goto except;
    }

    assert(! PyErr_Occurred());
    assert(is_connected);
    goto finally;
except:
    assert(PyErr_Occurred());
    Py_XDECREF(is_connected);
    is_connected = NULL;
finally:
    return is_connected;
}

static PyGetSetDef InformixCdc_getseters[] = {
    {
        "is_connected",
        (getter)InformixCdc_getis_connected,
        NULL,
        PyDoc_STR("status of connection to CDC database server"),
        NULL
    },
    {
        NULL
    }
};

/*
 * private helper functions that do all of the heavy lifting
 */

#define INT8_LO_OFFSET          2
#define INT8_HI_OFFSET          6
#define BOOL_COL_LEN            2
#define VARCHAR_LEN_OFFSET      1
#define LVARCHAR_LEN_OFFSET     3

static PyObject*
InformixCdc_PyString_FromTabid(const InformixCdcObject *self, const int tabid)
{
    PyObject *py_string = NULL;

    if (tabid >= self->next_table_id) {
        PyErr_SetString(PyExc_IndexError, "invalid internel CDC table id");
        goto except;
    }

    py_string = PyString_FromString(self->tables[tabid].tabname);
    if (py_string == NULL) {
        PyErr_SetString(PyExc_IndexError, "PyString_FromString failed to create tabname");
        goto except;
    }

    assert(! PyErr_Occurred());
    assert(py_string);
    goto finally;
except:
    assert(PyErr_Occurred());
    py_string = NULL;
finally:
    return py_string;
}

static int
InformixCdc_set_connection(const InformixCdcObject *self)
{
    int rc = -1;
    EXEC SQL BEGIN DECLARE SECTION;
    const char *conn_name = self->name;
    EXEC SQL END DECLARE SECTION;

    EXEC SQL SET CONNECTION :conn_name;

    if (SQLCODE != 0) {
        PyErr_SetString(PyExc_RuntimeError, "cannot set connection");
        goto except;
    }
    assert(! PyErr_Occurred());
    rc = 0;
    goto finally;
except:
    assert(PyErr_Occurred());
    rc = -1;
finally:
    return rc;
}

/*
 * Threading support based largely on the informixdb module
 * http://informixdb.sourceforge.net/
 */
#ifdef IFXDB_MT
#define TH_STATE_DICT_KEY "informixcdc_dbconn_key"

static char*
InformixCdc_th_get_dbconn(void)
{
    PyObject *py_dict = NULL;
    PyObject *py_dbconn = NULL;
    char* dbconn = NULL;

    py_dict = PyThreadState_GetDict();
    if (py_dict == NULL) {
            goto except;
    }
    py_dbconn = PyDict_GetItemString(py_dict, TH_STATE_DICT_KEY);
    if (py_dbconn == NULL) {
            goto except;
    }
    dbconn = PyString_AS_STRING(py_dbconn);
    if (! dbconn) {
            goto except;
    }
    assert(! PyErr_Occurred());
    goto finally;
except:
    assert(PyErr_Occurred());
    dbconn = NULL;
finally:
    return dbconn;
}

static void
InformixCdc_th_set_dbconn(const char* dbconn)
{
    PyObject *py_dict = NULL;
    PyObject *py_dbconn = NULL;

    py_dict = PyThreadState_GetDict();
    if (py_dict == NULL) {
            goto except;
    }
    if (name != NULL) {
        py_dbconn = PyString_FromString(dbconn);
        if (py_dbconn == NULL) {
            goto except;
        }
        if (PyDict_SetItemString(py_dict, TH_STATE_DICT_KEY, py_dbconn) != 0)
        {
            goto except;
        }
    }
    assert(! PyErr_Occurred());
    assert(py_dbconn);
    Py_DECREF(py_dbconn);
    goto finally;
except:
    assert(PyErr_Occurred());
    Py_XDECREF(py_dbconn);
finally:
    return;
}

#undef TH_STATE_DICT_KEY
#else

static char* informixcdc_dbconn = NULL;
#define InformixCdc_th_get_dbconn() (informixcdc_dbconn)
#define InformixCdc_th_set_dbconn(s) (informixcdc_dbconn = (s))

#endif // IFXDB_MT

#ifdef NDEBUG
#define IFX_LO_READ ifx_lo_read
#else
#define IFX_LO_READ mock_ifx_lo_read
#endif

static bigint
InformixCdc_query_restart_seq_number(const InformixCdcObject *self)
{
    EXEC SQL BEGIN DECLARE SECTION;
    int id = self->id;
    bigint restart_seq_number = -1;
    EXEC SQL END DECLARE SECTION;
    char err_str[ERRSTR_LEN+1];

    EXEC SQL PREPARE restart_seq_number FROM
        "select nvl(min(seq_number), 0) from informixcdc_opntxns where id = ?";

    if (SQLCODE != 0) {
        snprintf(err_str, sizeof(err_str),
                 "cannot prepare restart_seq_number SQLCODE %d", SQLCODE);
        PyErr_SetString(PyExc_IndexError, err_str);
        goto except;
    }

    EXEC SQL EXECUTE restart_seq_number
             INTO :restart_seq_number USING :id;

    if (SQLCODE != 0) {
        snprintf(err_str, sizeof(err_str),
                 "cannot execute restart_seq_number SQLCODE %d", SQLCODE);
        PyErr_SetString(PyExc_IndexError, err_str);
        goto except;
    }
    assert(! PyErr_Occurred());
    assert(restart_seq_number >= 0);
    assert(SQLCODE == 0);
    goto finally;
except:
    assert(PyErr_Occurred());
    restart_seq_number = -1;
finally:
    EXEC SQL FREE restart_seq_number;
    return restart_seq_number;
}

static bigint
InformixCdc_query_last_seq_number(const InformixCdcObject *self)
{
    EXEC SQL BEGIN DECLARE SECTION;
    int id = self->id;
    bigint last_seq_number = -1;
    EXEC SQL END DECLARE SECTION;
    char err_str[ERRSTR_LEN+1];

    EXEC SQL PREPARE last_seq_number FROM
        "select seq_number from informixcdc_lsttxn where id = ?";

    if (SQLCODE != 0) {
        snprintf(err_str, sizeof(err_str),
                 "cannot prepare last_seq_number SQLCODE %d", SQLCODE);
        PyErr_SetString(PyExc_IndexError, err_str);
        goto except;
    }

    EXEC SQL EXECUTE last_seq_number INTO :last_seq_number USING :id;

    if (SQLCODE == SQLNOTFOUND) {
        last_seq_number = 0;
    }
    else if (SQLCODE != 0) {
        snprintf(err_str, sizeof(err_str),
                 "cannot execute last_seq_number SQLCODE %d", SQLCODE);
        PyErr_SetString(PyExc_IndexError, err_str);
        goto except;
    }
    assert(! PyErr_Occurred());
    assert(last_seq_number >= 0);
    assert(SQLCODE == 0);
    goto finally;
except:
    assert(PyErr_Occurred());
    last_seq_number = -1;
finally:
    EXEC SQL FREE last_seq_number;
    return last_seq_number;
}

static int
InformixCdc_upsert_opntxns(const InformixCdcObject *self)
{
    int ret = -1;
    char *rec = self->next_record_start+RECORD_HEADER_OFFSET;
    char err_str[ERRSTR_LEN+1];
    EXEC SQL BEGIN DECLARE SECTION;
    int id = self->id;
    bigint seq_number;
    int transaction_id;
    EXEC SQL END DECLARE SECTION;

    seq_number = ld8(rec);
    transaction_id = ld4(rec+8);

    EXEC SQL EXECUTE informixcdc_opntxns_ins
             USING :id, :transaction_id, :seq_number;

    if (SQLCODE == -239 || SQLCODE == -268) {
        EXEC SQL EXECUTE informixcdc_opntxns_upd
                 USING :seq_number, :id, :transaction_id;
    }
    if (SQLCODE != 0) {
        snprintf(err_str, sizeof(err_str),
                 "cannot upsert informixcdc_opntxns SQLCODE %d", SQLCODE);
        PyErr_SetString(PyExc_IndexError, err_str);
        goto except;
    }
    assert(! PyErr_Occurred());
    assert(SQLCODE == 0);
    ret = 0;
    goto finally;
except:
    assert(PyErr_Occurred());
    ret = -1;
finally:
    return ret;
}

static int
InformixCdc_delete_opntxns(const InformixCdcObject *self)
{
    int ret = -1;
    char err_str[ERRSTR_LEN+1];
    char *rec = self->next_record_start+RECORD_HEADER_OFFSET;
    EXEC SQL BEGIN DECLARE SECTION;
    int id = self->id;
    int transaction_id;
    EXEC SQL END DECLARE SECTION;

    transaction_id = ld4(rec+8);

    EXEC SQL EXECUTE informixcdc_opntxns_del
             USING :id, :transaction_id;

    if (SQLCODE != 0) {
        snprintf(err_str, sizeof(err_str),
                 "cannot delete informixcdc_opntxns SQLCODE %d", SQLCODE);
        PyErr_SetString(PyExc_IndexError, err_str);
        goto except;
    }
    assert(! PyErr_Occurred());
    assert(SQLCODE == 0);
    ret = 0;
    goto finally;
except:
    assert(PyErr_Occurred());
    ret = -1;
finally:
    return ret;
}

static int
InformixCdc_upsert_lsttxn(const InformixCdcObject *self)
{
    int ret = -1;
    char *rec = self->next_record_start+RECORD_HEADER_OFFSET;
    char err_str[ERRSTR_LEN+1];
    EXEC SQL BEGIN DECLARE SECTION;
    int id = self->id;
    bigint seq_number;
    EXEC SQL END DECLARE SECTION;

    seq_number = ld8(rec);
    if (seq_number > self->last_seq_number) {
        EXEC SQL EXECUTE informixcdc_lsttxn_upd
                 USING :seq_number, :id;

        if (sqlca.sqlerrd[2] == 0) {
            EXEC SQL EXECUTE informixcdc_lsttxn_ins
                     USING :id, :seq_number;
        }
        if (SQLCODE != 0) {
            snprintf(err_str, sizeof(err_str),
                     "cannot upsert informixcdc_lsttxn SQLCODE %d", SQLCODE);
            PyErr_SetString(PyExc_IndexError, err_str);
            goto except;
        }
    }
    assert(! PyErr_Occurred());
    assert(SQLCODE == 0);
    ret = seq_number;
    goto finally;
except:
    assert(PyErr_Occurred());
    ret = -1;
finally:
    return ret;
}

/* get rid of this function. it is useless */
static void
InformixCdc_extract_header(const char *record, int *header_sz, int *payload_sz,
                           int *packet_scheme, int *record_number) {
    *header_sz = ld4((char*)record + HEADER_SZ_OFFSET);
    *payload_sz = ld4((char*)record + PAYLOAD_SZ_OFFSET);
    *packet_scheme = ld4((char*)record + PACKET_SCHEME_OFFSET);
    *record_number = ld4((char*)record + RECORD_NUMBER_OFFSET);
}

static PyObject *
InformixCdc_extract_column_to_dict(const InformixCdcObject *self,
                                   const column_t *column, const char* col,
                                   const char* varchar_len_arr,
                                   int *varchar_len_arr_idx, int *advance_col)
{
    char ch_int8[21];
    char ch_decimal[35];
    short c_smallint;
    int c_integer;
    float c_float;
    double c_double;
    dec_t c_decimal;
    dtime_t c_datetime;
    int varchar_len;
    int col_len;
    bigint c_bigint;
    ifx_int8_t c_int8;
    short mdy_date[3];
    int rc;
    char err_str[ERRSTR_LEN+1];
    PyObject *py_dict = NULL;
    PyObject *py_name = NULL;
    PyObject *py_value = NULL;

    py_dict = PyDict_New();
    if (py_dict == NULL) {
        goto except;
    }

    // there is a memory leak in here...somewhere
    *advance_col = 0;
    switch (MASKNONULL(column->col_type)) {
        case SQLINT8:
        case SQLSERIAL8:
            // IS THIS THE RIGHT WAY?
            c_int8.sign = ld2(col);
            c_int8.data[0] = ld4(col+INT8_LO_OFFSET);
            c_int8.data[1] = ld4(col+INT8_HI_OFFSET);

            if (risnull(CINT8TYPE, (char*)&c_int8)) {
                py_value = Py_None;
                Py_INCREF(py_value);
            }
            else {
                rc = ifx_int8toasc(&c_int8, ch_int8, 20);
                if (rc != 0) {
                    PyErr_SetString(PyExc_ValueError,
                                    "cannot convert INT8 to ascii");
                    goto except;
                }
                ch_int8[sizeof(ch_int8)-1] = '\0';
                py_value = PyLong_FromString(ch_int8, NULL, 10);
                if (py_value == NULL) {
                    snprintf(err_str, sizeof(err_str),
                             "INT8: PyLong_FromString failed: %s", ch_int8);
                    PyErr_SetString(PyExc_ValueError, err_str);
                    goto except;
                }
            }
            break;

        case SQLSERIAL:
        case SQLINT:
            c_integer = ld4(col);
            if (risnull(CINTTYPE, (char*)&c_integer)) {
                py_value = Py_None;
                Py_INCREF(py_value);
            }
            else {
                py_value = PyInt_FromLong(c_integer);
                if (py_value == NULL) {
                    snprintf(err_str, sizeof(err_str),
                             "INT: PyInt_FromLong failed: %d", c_integer);
                    PyErr_SetString(PyExc_ValueError, err_str);
                    goto except;
                }
            }
            break;

        case SQLDATE:
            c_integer = ld4(col);
            if (risnull(CINTTYPE, (char*)&c_integer)) {
                py_value = Py_None;
                Py_INCREF(py_value);
            }
            else {
                rc = rjulmdy(c_integer, mdy_date);
                if (rc < 0) {
                    snprintf(err_str, sizeof(err_str),
                             "DATE: rjulmdy failed: %d", c_integer);
                    PyErr_SetString(PyExc_ValueError, err_str);
                    goto except;
                }
                py_value = PyDate_FromDate(
                    mdy_date[2], mdy_date[0], mdy_date[1]);
                if (py_value == NULL) {
                    snprintf(err_str, sizeof(err_str),
                             "DATE: PyDate_FromDate failed: %d", c_integer);
                    PyErr_SetString(PyExc_ValueError, err_str);
                    goto except;
                }
            }
            break;

        case SQLBOOL:
            if (*col == 1) {
                py_value = Py_None;
            }
            else {
                py_value = *(col+1) ? Py_True : Py_False;
            }
            Py_INCREF(py_value);
            *advance_col = BOOL_COL_LEN;
            break;

        case SQLCHAR:
            if (risnull(CCHARTYPE, (char*)col)) {
                py_value = Py_None;
                Py_INCREF(py_value);
            }
            else {
                py_value = PyString_FromStringAndSize(col, column->col_size);
                if (py_value == NULL) {
                    snprintf(err_str, sizeof(err_str),
                             "CHAR: PyString_FromStringAndSize failed");
                    PyErr_SetString(PyExc_ValueError, err_str);
                    goto except;
                }
            }
            break;

        case SQLNVCHAR:
        case SQLVCHAR:
            varchar_len = ld4(varchar_len_arr + 4 * (*varchar_len_arr_idx)++);
            col_len = varchar_len - VARCHAR_LEN_OFFSET;
            col += VARCHAR_LEN_OFFSET;
            if (risnull(CVCHARTYPE, (char *)col)) {
                py_value = Py_None;
                Py_INCREF(py_value);
            }
            else {
                py_value = PyString_FromStringAndSize(col, col_len);
                if (py_value == NULL) {
                    snprintf(err_str, sizeof(err_str),
                             "VARCHAR: PyString_FromStringAndSize failed");
                    PyErr_SetString(PyExc_ValueError, err_str);
                    goto except;
                }
            }
            *advance_col = VARCHAR_LEN_OFFSET + col_len;
            break;

        case SQLLVARCHAR:
            varchar_len = ld4(varchar_len_arr + 4 * (*varchar_len_arr_idx)++);
            col_len = varchar_len - LVARCHAR_LEN_OFFSET;
            col += LVARCHAR_LEN_OFFSET;
            if (risnull(CLVCHARTYPE, (char *)col)) {
                py_value = Py_None;
                Py_INCREF(py_value);
            }
            else {
                py_value = PyString_FromStringAndSize(col, col_len);
                if (py_value == NULL) {
                    snprintf(err_str, sizeof(err_str),
                             "LVARCHAR: PyString_FromStringAndSize failed");
                    PyErr_SetString(PyExc_ValueError, err_str);
                    goto except;
                }
            }
            *advance_col = LVARCHAR_LEN_OFFSET + col_len;
            break;

        case SQLINFXBIGINT:
            c_bigint = ld8(col);
            if (risnull(CBIGINTTYPE, (char*)&c_bigint)) {
                py_value = Py_None;
                Py_INCREF(py_value);
            }
            else {
                py_value = PyLong_FromLong(c_bigint);
                if (py_value == NULL) {
                    snprintf(err_str, sizeof(err_str),
                             "BIGINT: PyLong_FromLong failed: %ld", c_bigint);
                    PyErr_SetString(PyExc_ValueError, err_str);
                    goto except;
                }
            }
            break;

        case SQLFLOAT :
            c_double = lddbl(col, self->endianness);
            if (risnull(CDOUBLETYPE, (char*)&c_double)) {
                py_value = Py_None;
                Py_INCREF(py_value);
            }
            else {
                py_value = PyFloat_FromDouble(c_double);
                if (py_value == NULL) {
                    snprintf(err_str, sizeof(err_str),
                             "FLOAT: PyFloat_FromDouble failed: %lf", c_double);
                    PyErr_SetString(PyExc_ValueError, err_str);
                    goto except;
                }
            }
            break;

        case SQLSMFLOAT:
            c_float = ldfloat(col, self->endianness);
            if (risnull(CFLOATTYPE, (char*)&c_float)) {
                py_value = Py_None;
                Py_INCREF(py_value);
            }
            else {
                py_value = PyFloat_FromDouble(c_float);
                if (py_value == NULL) {
                    snprintf(err_str, sizeof(err_str),
                             "SMFLOAT: PyFloat_FromDouble failed: %f", c_float);
                    PyErr_SetString(PyExc_ValueError, err_str);
                    goto except;
                }
            }
            break;

        case SQLSMINT:
            c_smallint = ld2(col);
            if (risnull(CSHORTTYPE, (char*)&c_smallint)) {
                py_value = Py_None;
                Py_INCREF(py_value);
            }
            else {
                py_value = PyInt_FromLong(c_smallint);
                if (py_value == NULL) {
                    snprintf(err_str, sizeof(err_str),
                             "SMINT: PyInt_FromLong failed: %hd", c_smallint);
                    PyErr_SetString(PyExc_ValueError, err_str);
                    goto except;
                }
            }
            break;

        /*
         * lddecimal most likely causes a memory leak, probably because it
         * is defined in Python and ESQL/C and we're calling the Python
         * verion.
         */
        case SQLMONEY:
        case SQLDECIMAL:
            // disable decimals
            py_value = PyString_FromString("0.0");
            if (py_value == NULL) {
                snprintf(err_str, sizeof(err_str),
                         "DECIMAL: PyString_FromString failed: %s",
                         ch_decimal);
                PyErr_SetString(PyExc_ValueError, err_str);
                goto except;
            }
            break;

            // I can't find the header file that contains lddecimal
            lddecimal(col, column->col_size, &c_decimal);
            if (risnull(CDECIMALTYPE, (char*)&c_decimal)) {
                py_value = Py_None;
                Py_INCREF(py_value);
            }
            else {
                rc = dectoasc(&c_decimal, ch_decimal, sizeof(ch_decimal)-1,
                              column->col_size);
                if (rc != 0) {
                    PyErr_SetString(PyExc_ValueError,
                                    "cannot extract decimal");
                    goto except;
                }
                ch_decimal[sizeof(ch_decimal)-1] = '\0';
                py_value = PyString_FromString(ch_decimal);
                if (py_value == NULL) {
                    snprintf(err_str, sizeof(err_str),
                             "DECIMAL: PyString_FromString failed: %s",
                             ch_decimal);
                    PyErr_SetString(PyExc_ValueError, err_str);
                    goto except;
                }
            }
            break;

        /*
         * lddecimal most likely causes a memory leak, probably because it
         * is defined in Python and ESQL/C and we're calling the Python
         * verion.
         */
        case SQLDTIME:
        case SQLINTERVAL:
            // disable decimals
            py_value = PyString_FromString("0.0");
            if (py_value == NULL) {
                snprintf(err_str, sizeof(err_str),
                         "DECIMAL: PyString_FromString failed: %s",
                         ch_decimal);
                PyErr_SetString(PyExc_ValueError, err_str);
                goto except;
            }
            break;
            
            lddecimal(col, column->col_size, &(c_datetime.dt_dec));
            if (risnull(CDTIMETYPE, (char*)&(c_datetime.dt_dec))) {
                py_value = Py_None;
                Py_INCREF(py_value);
            }
            else {
                char ch_year[5], ch_month[3], ch_day[3];
                char ch_hour[3], ch_min[3], ch_sec[3], ch_usec[11];
                char *end;
                rc = dectoasc(&c_datetime.dt_dec, ch_decimal, 34,
                              column->col_size);
                if (rc != 0) {
                    PyErr_SetString(PyExc_ValueError,
                                    "cannot extract datetime");
                    goto except;
                }
                ch_decimal[sizeof(ch_decimal)-1] = '\0';
                strncpy(ch_year,  ch_decimal,     4); ch_year[4]  = '\0';
                strncpy(ch_month, ch_decimal+4,   2); ch_month[2] = '\0';
                strncpy(ch_day,   ch_decimal+6,   2); ch_day[2]   = '\0';
                strncpy(ch_hour,  ch_decimal+8,   2); ch_hour[2]  = '\0';
                strncpy(ch_min,   ch_decimal+10,  2); ch_min[2]   = '\0';
                strncpy(ch_sec,   ch_decimal+12,  2); ch_sec[2]   = '\0';
                strncpy(ch_usec,  ch_decimal+15, 10); ch_usec[10]  = '\0';
                py_value = PyDateTime_FromDateAndTime(
                                strtol(ch_year,  &end, 10),
                                strtol(ch_month, &end, 10),
                                strtol(ch_day,   &end, 10),
                                strtol(ch_hour,  &end, 10),
                                strtol(ch_min,   &end, 10),
                                strtol(ch_sec,   &end, 10),
                                strtol(ch_usec,  &end, 10));
                if (py_value == NULL) {
                    snprintf(err_str, sizeof(err_str),
                             "DTIME: PyDateTime_FromDateAndTime failed: %s",
                             ch_decimal);
                    PyErr_SetString(PyExc_ValueError, err_str);
                    goto except;
                }
            }
            break;

        default:
            break; //teest
            PyErr_SetString(PyExc_ValueError, "unsupported data type");
            goto except;
            break;
    }

    py_name = PyString_FromString(column->col_name);
    if (py_name == NULL) {
        goto except;
    }

    if (PyDict_SetItemString(py_dict, "name", py_name) != 0) {
        goto except;
    }
    if (PyDict_SetItemString(py_dict, "value", py_value) != 0) {
        goto except;
    }
    assert(! PyErr_Occurred());
    assert(py_name);
    assert(py_value);
    assert(py_dict);
    Py_DECREF(py_name);
    Py_DECREF(py_value);
    goto finally;
except:
    assert(PyErr_Occurred());
    Py_XDECREF(py_name);
    Py_XDECREF(py_value);
    Py_XDECREF(py_dict);
    py_dict = NULL;
finally:
    return py_dict;
}

static PyObject *
InformixCdc_extract_columns_to_list(const InformixCdcObject *self, int tabid)
{
    char *rec = self->next_record_start+RECORD_HEADER_OFFSET;
    char *col;
    char *varchar_len_arr;
    int varchar_len_arr_idx = 0;
    const table_t *table;
    const column_t *column;
    int advance_col;
    int col_idx;
    PyObject *py_list = NULL;
    PyObject *py_dict = NULL;

    if (tabid >= self->next_table_id) {
        PyErr_SetString(PyExc_IndexError, "invalid internel CDC table id");
        goto except;
    }

    table = &self->tables[tabid];
    varchar_len_arr = rec + CHANGE_HEADER_SZ;
    col = varchar_len_arr + table->num_var_cols * 4;

    py_list = PyList_New(table->num_cols);
    if (py_list == NULL) {
        goto except;
    }
    for (col_idx=0; col_idx < table->num_cols; col_idx++) {
        column = &table->columns[col_idx];
        py_dict =
            InformixCdc_extract_column_to_dict(self, column, col,
                                               varchar_len_arr,
                                               &varchar_len_arr_idx,
                                               &advance_col);
        if (py_dict == NULL) {
            goto except;
        }
        PyList_SET_ITEM(py_list, col_idx, py_dict);

        if (advance_col == 0) {
            col += table->columns[col_idx].col_size;
        }
        else {
            col += advance_col;
        }
    }
    assert(! PyErr_Occurred());
    assert(py_list);
    goto finally;
except:
    assert(PyErr_Occurred());
    Py_XDECREF(py_list);
    py_list = NULL;
finally:
    return py_list;
}

static int
InformixCdc_extract_iud(InformixCdcObject *self, PyObject *py_dict)
{
    int ret = -1;
    char *rec = self->next_record_start+RECORD_HEADER_OFFSET;
    int tabid;
    PyObject *py_seq_number = NULL;
    PyObject *py_transaction_id = NULL;
    PyObject *py_tabid = NULL;
    PyObject *py_tabname = NULL;
    PyObject *py_flags = NULL;
    PyObject *py_list = NULL;

    py_seq_number = PyLong_FromLong(ld8(rec));
    if (py_seq_number == NULL) {
        goto except;
    }
    py_transaction_id = PyInt_FromLong(ld4(rec+8));
    if (py_transaction_id == NULL) {
        goto except;
    }

    tabid = ld4(rec+12);
    py_tabid = PyInt_FromLong(tabid);
    if (py_tabid == NULL) {
        goto except;
    }
    py_tabname = InformixCdc_PyString_FromTabid(self, tabid);
    if (py_tabname == NULL) {
        goto except;
    }
    py_flags = PyInt_FromLong(ld4(rec+16));
    if (py_flags == NULL) {
        goto except;
    }
    py_list = InformixCdc_extract_columns_to_list(self, tabid);
    if (py_list == NULL) {
        goto except;
    }

    if (PyDict_SetItemString(py_dict, "seq_number", py_seq_number) != 0) {
        goto except;
    }
    if (PyDict_SetItemString(py_dict, "transaction_id", py_transaction_id) != 0) {
        goto except;
    }
    if (PyDict_SetItemString(py_dict, "tabid", py_tabid) != 0) {
        goto except;
    }
    if (PyDict_SetItemString(py_dict, "tabname", py_tabname) != 0) {
        goto except;
    }
    if (PyDict_SetItemString(py_dict, "flags", py_flags) != 0) {
        goto except;
    }
    if (PyDict_SetItemString(py_dict, "columns", py_list) != 0) {
        goto except;
    }
    assert(! PyErr_Occurred());
    assert(py_seq_number);
    assert(py_transaction_id);
    assert(py_tabid);
    assert(py_tabname);
    assert(py_flags);
    assert(py_list);
    Py_DECREF(py_seq_number);
    Py_DECREF(py_transaction_id);
    Py_DECREF(py_tabid);
    Py_DECREF(py_tabname);
    Py_DECREF(py_flags);
    Py_DECREF(py_list);
    ret = 0;
    goto finally;
except:
    assert(PyErr_Occurred());
    Py_XDECREF(py_seq_number);
    Py_XDECREF(py_transaction_id);
    Py_XDECREF(py_tabid);
    Py_XDECREF(py_tabname);
    Py_XDECREF(py_flags);
    Py_XDECREF(py_list);
    ret = -1;
finally:
    return ret;
}

static int
InformixCdc_extract_tabschema(InformixCdcObject *self, int payload_sz,
                              PyObject *py_dict)
{
    int ret = -1;
    char *rec = self->next_record_start+RECORD_HEADER_OFFSET;
    int tabid;
    PyObject *py_tabid = NULL;
    PyObject *py_tabname = NULL;
    PyObject *py_flags = NULL;
    PyObject *py_fix_len_sz = NULL;
    PyObject *py_fix_len_cols = NULL;
    PyObject *py_var_len_cols = NULL;
    PyObject *py_cols_desc = NULL;

    tabid = ld4(rec);
    py_tabid = PyInt_FromLong(tabid);
    if (py_tabid == NULL) {
        goto except;
    }
    py_tabname = InformixCdc_PyString_FromTabid(self, tabid);
    if (py_tabname == NULL) {
        goto except;
    }
    py_flags = PyInt_FromLong(ld4(rec+4));
    if (py_flags == NULL) {
        goto except;
    }
    py_fix_len_sz = PyInt_FromLong(ld4(rec+8));
    if (py_fix_len_sz == NULL) {
        goto except;
    }
    py_fix_len_cols = PyInt_FromLong(ld4(rec+12));
    if (py_fix_len_cols == NULL) {
        goto except;
    }
    py_var_len_cols = PyInt_FromLong(ld4(rec+16));
    if (py_var_len_cols == NULL) {
        goto except;
    }
    py_cols_desc = PyString_FromStringAndSize(rec+20, payload_sz-1);
    if (py_cols_desc == NULL) {
        goto except;
    }

    if (PyDict_SetItemString(py_dict, "tabid", py_tabid) != 0) {
        goto except;
    }
    if (PyDict_SetItemString(py_dict, "tabname", py_tabname) != 0) {
        goto except;
    }
    if (PyDict_SetItemString(py_dict, "flags", py_flags) != 0) {
        goto except;
    }
    if (PyDict_SetItemString(py_dict, "fix_len_sz", py_fix_len_sz) != 0) {
        goto except;
    }
    if (PyDict_SetItemString(py_dict, "fix_len_cols", py_fix_len_cols) != 0) {
        goto except;
    }
    if (PyDict_SetItemString(py_dict, "var_len_cols", py_var_len_cols) != 0) {
        goto except;
    }
    if (PyDict_SetItemString(py_dict, "cols_desc", py_cols_desc) != 0) {
        goto except;
    }
    assert(! PyErr_Occurred());
    assert(py_tabid);
    assert(py_tabname);
    assert(py_flags);
    assert(py_fix_len_sz);
    assert(py_fix_len_cols);
    assert(py_var_len_cols);
    assert(py_cols_desc);
    Py_XDECREF(py_tabid);
    Py_XDECREF(py_tabname);
    Py_XDECREF(py_flags);
    Py_XDECREF(py_fix_len_sz);
    Py_XDECREF(py_fix_len_cols);
    Py_XDECREF(py_var_len_cols);
    Py_XDECREF(py_cols_desc);
    ret = 0;
    goto finally;
except:
    assert(PyErr_Occurred());
    Py_XDECREF(py_tabid);
    Py_XDECREF(py_tabname);
    Py_XDECREF(py_flags);
    Py_XDECREF(py_fix_len_sz);
    Py_XDECREF(py_fix_len_cols);
    Py_XDECREF(py_var_len_cols);
    Py_XDECREF(py_cols_desc);
    ret = -1;
finally:
    return ret;
}

static int
InformixCdc_extract_timeout(InformixCdcObject *self, PyObject *py_dict)
{
    int ret = -1;
    char *rec = self->next_record_start+RECORD_HEADER_OFFSET;
    PyObject *py_seq_number = NULL;

    py_seq_number = PyLong_FromLong(ld8(rec));
    if (py_seq_number == NULL) {
        goto except;
    }
    if (PyDict_SetItemString(py_dict, "seq_number", py_seq_number) != 0) {
        goto except;
    }
    assert(! PyErr_Occurred());
    assert(py_seq_number);
    Py_DECREF(py_seq_number);
    ret = 0;
    goto finally;
except:
    assert(PyErr_Occurred());
    Py_XDECREF(py_seq_number);
    ret = -1;
finally:
    return ret;
}

static int
InformixCdc_extract_begintx(InformixCdcObject *self, PyObject *py_dict)
{
    int ret = -1;
    char *rec = self->next_record_start+RECORD_HEADER_OFFSET;
    bigint seq_number;
    PyObject *py_seq_number = NULL;
    PyObject *py_transaction_id = NULL;
    PyObject *py_start_time = NULL;
    PyObject *py_user_id = NULL;
    PyObject *py_discard_candidate = NULL;

    seq_number = ld8(rec);
    py_seq_number = PyLong_FromLong(seq_number);
    if (py_seq_number == NULL) {
        goto except;
    }
    py_transaction_id = PyInt_FromLong(ld4(rec+8));
    if (py_transaction_id == NULL) {
        goto except;
    }
    py_start_time = PyLong_FromLong(ld8(rec+8+4));
    if (py_start_time == NULL) {
        goto except;
    }
    py_user_id = PyInt_FromLong(ld4(rec+8+4+8));
    if (py_start_time == NULL) {
        goto except;
    }
    py_discard_candidate =
        seq_number < self->last_seq_number ? Py_True : Py_False;
    Py_INCREF(py_discard_candidate);

    if (PyDict_SetItemString(py_dict, "seq_number", py_seq_number) != 0) {
        goto except;
    }
    if (PyDict_SetItemString(py_dict, "transaction_id", py_transaction_id) != 0) {
        goto except;
    }
    if (PyDict_SetItemString(py_dict, "start_time", py_start_time) != 0) {
        goto except;
    }
    if (PyDict_SetItemString(py_dict, "user_id", py_user_id) != 0) {
        goto except;
    }
    if (PyDict_SetItemString(py_dict, "discard_candidate", py_discard_candidate) != 0) {
        goto except;
    }
    assert(! PyErr_Occurred());
    assert(py_seq_number);
    assert(py_transaction_id);
    assert(py_start_time);
    assert(py_user_id);
    assert(py_discard_candidate);
    Py_DECREF(py_seq_number);
    Py_DECREF(py_transaction_id);
    Py_DECREF(py_start_time);
    Py_DECREF(py_user_id);
    Py_DECREF(py_discard_candidate);
    ret = 0;
    goto finally;
except:
    assert(PyErr_Occurred());
    Py_XDECREF(py_seq_number);
    Py_XDECREF(py_transaction_id);
    Py_XDECREF(py_start_time);
    Py_XDECREF(py_user_id);
    Py_XDECREF(py_discard_candidate);
    ret = -1;
finally:
    return ret;
}

static int
InformixCdc_extract_commtx(InformixCdcObject *self, PyObject *py_dict)
{
    int ret = -1;
    char *rec = self->next_record_start+RECORD_HEADER_OFFSET;
    bigint seq_number;
    PyObject *py_seq_number = NULL;
    PyObject *py_transaction_id = NULL;
    PyObject *py_commit_time = NULL;
    PyObject *py_discard_transaction = NULL;

    seq_number = ld8(rec);
    py_seq_number = PyLong_FromLong(seq_number);
    if (py_seq_number == NULL) {
        goto except;
    }
    py_transaction_id = PyInt_FromLong(ld4(rec+8));
    if (py_transaction_id == NULL) {
        goto except;
    }
    py_commit_time = PyLong_FromLong(ld8(rec+8+4));
    if (py_commit_time == NULL) {
        goto except;
    }
    py_discard_transaction =
        seq_number < self->last_seq_number ? Py_True : Py_False;
    Py_INCREF(py_discard_transaction);

    if (PyDict_SetItemString(py_dict, "seq_number", py_seq_number) != 0) {
        goto except;
    }
    if (PyDict_SetItemString(py_dict, "transaction_id", py_transaction_id) != 0) {
        goto except;
    }
    if (PyDict_SetItemString(py_dict, "commit_time", py_commit_time) != 0) {
        goto except;
    }
    if (PyDict_SetItemString(py_dict, "discard_transaction", py_discard_transaction) != 0) {
        goto except;
    }

    assert(! PyErr_Occurred());
    assert(py_seq_number);
    assert(py_transaction_id);
    assert(py_commit_time);
    assert(py_discard_transaction);
    Py_DECREF(py_seq_number);
    Py_DECREF(py_transaction_id);
    Py_DECREF(py_commit_time);
    Py_DECREF(py_discard_transaction);
    ret = 0;
    goto finally;
except:
    assert(PyErr_Occurred());
    Py_XDECREF(py_seq_number);
    Py_XDECREF(py_transaction_id);
    Py_XDECREF(py_commit_time);
    Py_XDECREF(py_discard_transaction);
    ret = -1;
finally:
    return ret;
}

static int
InformixCdc_extract_rbtx(InformixCdcObject *self, PyObject *py_dict)
{
    int ret = -1;
    char *rec = self->next_record_start+RECORD_HEADER_OFFSET;
    bigint seq_number;
    PyObject *py_seq_number = NULL;
    PyObject *py_transaction_id = NULL;
    PyObject *py_discard_transaction = NULL;

    seq_number = ld8(rec);
    py_seq_number = PyLong_FromLong(seq_number);
    if (py_seq_number == NULL) {
        goto except;
    }
    py_transaction_id = PyInt_FromLong(ld4(rec+8));
    if (py_transaction_id == NULL) {
        goto except;
    }
    py_discard_transaction =
        seq_number < self->last_seq_number ? Py_True : Py_False;

    if (PyDict_SetItemString(py_dict, "seq_number", py_seq_number) != 0) {
        goto except;
    }
    if (PyDict_SetItemString(py_dict, "transaction_id", py_transaction_id) != 0) {
        goto except;
    }
    if (PyDict_SetItemString(py_dict, "discard_transaction", py_discard_transaction) != 0) {
        goto except;
    }

    assert(! PyErr_Occurred());
    assert(py_seq_number);
    assert(py_transaction_id);
    assert(py_discard_transaction);
    Py_DECREF(py_seq_number);
    Py_DECREF(py_transaction_id);
    Py_DECREF(py_discard_transaction);
    ret = 0;
    goto finally;
except:
    assert(PyErr_Occurred());
    Py_XDECREF(py_seq_number);
    Py_XDECREF(py_transaction_id);
    Py_XDECREF(py_discard_transaction);
    ret = -1;
finally:
    return ret;
}

static int
InformixCdc_extract_discard(InformixCdcObject *self, PyObject *py_dict)
{
    int ret = -1;
    char *rec = self->next_record_start+RECORD_HEADER_OFFSET;
    PyObject *py_seq_number = NULL;
    PyObject *py_transaction_id = NULL;

    py_seq_number = PyLong_FromLong(ld8(rec));
    if (py_seq_number == NULL) {
        goto except;
    }
    py_transaction_id = PyInt_FromLong(ld4(rec+8));
    if (py_transaction_id == NULL) {
        goto except;
    }

    if (PyDict_SetItemString(py_dict, "seq_number", py_seq_number) != 0) {
        goto except;
    }
    if (PyDict_SetItemString(py_dict, "transaction_id", py_transaction_id) != 0) {
        goto except;
    }

    assert(! PyErr_Occurred());
    assert(py_seq_number);
    assert(py_transaction_id);
    Py_DECREF(py_seq_number);
    Py_DECREF(py_transaction_id);
    ret = 0;
    goto finally;
except:
    assert(PyErr_Occurred());
    Py_XDECREF(py_seq_number);
    Py_XDECREF(py_transaction_id);
    ret = -1;
finally:
    return ret;
}

static int
InformixCdc_extract_truncate(InformixCdcObject *self, PyObject *py_dict)
{
    int ret = -1;
    char *rec = self->next_record_start+RECORD_HEADER_OFFSET;
    int tabid;
    PyObject *py_seq_number = NULL;
    PyObject *py_transaction_id = NULL;
    PyObject *py_tabid = NULL;
    PyObject *py_tabname = NULL;

    py_seq_number = PyLong_FromLong(ld8(rec));
    if (py_seq_number == NULL) {
        goto except;
    }
    py_transaction_id = PyInt_FromLong(ld4(rec+8));
    if (py_transaction_id == NULL) {
        goto except;
    }

    tabid = ld4(rec+8+4);
    py_tabid = PyInt_FromLong(tabid);
    if (py_tabid == NULL) {
        goto except;
    }
    py_tabname = InformixCdc_PyString_FromTabid(self, tabid);
    if (py_tabname == NULL) {
        goto except;
    }

    if (PyDict_SetItemString(py_dict, "seq_number", py_seq_number) != 0) {
        goto except;
    }
    if (PyDict_SetItemString(py_dict, "transaction_id", py_transaction_id) != 0) {
        goto except;
    }
    if (PyDict_SetItemString(py_dict, "tabid", py_tabid) != 0) {
        goto except;
    }
    if (PyDict_SetItemString(py_dict, "tabname", py_tabname) != 0) {
        goto except;
    }

    assert(! PyErr_Occurred());
    assert(py_seq_number);
    assert(py_transaction_id);
    assert(py_tabid);
    assert(py_tabname);
    Py_DECREF(py_seq_number);
    Py_DECREF(py_transaction_id);
    Py_DECREF(py_tabid);
    Py_DECREF(py_tabname);
    ret = 0;
    goto finally;
except:
    assert(PyErr_Occurred());
    Py_XDECREF(py_seq_number);
    Py_XDECREF(py_transaction_id);
    Py_XDECREF(py_tabid);
    Py_XDECREF(py_tabname);
    ret = -1;
finally:
    return ret;
}

static int
InformixCdc_add_tabschema(InformixCdcObject *self)
{
    int ret = -1;
    EXEC SQL BEGIN DECLARE SECTION;
    char sql[MAX_SQL_STMT_LEN+1];
    EXEC SQL END DECLARE SECTION;

    char *rec = self->next_record_start+RECORD_HEADER_OFFSET;
    int tabid = ld4(rec);
    int var_len_cols = ld4(rec + 16);
    ifx_sqlda_t *sqlda = NULL;
    table_t *table;
    int col;

    if (tabid >= MAX_CDC_TABS) {
        PyErr_SetString(PyExc_IndexError, "max Informix CDC tables reached");
        goto except;
    }

    table = &self->tables[tabid];
    table->num_cols = 0;

    sprintf(sql, "create temp table t_informixcdc (%s) with no log", rec+20);

    EXEC SQL EXECUTE IMMEDIATE :sql;

    if (SQLCODE != 0) {
        PyErr_SetString(PyExc_Exception, "cannot create CDC temp table");
        goto except;
    }

    EXEC SQL PREPARE informixcdc FROM "select * from t_informixcdc";

    if (SQLCODE != 0) {
        goto except;
    }

    EXEC SQL DESCRIBE informixcdc INTO sqlda;

    if (SQLCODE != 0) {
        goto except;
    }

    // can't find the header that defines rtypsize
    for (col=0; col < sqlda->sqld; col++) {
        table->columns[col].col_type = sqlda->sqlvar[col].sqltype;
        table->columns[col].col_size =
            rtypsize(sqlda->sqlvar[col].sqltype, sqlda->sqlvar[col].sqllen);
        table->columns[col].col_xid = sqlda->sqlvar[col].sqlxid;
        table->columns[col].col_name =
            PyMem_Malloc(strlen(sqlda->sqlvar[col].sqlname)+1);
        if (! table->columns[col].col_name) {
            PyErr_SetString(PyExc_MemoryError, "cannot allocate column name");
            goto except;
        }
        strcpy(table->columns[col].col_name, sqlda->sqlvar[col].sqlname);
        table->num_cols++;
    }
    table->num_var_cols = var_len_cols;

    EXEC SQL EXECUTE IMMEDIATE "drop table t_informixcdc";

    // don't raise an error if we can't drop the temp table,
    // the next attempt to add a table will fail with error

    assert(! PyErr_Occurred());
    ret = 0;
    goto finally;
except:
    assert(PyErr_Occurred());
    if (tabid < MAX_CDC_TABS) {
        table = &self->tables[tabid];
        for (col=0; col < table->num_cols; col++) {
            PyMem_Free(table->columns[col].col_name);
        }
    }
    ret = -1;
finally:
    free(sqlda);
    EXEC SQL FREE informixcdc;
    return ret;
}

static PyObject*
InformixCdc_extract_record(InformixCdcObject *self, int payload_sz,
                           int packet_scheme, int record_number)
{
    char record_type[17];
    char err_str[ERRSTR_LEN+1];
    int rc = -1;
    PyObject *py_dict = NULL;
    PyObject *py_record_type = NULL;

    if (packet_scheme != PACKET_SCHEME) {
        PyErr_SetString(PyExc_BufferError,
                        "invalid Informix CDC packet scheme");
        goto except;
    }

    py_dict = PyDict_New();
    if (py_dict == NULL) {
        goto except;
    }

    switch (record_number) {
        case CDC_REC_BEGINTX:
            strcpy(record_type, "CDC_REC_BEGINTX");
            rc = InformixCdc_extract_begintx(self, py_dict);
            break;

        case CDC_REC_COMMTX:
            strcpy(record_type, "CDC_REC_COMMTX");
            rc = InformixCdc_extract_commtx(self, py_dict);
            break;

        case CDC_REC_INSERT:
            strcpy(record_type, "CDC_REC_INSERT");
            rc = InformixCdc_extract_iud(self, py_dict);
            break;

        case CDC_REC_DELETE:
            strcpy(record_type, "CDC_REC_DELETE");
            rc = InformixCdc_extract_iud(self, py_dict);
            break;

        case CDC_REC_UPDBEF:
            strcpy(record_type, "CDC_REC_UPDBEF");
            rc = InformixCdc_extract_iud(self, py_dict);
            break;

        case CDC_REC_UPDAFT:
            strcpy(record_type, "CDC_REC_UPDAFT");
            rc = InformixCdc_extract_iud(self, py_dict);
            break;

        case CDC_REC_RBTX:
            strcpy(record_type, "CDC_REC_RBTX");
            rc = InformixCdc_extract_rbtx(self, py_dict);
            break;

        case CDC_REC_DISCARD:
            strcpy(record_type, "CDC_REC_DISCARD");
            rc = InformixCdc_extract_discard(self, py_dict);
            break;

        case CDC_REC_TRUNCATE:
            strcpy(record_type, "CDC_REC_TRUNCATE");
            rc = InformixCdc_extract_truncate(self, py_dict);
            break;

        case CDC_REC_TABSCHEM:
            strcpy(record_type, "CDC_REC_TABSCHEM");
            rc = InformixCdc_extract_tabschema(self, payload_sz, py_dict);
            break;

        case CDC_REC_TIMEOUT:
            strcpy(record_type, "CDC_REC_TIMEOUT");
            rc = InformixCdc_extract_timeout(self, py_dict);
            break;

        case CDC_REC_ERROR:
            strcpy(record_type, "CDC_REC_ERROR");
            rc = 0;
            break;

        default:
            strcpy(record_type, "CDC_REC_UNKNOWN");
            rc = -1;
            break;
    }

    if (rc != 0) {
        if (! PyErr_Occurred()) {
            snprintf(err_str, sizeof(err_str),
                     "error extracting CDC record type %s", record_type);
            PyErr_SetString(PyExc_RuntimeError, err_str);
        }
        goto except;
    }

    py_record_type = PyString_FromString(record_type);
    if (py_record_type == NULL) {
        goto except;
    }

    if (PyDict_SetItemString(py_dict, "record_type", py_record_type) != 0) {
        goto except;
    }
    assert(! PyErr_Occurred());
    assert(py_record_type);
    assert(py_dict);
    Py_DECREF(py_record_type);
    goto finally;
except:
    assert(PyErr_Occurred());
    Py_XDECREF(py_dict);
    Py_XDECREF(py_record_type);
    py_dict = NULL;
finally:
    return py_dict;
}

static PyObject *
InformixCdc_connect(InformixCdcObject *self, PyObject *args, PyObject *kwds)
{
    EXEC SQL BEGIN DECLARE SECTION;
    char conn_string[CONNSTRING_LEN+1];
    char *conn_dbservername = self->dbservername;
    char *conn_name = self->name;
    char *conn_user = NULL;
    char *conn_passwd = NULL;
    long timeout = self->timeout;
    long max_records = self->max_records;
    $integer session_id;
    EXEC SQL END DECLARE SECTION;
    PyObject *ret = NULL;

    static const char *kwlist[] = { "user", "passwd", NULL };
    if (! PyArg_ParseTupleAndKeywords(args, kwds, "|ss:connect",
                                      (char**)kwlist, &conn_user,
                                      &conn_passwd)) {
        return NULL;
    }

    sprintf(conn_string, "%s@%s", self->syscdcdb, self->dbservername);
    if (conn_user && conn_passwd) {
        EXEC SQL CONNECT TO :conn_string
                         AS :conn_name
                         USER :conn_user
                         USING :conn_passwd;
    }
    else {
        EXEC SQL CONNECT TO :conn_string
                         AS :conn_name;
    }

    if (SQLCODE != 0) {
        goto badsqlcode;
    }

    self->is_connected = 1;

    EXEC SQL EXECUTE FUNCTION informix.cdc_opensess(
        :conn_dbservername, 0, :timeout, :max_records, CDC_MAJ_VER, CDC_MIN_VER
    ) INTO :session_id;

    if (SQLCODE != 0) {
        goto badsqlcode;
    }
    else if (session_id < 0) {
        goto badsqlcode;
    }

    self->session_id = session_id;

badsqlcode:
    assert(! PyErr_Occurred());
    ret = PyInt_FromLong(SQLCODE);
    if (ret == NULL) {
        goto except;
    }
    assert(ret);
    goto finally;
except:
    assert(PyErr_Occurred());
    Py_XDECREF(ret);
    ret = NULL;
finally:
    return ret;
}

static PyObject *
InformixCdc_enable(InformixCdcObject *self, PyObject *args, PyObject *kwds)
{
    EXEC SQL BEGIN DECLARE SECTION;
    char cdc_table[TABLENAME_LEN+1];
    char *cdc_columns = NULL;
    int session_id = self->session_id;
    int table_id = self->next_table_id;
    int retval;
    EXEC SQL END DECLARE SECTION;
    const char *database = NULL;
    const char *owner = NULL;
    const char *table = NULL;
    PyObject *ret = NULL;

    if (table_id >= MAX_CDC_TABS) {
        PyErr_SetString(PyExc_IndexError, "max Informix CDC tables reached");
        return NULL;
    }

    static const char *kwlist[] = {"database", "owner", "table",
                                   "columns", NULL };
    if (! PyArg_ParseTupleAndKeywords(args, kwds, "ssss:enable",
                                      (char**)kwlist, &database, &owner,
                                      &table, &cdc_columns)) {
        return NULL;
    }
    snprintf(cdc_table, TABLENAME_LEN, "%s:%s.%s", database, owner, table);

    EXEC SQL EXECUTE FUNCTION informix.cdc_set_fullrowlogging(
        :cdc_table, FULLROWLOG_ON
    ) INTO :retval;

    if (SQLCODE != 0) {
        ret = PyInt_FromLong(SQLCODE);
        if (ret == NULL) {
            goto except;
        }
        goto finally;
    }
    else if (retval < 0) {
        ret = PyInt_FromLong(retval);
        if (ret == NULL) {
            goto except;
        }
        goto finally;
    }

    EXEC SQL EXECUTE FUNCTION informix.cdc_startcapture(
        :session_id, 0, :cdc_table, :cdc_columns, :table_id
    ) INTO :retval;

    if (SQLCODE != 0) {
        ret = PyInt_FromLong(SQLCODE);
        if (ret == NULL) {
            goto except;
        }
        goto finally;
    }
    else if (retval < 0) {
        ret = PyInt_FromLong(retval);
        if (ret == NULL) {
            goto except;
        }
        goto finally;
    }

    snprintf(self->tables[self->next_table_id].tabname, TABLENAME_LEN,
             "%s@%s:%s.%s", database, self->dbservername, owner, table);
    self->next_table_id += 1;
    ret = PyInt_FromLong(0);
    if (ret == NULL) {
        goto except;
    }
    assert(! PyErr_Occurred());
    assert(ret);
    goto finally;
except:
    assert(PyErr_Occurred());
    Py_XDECREF(ret);
    ret = NULL;
finally:
    return ret;
}

static PyObject *
InformixCdc_activate(InformixCdcObject *self, PyObject *args, PyObject *kwds)
{
    EXEC SQL BEGIN DECLARE SECTION;
    $integer session_id = self->session_id;
    bigint seq_number = -1;
    $integer retval = -1;
    EXEC SQL END DECLARE SECTION;
    char err_str[ERRSTR_LEN+1];
    PyObject *ret = NULL;

    static const char *kwlist[] = {"seq_number", NULL };
    if (! PyArg_ParseTupleAndKeywords(args, kwds, "|l:activate",
                                      (char**)kwlist, &seq_number)) {
        return NULL;
    }

    if (seq_number < -1) {
        PyErr_SetString(PyExc_ValueError, "seq_number must be >= 0");
        goto except;
    }
    if (self->next_table_id <= 0) {
        PyErr_SetString(PyExc_ValueError, "no tables enabled for CDC");
        goto except;
    }
    if (self->use_savepoints) {
        self->last_seq_number = InformixCdc_query_last_seq_number(self);
        if (self->last_seq_number < 0) {
            goto except;
        }

        EXEC SQL PREPARE informixcdc_opntxns_ins FROM
            "insert into informixcdc_opntxns (id, transaction_id, seq_number) \
                values (?, ?, ?)";

        if (SQLCODE != 0) {
            snprintf(err_str, sizeof(err_str),
                    "cannot prepare informixcdc_opntxns_ins SQLCODE %d",
                    SQLCODE);
            PyErr_SetString(PyExc_ValueError, err_str);
            goto except;
        }

        EXEC SQL PREPARE informixcdc_opntxns_upd FROM
            "update informixcdc_opntxns \
             set seq_number = ? \
             where id = ? and transaction_id = ?";

        if (SQLCODE != 0) {
            snprintf(err_str, sizeof(err_str),
                    "cannot prepare informixcdc_opntxns_upd SQLCODE %d",
                    SQLCODE);
            PyErr_SetString(PyExc_ValueError, err_str);
            goto except;
        }

        EXEC SQL PREPARE informixcdc_opntxns_del FROM
            "delete from informixcdc_opntxns \
             where id = ? and transaction_id = ?";

        if (SQLCODE != 0) {
            snprintf(err_str, sizeof(err_str),
                    "cannot prepare informixcdc_opntxns_del SQLCODE %d",
                    SQLCODE);
            PyErr_SetString(PyExc_ValueError, err_str);
            goto except;
        }

        EXEC SQL PREPARE informixcdc_lsttxn_ins FROM
            "insert into informixcdc_lsttxn (id, seq_number) \
                values (?, ?)";

        if (SQLCODE != 0) {
            snprintf(err_str, sizeof(err_str),
                    "cannot prepare informixcdc_lsttxn_ins SQLCODE %d",
                    SQLCODE);
            PyErr_SetString(PyExc_ValueError, err_str);
            goto except;
        }

        EXEC SQL PREPARE informixcdc_lsttxn_upd FROM
            "update informixcdc_lsttxn \
             set seq_number = ? \
             where id = ?";

        if (SQLCODE != 0) {
            snprintf(err_str, sizeof(err_str),
                    "cannot prepare informixcdc_lsttxn_upd SQLCODE %d",
                    SQLCODE);
            PyErr_SetString(PyExc_ValueError, err_str);
            goto except;
        }
    }
    /*
     * use_savepoints seq_number   action
     * yes            -1 (default) seq_number from db. fail on err,
     * yes            >= 0         seq_number = arg
     * yes            < -1         error
     * no             -1 (default) seq_number = 0
     * no             >= 0         seq_number = arg
     * no             < -1         error
     */
    if (seq_number == -1) {
        if (self->use_savepoints) {
            seq_number = InformixCdc_query_restart_seq_number(self);
            if (seq_number < 0) {
                goto except;
            }
            else if (seq_number == 0 && self->last_seq_number != 0) {
                seq_number = self->last_seq_number + 1;
            }
        }
        else {
            seq_number = 0;
        }
    }

    EXEC SQL EXECUTE FUNCTION informix.cdc_activatesess(
        :session_id, :seq_number
    ) INTO :retval;

    if (retval < 0) {
        ret = PyInt_FromLong(retval);
        if (ret == NULL) {
            goto except;
        }
        goto finally;
    }

    ret = PyInt_FromLong(SQLCODE);
    if (ret == NULL) {
        goto except;
    }
    assert(! PyErr_Occurred());
    assert(ret);
    goto finally;
except:
    assert(PyErr_Occurred());
    Py_XDECREF(ret);
    EXEC SQL FREE informixcdc_opntxns_ins;
    EXEC SQL FREE informixcdc_opntxns_upd;
    EXEC SQL FREE informixcdc_opntxns_del;
    EXEC SQL FREE informixcdc_lsttxn_ins;
    EXEC SQL FREE informixcdc_lsttxn_upd;
    ret = NULL;
finally:
    return ret;
}

static PyObject *
InformixCdc_fetchone(InformixCdcObject *self)
{
    int bytes_read;
#ifdef OWRITESBLOB
    int bytes_written;
#endif
    mint lo_read_err = 0;
    int4 header_sz;
    int4 payload_sz;
    int4 packet_scheme;
    int4 record_number;
    int4 record_sz;
    int rc;
    PyObject *py_dict = NULL;

    while (1) {
        if (self->bytes_in_buffer >= 16) {
            // TODO: stop calling this as a function
            InformixCdc_extract_header(self->next_record_start, &header_sz,
                                       &payload_sz, &packet_scheme,
                                       &record_number);

            record_sz = header_sz + payload_sz;
            if (self->bytes_in_buffer >= record_sz) {
                py_dict = InformixCdc_extract_record(
                    self, payload_sz, packet_scheme, record_number);
                if (py_dict == NULL) {
                    goto except;
                }

                switch (record_number) {
                    case CDC_REC_BEGINTX:
                        if (self->use_savepoints) {
                            rc = InformixCdc_upsert_opntxns(self);
                            if (rc != 0) {
                                goto except;
                            }
                        }
                        break;

                    case CDC_REC_COMMTX:
                        if (self->use_savepoints) {
                            EXEC SQL BEGIN WORK;

                            if (SQLCODE != 0) {
                                PyErr_SetString(PyExc_RuntimeError,
                                                "cannot begin transaction");
                                goto except;
                            }
                            rc = InformixCdc_delete_opntxns(self);
                            if (rc != 0) {
                                EXEC SQL ROLLBACK WORK;
                                goto except;
                            }
                            rc = InformixCdc_upsert_lsttxn(self);
                            if (rc < 0) {
                                EXEC SQL ROLLBACK WORK;
                                goto except;
                            }

                            EXEC SQL COMMIT WORK;

                            if (SQLCODE != 0) {
                                EXEC SQL ROLLBACK WORK;
                                PyErr_SetString(PyExc_RuntimeError,
                                                "cannot commit transaction");
                                goto except;
                            }
                            self->last_seq_number = rc;
                        }
                        break;

                    case CDC_REC_RBTX:
                        if (self->use_savepoints) {
                            rc = InformixCdc_delete_opntxns(self);
                            if (rc != 0) {
                                goto except;
                            }
                        }
                        break;

                    case CDC_REC_TABSCHEM:
                        rc = InformixCdc_add_tabschema(self);
                        if (rc != 0) {
                            goto except;
                        }
                        break;

                    default:
                        break;
                }

                /* We've read and extracted a full record. break out of the loop
                 * DO NOT ADD ANY LOGIC THAT CAN FAIL HERE. LOGIC ABOVE
                 * DEPENDS ON BREAKING OUT OF THE INFINITE LOOP NOW.
                 */

                self->next_record_start += record_sz;
                self->bytes_in_buffer -= record_sz;

                break;
            }
        }

        // there isn't a full record in the buffer if we are here
        if (self->bytes_in_buffer > 0) {
            memcpy(self->lo_buffer,
                   self->next_record_start,
                   self->bytes_in_buffer);
        }
        bytes_read = IFX_LO_READ(self->session_id,
                                 &self->lo_buffer[self->bytes_in_buffer],
                                 self->lo_read_sz, &lo_read_err);
        if (bytes_read <= 0 || lo_read_err < 0) {
            PyErr_SetString(PyExc_IOError,
                            "read from Informix CDC SBLOB failed");
            goto except;
        }
#ifdef OWRITESBLOB
        bytes_written =
            write_testing_sblob(&self->lo_buffer[self->bytes_in_buffer],
                                bytes_read);
        if (bytes_written != bytes_read) {
            PyErr_SetString(PyExc_IOError,
                            "cannot write testing SBLOB");
        }
#endif
        self->next_record_start = self->lo_buffer;
        self->bytes_in_buffer += bytes_read;
    }
    assert(! PyErr_Occurred());
    assert(py_dict);
    goto finally;
except:
    assert(PyErr_Occurred());
    Py_XDECREF(py_dict);
    py_dict = NULL;
finally:
    return py_dict;
}

static PyObject *
InformixCdc_iter(InformixCdcObject *self)
{
    Py_INCREF(self);
    return (PyObject *)self;
}

static PyObject *
InformixCdc_iternext(InformixCdcObject *self)
{
    PyObject *py_dict = InformixCdc_fetchone(self);
    if (py_dict == NULL) {
        goto except;
    }
    assert(! PyErr_Occurred());
    assert(py_dict);
    goto finally;
except:
    assert(PyErr_Occurred());
    Py_XDECREF(py_dict);
    py_dict = NULL;
finally:
    return py_dict;
}

PyDoc_STRVAR(InformixCdc_connect__doc__,
"connect(username, password) -> integer\n\n\
Connect to the Informix engine.");

PyDoc_STRVAR(InformixCdc_enable__doc__,
"enable(database, owner, table, columns) -> integer\n\n\
Define a table and list of columns to capture.");

PyDoc_STRVAR(InformixCdc_activate__doc__,
"activate(seq_number) -> integer\n\n\
Start capture at current time or specific .");

static PyMethodDef InformixCdc_methods[] = {
    {
        "connect",
        (PyCFunction)InformixCdc_connect,
        METH_VARARGS | METH_KEYWORDS,
        InformixCdc_connect__doc__
    },
    {
        "enable",
        (PyCFunction)InformixCdc_enable,
        METH_VARARGS | METH_KEYWORDS,
        InformixCdc_enable__doc__
    },
    {
        "activate",
        (PyCFunction)InformixCdc_activate,
        METH_VARARGS | METH_KEYWORDS,
        InformixCdc_activate__doc__
    },
    {   /* sentinel */
        NULL,
        NULL
    }
};

static PyTypeObject InformixCdc_Type = {
    /* The ob_type field must be initialized in the module init function
     * to be portable to Windows without using C++. */
    PyVarObject_HEAD_INIT(NULL, 0)
    "informixcdcmodule.InformixCdc",            /*tp_name*/
    sizeof(InformixCdcObject),                  /*tp_basicsize*/
    0,                                          /*tp_itemsize*/
    /* methods */
    (destructor)InformixCdc_dealloc,            /*tp_dealloc*/
    0,                                          /*tp_print*/
    0,                                          /*tp_getattr*/
    0,                                          /*tp_setattr*/
    0,                                          /*tp_compare*/
    0,                                          /*tp_repr*/
    0,                                          /*tp_as_number*/
    0,                                          /*tp_as_sequence*/
    0,                                          /*tp_as_mapping*/
    0,                                          /*tp_hash*/
    0,                                          /*tp_call*/
    0,                                          /*tp_str*/
    0,                                          /*tp_getattro*/
    0,                                          /*tp_setattro*/
    0,                                          /*tp_as_buffer*/
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,   /*tp_flags*/
    "InformixCdc Objects",                      /*tp_doc*/
    0,                                          /*tp_traverse*/
    0,                                          /*tp_clear*/
    0,                                          /*tp_richcompare*/
    0,                                          /*tp_weaklistoffset*/
    (getiterfunc)InformixCdc_iter,              /*tp_iter*/
    (iternextfunc)InformixCdc_iternext,         /*tp_iternext*/
    InformixCdc_methods,                        /*tp_methods*/
    InformixCdc_members,                        /*tp_members*/
    InformixCdc_getseters,                      /*tp_getset*/
    0,                                          /*tp_base*/
    0,                                          /*tp_dict*/
    0,                                          /*tp_descr_get*/
    0,                                          /*tp_descr_set*/
    0,                                          /*tp_dictoffset*/
    (initproc)InformixCdc_init,                 /*tp_init*/
    0,                                          /*tp_alloc*/
    InformixCdc_new,                            /*tp_new*/
    0,                                          /*tp_free*/
    0,                                          /*tp_is_gc*/
};
/* --------------------------------------------------------------------- */

static PyTypeObject Str_Type = {
    /* The ob_type field must be initialized in the module init function
     * to be portable to Windows without using C++. */
    PyVarObject_HEAD_INIT(NULL, 0)
    "informixcdcmodule.Str",                    /*tp_name*/
    0,                                          /*tp_basicsize*/
    0,                                          /*tp_itemsize*/
    /* methods */
    0,                                          /*tp_dealloc*/
    0,                                          /*tp_print*/
    0,                                          /*tp_getattr*/
    0,                                          /*tp_setattr*/
    0,                                          /*tp_compare*/
    0,                                          /*tp_repr*/
    0,                                          /*tp_as_number*/
    0,                                          /*tp_as_sequence*/
    0,                                          /*tp_as_mapping*/
    0,                                          /*tp_hash*/
    0,                                          /*tp_call*/
    0,                                          /*tp_str*/
    0,                                          /*tp_getattro*/
    0,                                          /*tp_setattro*/
    0,                                          /*tp_as_buffer*/
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,   /*tp_flags*/
    0,                                          /*tp_doc*/
    0,                                          /*tp_traverse*/
    0,                                          /*tp_clear*/
    0,                                          /*tp_richcompare*/
    0,                                          /*tp_weaklistoffset*/
    0,                                          /*tp_iter*/
    0,                                          /*tp_iternext*/
    0,                                          /*tp_methods*/
    0,                                          /*tp_members*/
    0,                                          /*tp_getset*/
    0, /* see initxx */                         /*tp_base*/
    0,                                          /*tp_dict*/
    0,                                          /*tp_descr_get*/
    0,                                          /*tp_descr_set*/
    0,                                          /*tp_dictoffset*/
    0,                                          /*tp_init*/
    0,                                          /*tp_alloc*/
    0,                                          /*tp_new*/
    0,                                          /*tp_free*/
    0,                                          /*tp_is_gc*/
};

/* ---------- */

static PyObject *
null_richcompare(PyObject *self, PyObject *other, int op)
{
    Py_INCREF(Py_NotImplemented);
    return Py_NotImplemented;
}

static PyTypeObject Null_Type = {
    /* The ob_type field must be initialized in the module init function
     * to be portable to Windows without using C++. */
    PyVarObject_HEAD_INIT(NULL, 0)
    "informixcdcmodule.Null",                   /*tp_name*/
    0,                                          /*tp_basicsize*/
    0,                                          /*tp_itemsize*/
    /* methods */
    0,                                          /*tp_dealloc*/
    0,                                          /*tp_print*/
    0,                                          /*tp_getattr*/
    0,                                          /*tp_setattr*/
    0,                                          /*tp_compare*/
    0,                                          /*tp_repr*/
    0,                                          /*tp_as_number*/
    0,                                          /*tp_as_sequence*/
    0,                                          /*tp_as_mapping*/
    0,                                          /*tp_hash*/
    0,                                          /*tp_call*/
    0,                                          /*tp_str*/
    0,                                          /*tp_getattro*/
    0,                                          /*tp_setattro*/
    0,                                          /*tp_as_buffer*/
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,   /*tp_flags*/
    0,                                          /*tp_doc*/
    0,                                          /*tp_traverse*/
    0,                                          /*tp_clear*/
    null_richcompare,                           /*tp_richcompare*/
    0,                                          /*tp_weaklistoffset*/
    0,                                          /*tp_iter*/
    0,                                          /*tp_iternext*/
    0,                                          /*tp_methods*/
    0,                                          /*tp_members*/
    0,                                          /*tp_getset*/
    0, /* see initxx */                         /*tp_base*/
    0,                                          /*tp_dict*/
    0,                                          /*tp_descr_get*/
    0,                                          /*tp_descr_set*/
    0,                                          /*tp_dictoffset*/
    0,                                          /*tp_init*/
    0,                                          /*tp_alloc*/
    0, /* see initxx */                         /*tp_new*/
    0,                                          /*tp_free*/
    0,                                          /*tp_is_gc*/
};

/* ---------- */

/* List of functions defined in the module */

PyDoc_STRVAR(module_doc,
"Informix Change Data Capture interface.");

PyMODINIT_FUNC
init_informixcdc(void)
{
    PyObject *m;

    PyDateTime_IMPORT;

    /* Due to cross platform compiler issues the slots must be filled
     * here. It's required for portability to Windows without requiring
     * C++. */
    Null_Type.tp_base = &PyBaseObject_Type;
    Null_Type.tp_new = PyType_GenericNew;
    Str_Type.tp_base = &PyUnicode_Type;

    /* Finalize the type object including setting type of the new type
     * object; doing it here is required for portability, too. */
    if (PyType_Ready(&InformixCdc_Type) < 0)
        return;

    /* Create the module and add the functions */
    m = Py_InitModule3("_informixcdc", NULL, module_doc);
    if (m == NULL) {
        return;
    }

    /* Add some symbolic constants to the module */
    if (ErrorObject == NULL) {
        ErrorObject = PyErr_NewException("informixcdc.error", NULL, NULL);
        if (ErrorObject == NULL) {
            return;
        }
    }
    Py_INCREF(ErrorObject);
    PyModule_AddObject(m, "error", ErrorObject);

    /* Add Str */
    if (PyType_Ready(&Str_Type) < 0) {
        return;
    }
    PyModule_AddObject(m, "Str", (PyObject *)&Str_Type);

    /* Add Null */
    if (PyType_Ready(&Null_Type) < 0) {
        return;
    }
    PyModule_AddObject(m, "Null", (PyObject *)&Null_Type);

    Py_INCREF(&InformixCdc_Type);
    PyModule_AddObject(m, "InformixCdc", (PyObject *)&InformixCdc_Type);
}

/*
 * helper functions to extract numeric values from bytes
 */

#define BYTESHIFT 8
#define BYTEMASK 0xFF
#define IS_LITTLE_ENDIAN 0
#define IS_BIG_ENDIAN 1

static int2
ld2 (const char *p)
{
    int2 rtn = (((p)[0] << BYTESHIFT) + ((p)[1] & BYTEMASK));
    return rtn;
}

static int4
ld4 (const char *p)
{
    int4 rtn = ((((((
        ((int4)p[0] << BYTESHIFT) +
        (p[1] & BYTEMASK)) << BYTESHIFT) +
        (p[2] & BYTEMASK)) << BYTESHIFT) +
        (p[3] & BYTEMASK)));
    return rtn;
}

static bigint
ld8(const char *p)
{
    bigint rtn = ((((((((((((((
        ((bigint)p[0] << BYTESHIFT) +
        ((bigint)p[1] & BYTEMASK)) << BYTESHIFT) +
        ((bigint)p[2] & BYTEMASK)) << BYTESHIFT) +
        ((bigint)p[3] & BYTEMASK)) << BYTESHIFT) +
        ((bigint)p[4] & BYTEMASK)) << BYTESHIFT) +
        ((bigint)p[5] & BYTEMASK)) << BYTESHIFT) +
        ((bigint)p[6] & BYTEMASK)) << BYTESHIFT) +
        ((bigint)p[7] & BYTEMASK)));
    return rtn;
}

static double
lddbl(const char *p, const int endianness)
{
    double fval;

    if (endianness == IS_LITTLE_ENDIAN) {
        char *f;
        int c;
        f = (char *)&fval + sizeof(double) - 1;
        c = sizeof(double);
        do {
            *f-- = *p++;
        } while (--c);
    }
    else {
        memcpy((char*)&fval, p, sizeof(double));
    }

    return(fval);
}

float
ldfloat(const char *p, const int endianness)
{
    float fval;

    if (endianness == IS_LITTLE_ENDIAN) {
        char *f;
        int c;
        f = (char *)&fval + sizeof(float) - 1;
        c = sizeof(float);
        do {
            *f-- = *p++;
        } while (--c);
    }
    else {
        memcpy((char*)&fval, p, sizeof(float));
    }

    return(fval);
}

static int
get_platform_endianness()
{
    unsigned int i = 1;
    char *c = (char*)&i;
    if (*c) {
        return IS_LITTLE_ENDIAN;
    }
    else {
        return IS_BIG_ENDIAN;
    }
};
