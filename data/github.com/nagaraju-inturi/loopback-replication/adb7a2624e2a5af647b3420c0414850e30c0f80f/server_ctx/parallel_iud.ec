/*
 * TO COMPILE:
 * export THREADLIB=posix
 * export INFORMIXDIR=/work/cm/csdk/4.10.FCX_latest
 * LD_LIBRARY_PATH=$INFORMIXDIR/lib:$INFORMIXDIR/lib/esql:$LD_LIBRARY_PATH 
 * export PATH=$INFORMIXDIR/bin:$PATH
 *
 * esql -g -o parallel_iud -thread -static parallel_iud.ec
 *
 * Informix password to be set in pwd field in this file.
 *
 * SCHEMA:
 * Database: ycsb, Table : usertable,
 * Columns : YCSB_KEY serial, FIELD1, FIELD2, FIELD3, FIELD4 string type data
 * 
CREATE TABLE usertable(
  YCSB_KEY serial,
  FIELD1 CHAR (112),
  FIELD2 CHAR (112),
  FIELD3 lvarchar(112),
  FIELD4 CHAR (112),
  FIELD5 CHAR (112),
  FIELD6 CHAR (112),
  FIELD7 CHAR (112),
  FIELD8 CHAR (112),
  FIELD9 CHAR (112),
  FIELD10 CHAR (112)
) lock mode row;
--create unique index pkidx on usertable (YCSB_KEY) compressed;
alter table usertable add constraint primary key(YCSB_KEY);
CREATE PROCEDURE public.sysdbopen() SET environment use_sharding on; END PROCEDURE;

 *
 * USAGE:
 * time userid informix  ./parallel_iud 20 50000 I # To insert 1 million rows using 20 threads 
 * time userid informix  ./parallel_iud 20 50000 U # To update 1 million rows using 20 threads 
 */
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <termios.h>
#include <ifxgls.h>

#define DEFAULT_NUM_THREADS 5
#define DEFAULT_NUM_CONNECTS 5
#define DEFAULT_CONN_DELAY 0
#define DEFAULT_WORK_LOOPS 1
#define DEFAULT_WORK_DELAY 0

$define FETCH_VAL_LEN 260;

void *connect_thread();
int counter = 0;
int conn_delay = 0;
int do_work_delay = 0;
int do_work_loops = 0;
$char usr[80] = "informix";
$char pwd[80] = "informix1";
char *opType ;

static $char userID[129];
static $char password[33];
char *isTrusted;


static void checksql(char *msg, ...);



static void
getUserID()
{
    int4    len;

do  {
    do  {
        fprintf(stdout,"Enter user name: \n");
        if  (fgets(userID, sizeof(userID) - 1, stdin) == NULL)
            {
            fprintf(stdout,"%s\n", "User ID must be entered in a non-trusted request");
            continue;
            }
        else
            break;
        }   while(1);

    len = strlen(userID);

    /*
     *  Get rid of any trailing carriage return
     */
    if  ((len > 0) && (userID[len - 1] == '\n'))
         userID[len - 1] = '\0';
    }   while(0);

    return;
}
static void
getPassword()
{   
    int             tchar;
    char            *tpnt = password;
    int            gotTerm = 0;
    tcflag_t        sflag;
    struct termios  sgtty;

do  {
    /*
     *  Turn off echo when getting the password
     */
    if  (isatty(fileno(stdin)))
        {
        tcgetattr(fileno(stdin), &sgtty);
        sflag = sgtty.c_lflag;
        sgtty.c_lflag &= (~ECHO);
        tcsetattr(fileno(stdin), TCSANOW, &sgtty);
        gotTerm = 1;
        }
    
    fprintf(stdout, "Enter password: \n");

    do  {
        tchar = fgetc(stdin);
 
        if  (tchar == '\n')
            {
            *tpnt = '\0';
            fprintf(stdout,"\n");
            break;
            }
        *(tpnt++) = (char)(tchar & 0xFF);
        }   while(1);

    if  (gotTerm);
        {
        sgtty.c_lflag = sflag;
        tcsetattr(fileno(stdin), TCSANOW, &sgtty);
        }
    }   while(0);

    return;
}

void
start_threads(num_threads, row_count)
int num_threads;
int row_count;
{
    int i = 0;
    int rc1 = 0;
    pthread_t thread_id[1024];
    char type[20];


    if (strcasecmp(opType, "I") == 0)
        strcpy(type, "insert");
    else if (strcasecmp(opType, "U") == 0)
        strcpy(type, "update");
    else if (strcasecmp(opType, "S") == 0)
        strcpy(type, "select");
    else if (strcasecmp(opType, "M") == 0)
        strcpy(type, "mix");
    printf("Creating %d threads, each executing %s %d count\n",
	num_threads, type, row_count);

    for (i=0; i<num_threads; i++) 
	{
	rc1 = pthread_create(&thread_id[i],NULL,&connect_thread,&row_count);
	if (rc1 != 0)
	    {
	    printf("\nError creating pthread[%1d]\n",i);
	    exit(1);
	    }
	}

    for (i=0; i<num_threads; i++) 
	{
	rc1 = pthread_join( thread_id[i], NULL);
	if (rc1 != 0)
	    {
	    printf("\nError joining pthread[%1d]\n",i);
	    exit(1);
	    }
	}
}

char
getrandchar()
{
    int randval = 0;
    char randchar = 0;

    for(;;) 
	{
    	randval = rand() % 58;
	if (randval < 26 || randval > 31)
	    break;
	}

    randchar = (char)(65+randval);

    return randchar;
}

mint
genData(
    char *field1_val,
    char *field2_val,
    char *field3_val,
    char *field4_val
    )
{
    int j;

    field1_val[0] = '\0';
    field2_val[0] = '\0';
    field3_val[0] = '\0';
    field4_val[0] = '\0';

    strcpy(field1_val, "{key:\"");
    for (j=6;j<90;j++)
        field1_val[j]=getrandchar();
    field1_val[j] = '\0';
    strcat(field1_val, "\"}");
    field1_val[j+2] = '\0';

    strcpy(field2_val, "{key:\"");
    for (j=6;j<90;j++)
        field2_val[j]=getrandchar();
    field2_val[j] = '\0';
    strcat(field2_val, "\"}");
    field2_val[j+2] = '\0';

    strcpy(field3_val, "{key:\"");
    for (j=6;j<90;j++)
        field3_val[j]=getrandchar();
    field3_val[j] = '\0';
    strcat(field3_val, "\"}");
    field3_val[j+2] = '\0';

    strcpy(field4_val, "{key:\"");
    for (j=6;j<90;j++)
        field4_val[j]=getrandchar();
    field4_val[j] = '\0';
    strcat(field4_val, "\"}");
    field4_val[j+2] = '\0';

    //fprintf(stdout, "field1 %s\n", field1_val);
    return 0;

}

int
do_insert(int row_count)
{
    $char cli_noms[26];
    $char cli_pat[26];
    $char cli_mat[26];
    $char cli_nomcap[26];
    $char cli_nom_completo[76];
    $char demoquery1[256];
    $char cursor_name[100]; 
    $char demostmt1[100]; 
    $char activno='t';
    $dtime_t now;
    $int cli_id_cli = 0;
	$int c1_val=0;
    int size25=0;
    int size75=0;

    $char field1_val[112];
    $char field2_val[112];
    $char field3_val[112];
    $char field4_val[112];
    int i,j;

   field1_val[0] = '\0';
   field2_val[0] = '\0';
   field3_val[0] = '\0';
   field4_val[0] = '\0';
   cursor_name[0] = '\0';
   demostmt1[0] = '\0';
   sprintf(cursor_name, "cursor_insert_%d", abs(pthread_self()));
   sprintf(demostmt1, "demo_insert_%d", abs(pthread_self()));
/*
    size25 = rand()%25 + 1;
    for (i=0;i<size25;i++)
	cli_noms[i]=getrandchar();
    cli_noms[size25]='\0';

    size25 = rand()%25 + 1;
    for (i=0;i<size25;i++)
	cli_pat[i]=getrandchar();
    cli_pat[size25]='\0';

    size25 = rand()%25 + 1;
    for (i=0;i<size25;i++)
	cli_mat[i]=getrandchar();
    cli_mat[size25]='\0';

    size25 = rand()%25 + 1;
    for (i=0;i<size25;i++)
	cli_nomcap[i]=getrandchar();
    cli_nomcap[size25]='\0';

    size75 = rand()%75 + 1;
    for (i=0;i<size75;i++)
	cli_nom_completo[i]=getrandchar();
    cli_nom_completo[size75]='\0';

    now.dt_qual=TU_DTENCODE(TU_YEAR,TU_SECOND);
    dtcurrent(&now);
	c1_val = rand();
*/
//    sprintf(demoquery1, "insert into usertable (YCSB_KEY, FIELD1, FIELD2, FIELD3, FIELD4) values (0, ?::JSON, ?, ?, ?)");
    sprintf(demoquery1, "insert into usertable (YCSB_KEY, FIELD1, FIELD2, FIELD3, FIELD4) values (0, ?, ?, ?, ?)");


    $prepare :demostmt1 from :demoquery1;
    if (sqlca.sqlcode != 0) return -1;

    EXEC SQL DECLARE :cursor_name CURSOR WITH HOLD FOR :demostmt1;
    if (sqlca.sqlcode != 0)
       {
       //printf("\ndeclare cursor failed sqlcode = %d, isam = %d\n",sqlca.sqlcode, sqlca.sqlerrd[1]); 
       checksql("declare cursor failed");
       return -1;
       }
    EXEC SQL OPEN :cursor_name;
    if (sqlca.sqlcode != 0)
       {
       //printf("\nopen cursor failed sqlcode = %d, isam = %d\n",sqlca.sqlcode, sqlca.sqlerrd[1]); 
       checksql("open cursor failed");
       return -1;
       }
    $begin work;
    for (i=0; i < row_count; i ++)
        {
        genData(field1_val, field2_val, field3_val, field4_val);

        EXEC SQL PUT :cursor_name FROM :field1_val, :field2_val, :field3_val, :field4_val;
        //$execute :demostmt1 using :field1_val, :field2_val, :field3_val, :field4_val;
        if (sqlca.sqlcode != 0)
           {
           //printf("\ninsert failed sqlcode = %d, isam = %d\n",sqlca.sqlcode, sqlca.sqlerrd[1]); 
           checksql("insert failed");
           }
        if (sqlca.sqlcode != 0) return -2;
        /*
         * Commit for every 100 rows.
         */
        if ((i % 100) == 0)
           {
           EXEC SQL FLUSH :cursor_name;
           if (sqlca.sqlcode != 0)
              {
              //printf("\nflush failed sqlcode = %d, isam = %d\n",sqlca.sqlcode, sqlca.sqlerrd[1]); 
              checksql("flush failed");
              }
           $commit work;
           if (sqlca.sqlcode != 0)
              printf("\ncommit failed sqlcode = %d, isam = %d\n",sqlca.sqlcode, sqlca.sqlerrd[1]); 
           $begin work;
           }
        }

    EXEC SQL FLUSH :cursor_name;
    $commit work;
    EXEC SQL CLOSE :cursor_name;

    $free :demostmt1;
    if (sqlca.sqlcode != 0) 
	return sqlca.sqlerrd[1];
    return 0;
}

int
do_update(int row_count)
{
    $char cli_noms[26];
    $char cli_pat[26];
    $char cli_mat[26];
    $char cli_nomcap[26];
    $char cli_nom_completo[76];
    $char demoquery1[256];
    $char activno='t';
    $dtime_t now;
    $int cli_id_cli = 0;
    $int maxval=0;
    $int minval=0;
    int size25=0;
    int size75=0;

    $char field1_val[112];
    $char field2_val[112];
    $char field3_val[112];
    $char field4_val[112];
    int i,j;

   field1_val[0] = '\0';
   field2_val[0] = '\0';
   field3_val[0] = '\0';
   field4_val[0] = '\0';

/*
    size25 = rand()%25 + 1;
    for (i=0;i<size25;i++)
        cli_noms[i]=getrandchar();
    cli_noms[size25]='\0';

    size25 = rand()%25 + 1;
    for (i=0;i<size25;i++)
        cli_pat[i]=getrandchar();
    cli_pat[size25]='\0';

    size25 = rand()%25 + 1;
    for (i=0;i<size25;i++)
        cli_mat[i]=getrandchar();
    cli_mat[size25]='\0';

    size25 = rand()%25 + 1;
    for (i=0;i<size25;i++)
        cli_nomcap[i]=getrandchar();
    cli_nomcap[size25]='\0';

    size75 = rand()%75 + 1;
    for (i=0;i<size75;i++)
        cli_nom_completo[i]=getrandchar();
    cli_nom_completo[size75]='\0';

    now.dt_qual=TU_DTENCODE(TU_YEAR,TU_SECOND);
    dtcurrent(&now);

    sprintf(demoquery1, "update tonom set activo=?, cli_nomcap=?, cli_fecf_nom=?, cli_mat=?,cli_noms=?, cli_nom_completo=?, cli_pat=? where cli_id_cli=?");
*/

    sprintf(demoquery1, "update usertable set FIELD1=?, FIELD2=?, FIELD3=?, FIELD4=? where YCSB_KEY=?");
    $set isolation to dirty read;
    $select max(YCSB_KEY) into :maxval from usertable;
    $select min(YCSB_KEY) into :minval from usertable;
    $set isolation to committed read;


    $prepare demostmt2 from :demoquery1;
    if (sqlca.sqlcode != 0) return -1;

    $begin work;
    for (i=0; i < row_count; i ++)
        {
        genData(field1_val, field2_val, field3_val, field4_val);

        cli_id_cli = (rand() % maxval) + 1;
        if (cli_id_cli < minval)
            {
            cli_id_cli += minval;
            if (cli_id_cli > maxval)
                cli_id_cli = maxval;
            }
        $execute demostmt2 using :field1_val, :field2_val, :field3_val, :field4_val, :cli_id_cli;
        if (sqlca.sqlcode != 0)
           printf("\nupdate failed sqlcode = %d, isam = %d\n",sqlca.sqlcode, sqlca.sqlerrd[1]); 
/*
        else
           printf("\nupdate success\n");
        if (sqlca.sqlcode != 0) 
            return sqlca.sqlerrd[1];
*/
        /*
         * Commit for every 10 rows.
         */
        if ((i % 10) == 0)
           {
           $commit work;
           $begin work;
           }
        }

    $commit work;
    $free demostmt2;
    return 0;

  return 0;
}

int
do_select(int row_count)
{
    $char demoquery1[256];
    $dtime_t now;
    $int cli_id_cli = 0;
    $int maxval=0;
    $int minval=0;

    $char field1_val[112];
    $char field2_val[112];
    $char field3_val[112];
    $char field4_val[112];
    int i,j;

   field1_val[0] = '\0';
   field2_val[0] = '\0';
   field3_val[0] = '\0';
   field4_val[0] = '\0';

    sprintf(demoquery1, "select FIELD1, FIELD2, FIELD3, FIELD4 from usertable where YCSB_KEY=?");
    $set isolation to dirty read;
    /* $select max(YCSB_KEY) into :maxval from usertable; */
    $select max(m) into :maxval from (select max(YCSB_KEY) m from usertable);
    /* $select min(YCSB_KEY) into :minval from usertable; */
    $select min(m) into :minval from (select min(YCSB_KEY) m from usertable);
    $set isolation to committed read;

/*
    printf("maxval value %d\n", maxval);
    printf("minval value %d\n", minval);
*/

    $prepare demostmt3 from :demoquery1;
    if (sqlca.sqlcode != 0) return -1;

    for (i=0; i < row_count; i ++)
        {
        cli_id_cli = (rand() % maxval) + 1;
        if (cli_id_cli < minval)
            {
            cli_id_cli += minval;
            if (cli_id_cli > maxval)
                cli_id_cli = maxval;
            }
        $execute demostmt3 into :field1_val, :field2_val, :field3_val, :field4_val using :cli_id_cli;
        if (sqlca.sqlcode != 0)
           printf("\nselect failed sqlcode = %d, isam = %d for key %d\n",sqlca.sqlcode, sqlca.sqlerrd[1], cli_id_cli); 

/*
        if (sqlca.sqlcode != 0) 
            return sqlca.sqlerrd[1];
*/
        }

    $free demostmt3;

  return 0;
}
int
do_select2(int row_count)
{
    $dtime_t now;
    $int cli_id_cli_b = 0;
    $int cli_id_cli_e = 0;
    $int maxval=0;
    $int minval=0;
    int range = 1000;

    $char field1_val[112];
    $char field2_val[112];
    $char field3_val[112];
    $char field4_val[112];
    $char cursor_name[100]; 
    $char demostmt1[100]; 
    $static char  stmt[] = "select FIELD1, FIELD2, FIELD3, FIELD4 from usertable where YCSB_KEY between ? and ?";
    int i,j;

   field1_val[0] = '\0';
   field2_val[0] = '\0';
   field3_val[0] = '\0';
   field4_val[0] = '\0';
   cursor_name[0] = '\0';
   demostmt1[0] = '\0';
   sprintf(cursor_name, "cursor_sel_%d", abs(pthread_self()));
   sprintf(demostmt1, "demo_sel_%d", abs(pthread_self()));

    $set isolation to dirty read;
    /* $select max(YCSB_KEY) into :maxval from usertable; */
    $select max(m) into :maxval from (select max(YCSB_KEY) m from usertable);
    /* $select min(YCSB_KEY) into :minval from usertable; */
    $select min(m) into :minval from (select min(YCSB_KEY) m from usertable);
    $set isolation to committed read;

/*
    printf("maxval value %d\n", maxval);
    printf("minval value %d\n", minval);
*/


    $prepare :demostmt1 from :stmt;
    if (sqlca.sqlcode != 0)
        {
        printf("\n prepare statement failed, sqlcode %d isam error %d\n", sqlca.sqlcode, sqlca.sqlerrd[1]);
        printf("\n demostmt1 %s stmt %s\n", demostmt1, stmt);
        exit(-1);
        }
    for (i=0; i < row_count; i ++)
        {
        cli_id_cli_b = (rand() % (maxval-range)) + 1;
        cli_id_cli_b = abs(cli_id_cli_b);
        if (cli_id_cli_b < minval)
            {
            cli_id_cli_b += minval;
            if (cli_id_cli_b > maxval)
                cli_id_cli_b = maxval;
            }
        cli_id_cli_e = cli_id_cli_b + range;
        $declare :cursor_name cursor for :demostmt1;
        $open :cursor_name using :cli_id_cli_b, :cli_id_cli_e;
        if (sqlca.sqlcode != 0)
           {
           printf("\n declare cursor failed, sqlcode %d isam error %d\n", sqlca.sqlcode, sqlca.sqlerrd[1]);
           exit(-1);
           }
        for(;;)
           {
           $fetch :cursor_name into :field1_val, :field2_val, :field3_val, :field4_val;
           if (sqlca.sqlcode == 100)
               break;
           else if (sqlca.sqlcode)
                 {
                 printf("\n fetch cursor failed (%d, %d), sqlcode %d isam error %d\n", cli_id_cli_b, cli_id_cli_e, sqlca.sqlcode, sqlca.sqlerrd[1]);
                 exit(-1);
                 }
           }
        
        $close :cursor_name;
        }

    $free :demostmt1;

  return 0;
}

int
do_mixedwork(int row_count)
{
    int i = 0;
    for (i=0; i < row_count; i ++)
        {
        do_insert(1);
        do_update(1);
        do_select2(5);
        }
  return 0;
}

int
do_work()
{
    int rc = 0;
	int wtab = 0;
	$char mtst[200];
	$int rid = 0;
	int j;

	wtab = rand();

	switch (wtab%3)
		{
		case 0:
			sprintf(mtst,"update b1 set c2 = c2 + 1 where c1 = ?");
			break;	
		case 1:
			sprintf(mtst,"update b2 set c2 = c2 + 1 where c1 = ?");
			break;	
		case 2:
			sprintf(mtst,"update b3 set c2 = c2 + 1 where c1 = ?");
			break;	
		}
	$prepare u1 from :mtst;

	$set lock mode to wait;
	$begin work;	
	rid = (wtab % 970) + 1;
	for ( j=1 ; j <= ((wtab%30)+1); j++)
		{
		$execute u1 using :rid;
		rid++; 
		}
	$commit work;

	$free u1;

    return 0;
}

void *
connect_thread(row_count)
int *row_count;
{
    $char conn[64];
    int i = 0;
    int j = 0;
    int rc = 0;
   
    sprintf(conn, "conn_%d", abs(pthread_self()));
    if (strcasecmp(isTrusted, "T") == 0)
       {
       $connect to "ycsb" as :conn;
       }
    else
       {
       $connect to "ycsb" as :conn user :userID using :password;
       }
    if (SQLCODE)
        printf("Error %d connecting to server as %s\n", sqlca.sqlcode, conn);
    srand(pthread_self());
    $set environment USE_SHARDING "on";
    $set lock mode to wait 30;
    if (strcasecmp(opType, "I") == 0)
        do_insert(*row_count);
    else if (strcasecmp(opType, "U") == 0)
        do_update(*row_count);
    else if (strcasecmp(opType, "S") == 0)
        do_select2(*row_count);
    else if (strcasecmp(opType, "M") == 0)
        do_mixedwork(*row_count);
    $disconnect current;
}

main(argc, argv)
int argc;
char *argv[];
{
    int num_threads = 0;
    int row_count = 0;

    if ((argc == 4) || (argc == 5))
	{
        num_threads = atoi(argv[1]);
	row_count = atoi(argv[2]);
        opType = argv[3];
        if (argc == 5)
            isTrusted = argv[4];
        else
            isTrusted = "N";
	}
    else
	{
	printf("\nUsage: parallel_iud <num_threads> <row count per thread> <I|U|S|M> <T> #T for trusted hosts connection\n");

	exit(1);
	}

    if (strcasecmp(isTrusted, "T") != 0)
        {
        userID[0] = '\0';
        password[0] = '\0';
        getUserID();
        getPassword();
        }
    if (num_threads <= 0 || num_threads > 10000)
	num_threads = DEFAULT_NUM_THREADS;

    if (row_count < 0 || row_count > 1000000)
	row_count = DEFAULT_NUM_CONNECTS;

    start_threads(num_threads, row_count);

    exit(0);
}



void
geterrmsg(int err, char *msg, int msglen, char *a1, char *a2)
{
    register char *p;
    char *a;
    register int i;
    int sw;

    a = a1;
    i = 0;
    if (err < 0)
        {
        sprintf(msg,"%5d: ",-err);
        i = 7;
        }
    p = mmsgtext(err);

    /* For 7.0 GLS the # of bytes to increment a string by is determined
     * by gl_mblen().
     */
    while (((i + gl_mblen(gl_locale, (gl_mchar_t *) p, GL_MB_MAX)) <= msglen) && *p)
        {
        if ( *p == '%' )
            {
            p++;
            if ( a )
               while (((i + gl_mblen(gl_locale,(gl_mchar_t *) a, GL_MB_MAX)) <= msglen) && *a)
                   {
                   sw = gl_mblen(gl_locale, (gl_mchar_t *) a,GL_MB_MAX);
                   while (sw-- > 0)
                       msg[i++] = *a++;
                   }
            if ( *p ) p++;
                a = a2;
            }
        else
            {
            sw = gl_mblen(gl_locale,(gl_mchar_t *) p,GL_MB_MAX);
            while (sw-- > 0)
                msg[i++] = *p++;
            }
        }
    msg[i] = 0;
}

static void
checksql(char *msg, ...)
{
    mint    errmsgSize = 1024;
    char    *errmsg;
    char    *errmsgIsam;
    mint    errorCd;
    va_list ap;

    va_start(ap, msg);

        vfprintf(stderr, msg, ap);
        fprintf(stderr, "\nSQLCODE %ld ISAM %ld\n",
                    (long)sqlca.sqlcode, (long)sqlca.sqlerrd[1]);
        fprintf(stderr,"\n");
        errmsg = (char*) malloc(errmsgSize+1);
        if (errmsg != NULL)
            {
            if (sqlca.sqlcode)
                {
                errmsg[0]='\0';
                  geterrmsg(sqlca.sqlcode, errmsg, errmsgSize, sqlca.sqlerrm, (char *)NULL);
                if (*errmsg)
                       fprintf(stderr,"\n\t%s", errmsg);
                }

            if (sqlca.sqlerrd[1])
                {
                errmsg[0]='\0';
                geterrmsg(sqlca.sqlerrd[1], errmsg, errmsgSize, (char *)NULL, (char *)NULL);
                if (*errmsg)
                     fprintf(stderr,"\n\t%s", errmsg);
               }
            }
    va_end(ap);
}



