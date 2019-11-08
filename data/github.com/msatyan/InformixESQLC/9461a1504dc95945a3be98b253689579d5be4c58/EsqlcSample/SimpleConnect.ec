#include <string.h>
#include <stdio.h>
#include <stdio.h>
EXEC SQL include sqlca;
EXEC SQL include sqltypes;
EXEC SQL include sqlca;



int main_SimpleConnect()
{
    EXEC SQL BEGIN DECLARE SECTION;
    char user[32] = "myuser1";
    char pwd[32] = "mypwd1";

    char ConnStr[256] = "ids5db1@ids5|olsoctcp|lxvm-l170.ibm.com|5555";

    EXEC SQL END DECLARE SECTION;


    printf("\n Simple Connection Test..!");

    EXEC SQL connect to :ConnStr USER :user USING :pwd; 

    {
        int rc=0;
        char *msg = ConnStr;

        printf( "\n Connection Test Status :" );
        rc = sqlca.sqlcode;
        if ( sqlca.sqlcode != 0 && sqlca.sqlcode != 100 )
        {
            printf ( "\n %d  Error : %s", rc, (msg==NULL) ? "NULL" : msg  );
            rc = -1;
        }
        else
        {
            printf ( "\n Success : %s", (msg==NULL) ? "NULL" : msg  );
            rc = 0;
        }
        printf ( "\n");

    }

    printf("\nDone!! \n");

    return(0);
}



/*
#include <stdio.h>
#include <sqlca.h>
#include <sqltypes.h>
#include <sqlhdr.h>

int
main(argc, argv)
int argc;
char *argv[];
{
EXEC SQL BEGIN DECLARE SECTION;
    int i;
EXEC SQL END DECLARE SECTION;

    $database testdb;

    $select count(*) into :i from systables;
   exit(0);

}
*/
