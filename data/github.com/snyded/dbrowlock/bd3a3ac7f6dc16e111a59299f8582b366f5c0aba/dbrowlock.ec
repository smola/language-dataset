/*
    dbrowlock.ec - set lock mode of a database table to row-level
    Copyright (C) 1994  David A. Snyder
 
    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; version 2 of the License.
 
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
 
    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#ifndef lint
static char sccsid[] = "@(#) dbrowlock.ec 1.1  94/09/25 13:27:07";
#endif /* not lint */


#include <ctype.h>
#include <stdio.h>
#include <string.h>
$include sqlca;
$include sqltypes;

#define SUCCESS	0
#define strsch(s1, s2) strnsch(s1, s2, 0)

FILE	*fp;
char	*database = NULL, *table = NULL;
void	exit();

$struct _systables {
	char	tabname[19];
	char	owner[9];
	char	dirpath[65];
	long	tabid;
	short	rowsize;
	short	ncols;
	short	nindexes;
	long	nrows;
	long	created;
	long	version;
	char	tabtype[2];
	char	audpath[65];
} systables;

main(argc, argv)
int	argc;
char	*argv[];
{

	$char	exec_stmt[80], qry_stmt[72];
	char	engine[64], *getenv(), *informixdir = NULL, *sqlexec = NULL;
	extern char	*optarg;
	extern int	optind, opterr;
	int	c, dflg = 0, errflg = 0, tflg = 0;

	/* Print copyright message */
	(void)fprintf(stderr, "DBROWLOCK version 1.1, Copyright (C) 1994 David A. Snyder\n\n");

	/* check for sqlexec */
	sqlexec = getenv("SQLEXEC");
	if ((informixdir = getenv("INFORMIXDIR")) == NULL)
		informixdir = "/usr/informix";

	if (sqlexec != NULL) {
		if (strsch(sqlexec, "sqlexec") != -1) {
			fprintf(stderr, "%s: not needed for Informix-SE\n", argv[0]);
			exit(1);
		}
	} else {
		sprintf(engine, "%s/lib/sqlexec", informixdir);
		if (access(engine, 0) == SUCCESS) {
			fprintf(stderr, "%s: not needed for Informix-SE\n", argv[0]);
			exit(1);
		}
	}

	/* get command line options */
	while ((c = getopt(argc, argv, "d:t:")) != EOF)
		switch (c) {
		case 'd':
			dflg++;
			database = optarg;
			break;
		case 't':
			tflg++;
			table = optarg;
			break;
		default:
			errflg++;
			break;
		}

	/* validate command line options */
	if (errflg || !dflg) {
		(void)fprintf(stderr, "usage: %s -d dbname [-t tabname] [filename]\n", argv[0]);
		exit(1);
	}

	/* locate the database in the system */
	sprintf(exec_stmt, "database %s", database);
	$prepare db_exec from $exec_stmt;
	$execute db_exec;
	if (sqlca.sqlcode != SUCCESS) {
		fprintf(stderr, "Database not found or no system permission.\n\n");
		exit(1);
	}

	/* build the select statement */
	if (tflg) {
		if (strchr(table, '*') == NULL &&
		    strchr(table, '[') == NULL &&
		    strchr(table, '?') == NULL)
			sprintf(qry_stmt, "select tabname, tabid from systables where tabname = \"%s\" and tabtype = \"T\"", table);
		else
			sprintf(qry_stmt, "select tabname, tabid from systables where tabname matches \"%s\" and tabtype = \"T\"", table);
	} else
		sprintf(qry_stmt, "select tabname, tabid from systables where tabtype = \"T\" order by tabname");

	/* declare some cursors */
	$prepare tab_query from $qry_stmt;
	$declare tab_cursor cursor for tab_query;

	if (argc > optind) {
		if ((fp = fopen(argv[argc - 1], "w")) == NULL) {
			fprintf(stderr, "Could not open %s\n", argv[argc - 1]);
			exit(1);
		}
	} else
		fp = stdout;

	/* read the database for the table(s) and create some output */
	$open tab_cursor;
	$fetch tab_cursor into $systables.tabname, $systables.tabid;
	if (sqlca.sqlcode == SQLNOTFOUND)
		fprintf(stderr, "Table %s not found.\n", table);
	while (sqlca.sqlcode == SUCCESS) {
		rtrim(systables.tabname);
		if (systables.tabid >= 100 &&
		   (strcmp(systables.tabname, "sysmenus") &&
		    strcmp(systables.tabname, "sysmenuitems") &&
		    strcmp(systables.tabname, "syscolatt") &&
		    strcmp(systables.tabname, "sysvalatt") || tflg))
			fprintf(fp, "ALTER TABLE %s LOCK MODE (ROW);\n", systables.tabname);
		$fetch tab_cursor into $systables.tabname, $systables.tabid;
	}
	$close tab_cursor;

	exit(0);
}


/*******************************************************************************
* This function will trim trailing spaces from s.                              *
*******************************************************************************/

rtrim(s)
char *s;
{
	int	i;

	for (i = strlen(s) - 1; i >= 0; i--)
		if (!isgraph(s[i]) || !isascii(s[i]))
			s[i] = '\0';
		else
			break;
}


/*******************************************************************************
* This function is similar to strchr(2).  Instead of returning a pointer, it   *
* returns the numerical position of s2 in s1.  A nice macro to define is:      *
* #define strsch(s1, s2) strnsch(s1, s2, 0)                                    *
*******************************************************************************/

strnsch(s1, s2, n)
char	*s1;
char	*s2;
int	n;
{
	int	i;

	for (i = n; i < (strlen(s1) - strlen(s2) + 1); i++)
		if (strncmp(s1 + i, s2, strlen(s2)) == 0)
			return(i);

	return(-1);
}


