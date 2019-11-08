/*
    dbdatabases.ec - prints a database information report
    Copyright (C) 1998  David A. Snyder
 
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
static char sccsid[] = "@(#)dbdatabases.ec 1.1  02/11/98 16:34:03  02/23/98 15:57:14";
#endif /* not lint */


#include <stdio.h>
#include <string.h>
#include <ctype.h>

$char	dbsnames[2048+1][18+1];
int	debug = 0, errflg = 0;
void	exit(), dberror();

main(argc, argv)
int     argc;
char    *argv[];
{
	$char	name[18+1], owner[8+1], partnum[11];
	$long	created, is_logging, is_buff_log, is_ansi;
	char	createdstr[10+1], logmode;
	extern char	*optarg;
	extern int	optind, opterr;
	register int	c;
	void	GetDbsnames();

	/* Print copyright message */
	(void)fprintf(stderr, "DBDATABASES version 1.1, Copyright (C) 1998 David A. Snyder\n\n");

	/* get command line options */
	while ((c = getopt(argc, argv, "b")) != EOF)
		switch (c) {
		case 'b':
			debug++;
			break;
		default:
			errflg++;
			break;
		}

	/* validate command line options */
	if (errflg) {
		(void)fprintf(stderr, "usage: %s\n", argv[0]);
		exit(1);
	}

	/* open the "sysmaster" database */
	$database sysmaster;
	if (sqlca.sqlcode)
		dberror("database sysmaster");

	/* gather some data from "sysmaster" and print it out */
	$prepare dbname_stmt from
	  "select name, owner, created, is_logging, is_buff_log, is_ansi, hex(partnum) from sysdatabases order by 7";
	if (sqlca.sqlcode)
		dberror("prepare dbname_stmt");

	$declare dbname_curs cursor for dbname_stmt;
	if (sqlca.sqlcode)
		dberror("declare dbname_curs");

	$open dbname_curs;
	if (sqlca.sqlcode)
		dberror("open dbname_curs");
	$fetch dbname_curs into $name, $owner, $created, $is_logging, $is_buff_log, $is_ansi, $partnum;
	if (sqlca.sqlcode < 0)
		dberror("fetch(1) dbname_curs");
	(void)printf("%-18.18s     %-8.8s     %-18.18s     %-10.10s    %s\n",
	  "Database Name", "Owner", "In Dbspace", "Created", "Status");
	(void)printf("------------------     --------     ------------------     ----------    ------\n");
	GetDbsnames();
	while (sqlca.sqlcode != SQLNOTFOUND) {
		ldchar(name, strlen(name), name);
		ldchar(owner, strlen(owner), owner);
		rdatestr(created, createdstr);
		if (is_logging)
			if (is_buff_log)
				logmode = 'B';
			else if (is_ansi)
				logmode = 'A';
			else
				logmode = 'U';
		else
			logmode = 'N';
		(void)printf("%-18.18s     %-8.8s     %-18.18s     %-10.10s      %c\n",
		  name, owner, dbsnames[ExtractDbsnum(partnum)], createdstr, logmode);
		$fetch dbname_curs into $name, $owner, $created, $is_logging, $is_buff_log, $is_ansi, $partnum;
		if (sqlca.sqlcode < 0)
			dberror("fetch(2) dbname_curs");
	}
	$close dbname_curs;
	if (sqlca.sqlcode)
		dberror("close dbname_curs");

	return(0);
}


void
GetDbsnames()
{
	$char	name[18+1];
	$short	dbsnum;

	$prepare dbsname_stmt from
	  "select dbsnum, name from sysdbspaces";
	if (sqlca.sqlcode)
		dberror("prepare dbsname_stmt");

	$declare dbsname_curs cursor for dbsname_stmt;
	if (sqlca.sqlcode)
		dberror("declare dbsname_curs");

	$open dbsname_curs;
	if (sqlca.sqlcode)
		dberror("open dbsname_curs");
	$fetch dbsname_curs into $dbsnum, $name;
	if (sqlca.sqlcode < 0)
		dberror("fetch(1) dbsname_curs");
	while (sqlca.sqlcode != SQLNOTFOUND) {
		ldchar(name, strlen(name), name);
		(void)strcpy(dbsnames[dbsnum], name);
		$fetch dbsname_curs into $dbsnum, $name;
		if (sqlca.sqlcode < 0)
			dberror("fetch(2) dbsname_curs");
	}
	$close dbsname_curs;
	if (sqlca.sqlcode)
		dberror("close dbsname_curs");
}


ExtractDbsnum(s)
char	*s;
{
	static char	buf[4];

	(void)sprintf(buf, "%.3s", s + 2);

	return(((isdigit((int)buf[0])) ? buf[0] - 48 : buf[0] - 55) * 256 +
	       ((isdigit((int)buf[1])) ? buf[1] - 48 : buf[1] - 55) * 16 +
	       ((isdigit((int)buf[2])) ? buf[2] - 48 : buf[2] - 55) * 1);
}


void
dberror(object)
char	*object;
{
	int	msglen;
	char	buf[BUFSIZ], errmsg[BUFSIZ];

	if (debug)
		(void)fprintf(stderr, "SQL statment: %s\n", object);

	(void)rgetlmsg(sqlca.sqlcode, errmsg, sizeof(errmsg), &msglen);
	(void)sprintf(buf, errmsg, sqlca.sqlerrm);
	(void)fprintf(stderr, "%d: %s", sqlca.sqlcode, buf);

	exit(1);
}


