module libpq is pkgconfig("libpq")

in "C header" `{
        #include <libpq-fe.h>
`}

in "C body" `{
        PGconn *conn;
        PGresult *res;
        int nFields, i, j;
`}

extern class Postgres
        new connect `{
            conn = PQconnectdb("dbname = postgres");
            return conn;
        `}

        fun exec(query: String) import String.to_cstring `{
            const char *c_query = String_to_cstring(query);
            res = PQexec(conn, c_query);
            nFields = PQnfields(res);
            for (i = 0; i < nFields; i++)
                printf("%-15s", PQfname(res, i));
            printf("\n\n");
            /* next, print out the rows */
            for (i = 0; i < PQntuples(res); i++)
            {
                for (j = 0; j < nFields; j++)
                    printf("%-15s", PQgetvalue(res, i, j));
                    printf("\n");
            }
        `}

        fun connect_ok: Bool `{
            return PQstatus(conn) == CONNECTION_OK;
        `}

        fun close `{
            PQfinish(conn);
        `}

end

var pg = new Postgres.connect
assert pg.connect_ok
pg.exec("select * from pg_catalog.pg_user;")
pg.close
assert not pg.connect_ok
