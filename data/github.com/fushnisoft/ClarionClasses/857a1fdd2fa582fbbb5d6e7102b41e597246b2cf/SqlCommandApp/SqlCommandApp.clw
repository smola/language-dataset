  PROGRAM
  Include('ConsoleSupport.inc'),ONCE
  Include('SqlCommand.inc'),ONCE
  MAP
  END

Console   Class(ConsoleSupport)
WriteBlock  PROCEDURE(STRING pHeading, STRING pContent)
          END

Sql   SqlCommand
response CSTRING(51)
  CODE
  IF Console.Init() 
    Halt()
  END

  Console.WriteLine('*** SqlCommand Test App! ***')
  ! =======================
  Console.WriteLine('Connecting...')
  Sql.Init('/TRUSTEDCONNECTION=TRUE /LOGONSCREEN=FALSE /TURBOSQL=TRUE','beto\sql2012,master,,')
  
  !Get the datetime from the server 
  Sql.Str('SELECT GetDate()')
  Console.WriteBlock('Executing SQL', Sql.Str())
  Console.WriteBlock('Response', Sql.ExecuteReader())
  
  
  !Set a global temp table and do some stuff?
  Sql.Str('IF object_id(''tempdb..##SqlCommand'') IS NOT NULL DROP TABLE ##SqlCommand')
  Console.WriteBlock('Executing NonQuery', Sql.Str())
  Sql.ExecuteNonQuery()

  Sql.Str('CREATE TABLE ##SqlCommand (Id int, Name varchar(50))')
  Console.WriteBlock('Executing NonQuery', Sql.Str())
  Sql.ExecuteNonQuery()

  Sql.Str('INSERT INTO ##SqlCommand VALUES (1, ''Gabriel Ellis'')')
  Console.WriteBlock('Executing NonQuery', Sql.Str())
  Sql.ExecuteNonQuery()
  Sql.Str('INSERT INTO ##SqlCommand VALUES (2, ''Finley Smith'')')
  Sql.ExecuteNonQuery()
  Sql.Str('INSERT INTO ##SqlCommand VALUES (3, ''Jordan Simpson'')')
  Sql.ExecuteNonQuery()
  Sql.Str('INSERT INTO ##SqlCommand VALUES (4, ''Josh Johnson'')')
  Sql.ExecuteNonQuery()
  Sql.Str('INSERT INTO ##SqlCommand VALUES (5, ''William Cox'')')
  Sql.ExecuteNonQuery()
  Sql.Str('INSERT INTO ##SqlCommand VALUES (6, ''Benton Bell'')')
  Sql.ExecuteNonQuery()
  Sql.Str('INSERT INTO ##SqlCommand VALUES (7, ''Tate Vazquez'')')
  Sql.ExecuteNonQuery()
  Sql.Str('INSERT INTO ##SqlCommand VALUES (8, ''Ahmad Downs'')')
  Sql.ExecuteNonQuery()
  Sql.Str('INSERT INTO ##SqlCommand VALUES (9, ''Malaki Marks'')')
  Sql.ExecuteNonQuery()
  Sql.Str('INSERT INTO ##SqlCommand VALUES (10, ''Ronald Rowland'')')
  Sql.ExecuteNonQuery()

  Sql.Str('SELECT Name FROM ##SqlCommand WHERE Id=1')
  Console.WriteBlock('Executing SQL', Sql.Str())
  Console.WriteBlock('Response', Sql.ExecuteReader())

  ! How about looping through the results then eh?
  Sql.Open()
  Sql.Str('SELECT Name FROM ##SqlCommand ORDER BY Name')
  Console.WriteBlock('Executing SQL, then looping over results!', Sql.Str())
  Sql.ExecuteReader()
  LOOP 
    response = Sql.Read()
    IF response = ''
      BREAK
    END
    Console.WriteLine(response)
  END
  Sql.Close()
  
  Sql.Str('IF object_id(''tempdb..##SqlCommand'') IS NOT NULL DROP TABLE ##SqlCommand')  
  Console.WriteBlock('Executing NonQuery', Sql.Str())
  Sql.ExecuteNonQuery()

  ! =======================
  Console.ReadKey()
  
Console.WriteBlock    PROCEDURE(STRING pHeading, STRING pContent)
  CODE
  SELF.WriteLine('<13,10>' & pHeading & ':<13,10>' & |
  '<13,10>' & |
  pContent & '<13,10>' & |
  '--------------')
  RETURN
  