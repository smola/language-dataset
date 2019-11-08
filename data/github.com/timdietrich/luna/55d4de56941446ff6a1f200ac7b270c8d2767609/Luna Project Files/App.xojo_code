#tag Class
Protected Class App
Inherits WebApplication
	#tag Event
		Function HandleURL(Request As WebRequest) As Boolean
		  // Create a new "Luna" APIRequest object for this request.
		  Dim APIRequest As new Luna(Request, SecureConnectionsRequired, DatabaseHost, DatabaseUserName, DatabasePassword, DatabaseName, DatabaseSchema)
		  
		  
		  // If this is a request for the root, or an error was encountered while preparing to process the request...
		  If (Request.Path = "") or (Request.Status <> 200) Then
		    Return True
		  End If
		  
		  
		  // If the request is not authenticated...
		  If not RequestAuthenticate(Request, APIRequest) Then 
		    Request.Status = 401
		    Return True
		  End If
		  
		  
		  // See if the app has a method that can process this request.
		  Dim method As Introspection.MethodInfo = APIRequest.AppMethodGet(self, Request)
		  
		  
		  // If a method was found...
		  If method <> nil Then
		    
		    // Create an array of parameters to use when calling the method.
		    Dim params() As Variant
		    
		    // Add the APIRequest to the params.
		    params.Append(APIRequest)
		    
		    // Invoke the method.
		    Dim Response As Dictionary = method.Invoke(self, params)
		    
		    // Set the request status and body.
		    Request.Status = Response.Value("ResponseStatus")
		    Request.Print(Response.Value("ResponseBody"))
		    
		  Else
		    Request.Status = 404
		    Request.Print( APIRequest.ErrorResponseCreate ( "404", "Unsupported API Version, Entity, and/or Method", "") )
		  End If
		  
		  
		  // Close the connection to the database.
		  #if UseMySQL
		    APIRequest.DatabaseConnection.Close
		  #elseif UsePostgreSQL
		    APIRequest.pgDatabaseConnection.Close
		  #endif
		  
		  
		  // Return True to avoid sending back the default 404 response.
		  Return True
		End Function
	#tag EndEvent


	#tag Method, Flags = &h0
		Function ContactsDeleteV1(APIRequest As Luna) As Dictionary
		  // Attempt to delete the record, and return the result.
		  // Note: The params being passed are the table name and the column name of the primary key.
		  Return APIRequest.SQLDELETEProcess("Contacts", "EmailAddress")
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function ContactsGetV1(APIRequest As Luna) As Dictionary
		  // If no record ID was specified...
		  //changed 2 to 1 in the next line because otherwise I only got results if I ended the request with a /
		  //ending the request with a slash to me does not look like expected functionality (maybe it worked correctly with MySQL?)
		  If APIRequest.RequestPathComponents.Ubound = 1 Then
		    #if UseMySQL
		      APIRequest.SQLStatement = APIRequest.DatabaseConnection.Prepare("SELECT " + APIRequest.SQLColumnsPrepare + " FROM Contacts")
		    #elseif UsePostgreSQL
		      APIRequest.pgSQLStatement = APIRequest.pgDatabaseConnection.Prepare("SELECT " + APIRequest.SQLColumnsPrepare + " FROM contacts")
		    #endif
		  Else
		    #if UseMySQL
		      APIRequest.SQLStatement = APIRequest.DatabaseConnection.Prepare("SELECT " + APIRequest.SQLColumnsPrepare + " FROM Contacts WHERE EmailAddress = ?")
		      APIRequest.SQLStatement.BindType(0, MySQLPreparedStatement.MYSQL_TYPE_STRING)
		      APIRequest.SQLStatement.Bind(0, APIRequest.RequestPathComponents(2))
		    #elseif UsePostgreSQL
		      APIRequest.pgSQLStatement = APIRequest.pgDatabaseConnection.Prepare("SELECT " + APIRequest.SQLColumnsPrepare + " FROM contacts WHERE emailaddress = $1")
		      APIRequest.pgSQLStatement.Bind(0, APIRequest.RequestPathComponents(2))
		    #endif
		  End If
		  
		  // Get and return the record.
		  Return APIRequest.SQLSELECTProcess
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function ContactsPatchV1(APIRequest As Luna) As Dictionary
		  Dim Response As New Dictionary
		  
		  
		  // Get the record to be updated.
		  #if UseMySQL
		    APIRequest.SQLStatement = APIRequest.DatabaseConnection.Prepare("SELECT * FROM Contacts WHERE EmailAddress = ?")
		    APIRequest.SQLStatement.BindType(0, MySQLPreparedStatement.MYSQL_TYPE_STRING)
		    APIRequest.SQLStatement.Bind(0, APIRequest.RequestPathComponents(2))
		  #elseif UsePostgreSQL
		    APIRequest.pgSQLStatement = APIRequest.pgDatabaseConnection.Prepare("SELECT * FROM contacts WHERE emailaddress = $1")
		    APIRequest.pgSQLStatement.Bind(0, APIRequest.RequestPathComponents(2))
		  #endif
		  Response = APIRequest.SQLSELECTProcess
		  
		  
		  // If the attempt to get the record has failed...
		  If Response.Value("ResponseStatus") <> 200 Then
		    // Abort the request.
		    Return Response
		  End If
		  
		  
		  // Convert the response body from text to JSON.
		  Dim CurrentRecord as new JSONITEM(Response.Value("ResponseBody"))
		  
		  
		  // An array of records is returned, so grab the first one.
		  CurrentRecord = CurrentRecord(0)
		  
		  
		  // For any value that could have been provided, but wasn't, use the current value...
		  If not APIRequest.RequestJSON.HasName("City") Then
		    APIRequest.RequestJSON.Value("City") = CurrentRecord.Value(GetFieldName("City"))
		  End If
		  If not APIRequest.RequestJSON.HasName("Company") Then
		    APIRequest.RequestJSON.Value("Company") = CurrentRecord.Value(GetFieldName("Company"))
		  End If
		  If not APIRequest.RequestJSON.HasName("Domain") Then
		    APIRequest.RequestJSON.Value("Domain") = CurrentRecord.Value(GetFieldName("Domain"))
		  End If
		  If not APIRequest.RequestJSON.HasName("EmailAddress") Then
		    APIRequest.RequestJSON.Value("EmailAddress") = CurrentRecord.Value(GetFieldName("EmailAddress"))
		  End If
		  If not APIRequest.RequestJSON.HasName("GivenName") Then
		    APIRequest.RequestJSON.Value("GivenName") = CurrentRecord.Value(GetFieldName("GivenName"))
		  End If
		  If not APIRequest.RequestJSON.HasName("Occupation") Then
		    APIRequest.RequestJSON.Value("Occupation") = CurrentRecord.Value(GetFieldName("Occupation"))
		  End If
		  If not APIRequest.RequestJSON.HasName("State") Then
		    APIRequest.RequestJSON.Value("State") = CurrentRecord.Value(GetFieldName("State"))
		  End If
		  If not APIRequest.RequestJSON.HasName("StreetAddress") Then
		    APIRequest.RequestJSON.Value("StreetAddress") = CurrentRecord.Value(GetFieldName("StreetAddress"))
		  End If
		  If not APIRequest.RequestJSON.HasName("Surname") Then
		    APIRequest.RequestJSON.Value("Surname") = CurrentRecord.Value(GetFieldName("Surname"))
		  End If
		  If not APIRequest.RequestJSON.HasName("TelephoneNumber") Then
		    APIRequest.RequestJSON.Value("TelephoneNumber") = CurrentRecord.Value(GetFieldName("TelephoneNumber"))
		  End If
		  If not APIRequest.RequestJSON.HasName("Title") Then
		    APIRequest.RequestJSON.Value("Title") = CurrentRecord.Value(GetFieldName("Title"))
		  End If
		  If not APIRequest.RequestJSON.HasName("ZipCode") Then
		    APIRequest.RequestJSON.Value("ZipCode") = CurrentRecord.Value(GetFieldName("ZipCode"))
		  End If
		  
		  
		  // Build the UPDATE statement.
		  #if UseMySQL
		    Dim sql As String = "UPDATE Contacts SET " _
		    + "City = ?, " _
		    + "Company = ?, " _
		    + "Domain = ?, " _
		    + "EmailAddress = ?, " _
		    + "GivenName = ?, " _
		    + "Occupation = ?, " _
		    + "State = ?, " _
		    + "StreetAddress = ?, " _
		    + "Surname = ?, " _
		    + "TelephoneNumber = ?, " _
		    + "Title = ?, " _
		    + "ZipCode = ? " _
		    + "WHERE " _
		    + "EmailAddress = ?"
		  #elseif UsePostgreSQL
		    Dim sql As String = "UPDATE contacts SET " _
		    + "city = $1, " _
		    + "company = $2, " _
		    + "domain = $3, " _
		    + "emailaddress = $4, " _
		    + "givenname = $5, " _
		    + "occupation = $6, " _
		    + "state = $7, " _
		    + "streetaddress = $8, " _
		    + "surname = $9, " _
		    + "telephonenumber = $10, " _
		    + "title = $11, " _
		    + "zipcode = $12 " _
		    + "WHERE " _
		    + "emailaddress = $13"
		  #endif
		  
		  
		  // Create the prepared statement.
		  #if UseMySQL Then
		    APIRequest.SQLStatement = APIRequest.DatabaseConnection.Prepare(sql)
		  #elseif UsePostgreSQL
		    APIRequest.pgSQLStatement = APIRequest.pgDatabaseConnection.Prepare(sql)
		  #endif
		  
		  // Specify the binding types.
		  // For additional BindType methods, see:
		  // http://docs.xojo.com/index.php/MySQLPreparedStatement
		  #if UseMySQL
		    APIRequest.SQLStatement.BindType(0, MySQLPreparedStatement.MYSQL_TYPE_STRING)
		    APIRequest.SQLStatement.BindType(1, MySQLPreparedStatement.MYSQL_TYPE_STRING)
		    APIRequest.SQLStatement.BindType(2, MySQLPreparedStatement.MYSQL_TYPE_STRING)
		    APIRequest.SQLStatement.BindType(3, MySQLPreparedStatement.MYSQL_TYPE_STRING)
		    APIRequest.SQLStatement.BindType(4, MySQLPreparedStatement.MYSQL_TYPE_STRING)
		    APIRequest.SQLStatement.BindType(5, MySQLPreparedStatement.MYSQL_TYPE_STRING)
		    APIRequest.SQLStatement.BindType(6, MySQLPreparedStatement.MYSQL_TYPE_STRING)
		    APIRequest.SQLStatement.BindType(7, MySQLPreparedStatement.MYSQL_TYPE_STRING)
		    APIRequest.SQLStatement.BindType(8, MySQLPreparedStatement.MYSQL_TYPE_STRING)
		    APIRequest.SQLStatement.BindType(9, MySQLPreparedStatement.MYSQL_TYPE_STRING)
		    APIRequest.SQLStatement.BindType(10, MySQLPreparedStatement.MYSQL_TYPE_STRING)
		    APIRequest.SQLStatement.BindType(11, MySQLPreparedStatement.MYSQL_TYPE_STRING)
		    APIRequest.SQLStatement.BindType(12, MySQLPreparedStatement.MYSQL_TYPE_STRING)
		  #endif
		  
		  
		  // Bind the values.
		  #if UseMySQL
		    APIRequest.SQLStatement.Bind(0, APIRequest.RequestJSON.Value("City"))
		    APIRequest.SQLStatement.Bind(1, APIRequest.RequestJSON.Value("Company"))
		    APIRequest.SQLStatement.Bind(2, APIRequest.RequestJSON.Value("Domain"))
		    APIRequest.SQLStatement.Bind(3, APIRequest.RequestJSON.Value("EmailAddress"))
		    APIRequest.SQLStatement.Bind(4, APIRequest.RequestJSON.Value("GivenName"))
		    APIRequest.SQLStatement.Bind(5, APIRequest.RequestJSON.Value("Occupation"))
		    APIRequest.SQLStatement.Bind(6, APIRequest.RequestJSON.Value("State"))
		    APIRequest.SQLStatement.Bind(7, APIRequest.RequestJSON.Value("StreetAddress"))
		    APIRequest.SQLStatement.Bind(8, APIRequest.RequestJSON.Value("Surname"))
		    APIRequest.SQLStatement.Bind(9, APIRequest.RequestJSON.Value("TelephoneNumber"))
		    APIRequest.SQLStatement.Bind(10, APIRequest.RequestJSON.Value("Title"))
		    APIRequest.SQLStatement.Bind(11, APIRequest.RequestJSON.Value("ZipCode"))
		    APIRequest.SQLStatement.Bind(12, APIRequest.RequestPathComponents(2))
		  #elseif UsePostgreSQL
		    APIRequest.pgSQLStatement.Bind(0, APIRequest.RequestJSON.Value("City"))
		    APIRequest.pgSQLStatement.Bind(1, APIRequest.RequestJSON.Value("Company"))
		    APIRequest.pgSQLStatement.Bind(2, APIRequest.RequestJSON.Value("Domain"))
		    APIRequest.pgSQLStatement.Bind(3, APIRequest.RequestJSON.Value("EmailAddress"))
		    APIRequest.pgSQLStatement.Bind(4, APIRequest.RequestJSON.Value("GivenName"))
		    APIRequest.pgSQLStatement.Bind(5, APIRequest.RequestJSON.Value("Occupation"))
		    APIRequest.pgSQLStatement.Bind(6, APIRequest.RequestJSON.Value("State"))
		    APIRequest.pgSQLStatement.Bind(7, APIRequest.RequestJSON.Value("StreetAddress"))
		    APIRequest.pgSQLStatement.Bind(8, APIRequest.RequestJSON.Value("Surname"))
		    APIRequest.pgSQLStatement.Bind(9, APIRequest.RequestJSON.Value("TelephoneNumber"))
		    APIRequest.pgSQLStatement.Bind(10, APIRequest.RequestJSON.Value("Title"))
		    APIRequest.pgSQLStatement.Bind(11, APIRequest.RequestJSON.Value("ZipCode"))
		    APIRequest.pgSQLStatement.Bind(12, APIRequest.RequestPathComponents(2))
		  #endif
		  
		  
		  // Execute the statement.
		  #if UseMySQL
		    APIRequest.SQLStatement.SQLExecute
		  #elseif UseMySQL
		    APIRequest.pgSQLStatement.SQLExecute
		  #endif
		  
		  // If an error was thrown...
		  Dim bError As Boolean=False
		  #if UseMySQL
		    bError=APIRequest.DatabaseConnection.Error
		  #elseif UsePostgreSQL
		    bError=APIRequest.pgDatabaseConnection.Error
		  #endif
		  If bError Then
		    Response.Value("ResponseStatus") = 500
		    #if UseMySQL
		      Response.Value("ResponseBody") = APIRequest.ErrorResponseCreate ( "500", "SQL UPDATE Failure", "Database error code: " + APIRequest.DatabaseConnection.ErrorCode.ToText) 
		    #elseif UsePostgreSQL
		      Response.Value("ResponseBody") = APIRequest.ErrorResponseCreate ( "500", "SQL UPDATE Failure", "Database error code: " + APIRequest.pgDatabaseConnection.ErrorCode.ToText) 
		    #endif
		    Return Response
		  End If
		  
		  
		  // Prepare the SQL and prepared statement to get the record that was just udpated.
		  #if UseMySQL
		    sql = "SELECT * FROM Contacts WHERE EmailAddress = ?"
		    APIRequest.SQLStatement = APIRequest.DatabaseConnection.Prepare(sql)
		    APIRequest.SQLStatement.BindType(0, MySQLPreparedStatement.MYSQL_TYPE_STRING)
		    APIRequest.SQLStatement.Bind(0, APIRequest.RequestJSON.Value("EmailAddress"))
		  #elseif UsePostgreSQL
		    sql = "SELECT * FROM contacts WHERE emailaddress = $1"
		    APIRequest.pgSQLStatement = APIRequest.pgDatabaseConnection.Prepare(sql)
		    APIRequest.pgSQLStatement.Bind(0, APIRequest.RequestJSON.Value("EmailAddress"))
		  #endif
		  
		  
		  // Return the updated record.
		  Return APIRequest.SQLSELECTProcess
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function ContactsPostV1(APIRequest As Luna) As Dictionary
		  Dim Response As New Dictionary
		  
		  
		  // Check to see that all of the expected values have been provided.
		  If not APIRequest.RequestJSON.HasName("EmailAddress") Then
		    Response.Value("ResponseStatus") = 400
		    Response.Value("ResponseBody") = APIRequest.ErrorResponseCreate ( "400", "Required column is missing", "EmailAddress is missing from the request body.")
		    Return Response
		  End If
		  
		  
		  // Build the INSERT statement.
		  #if UseMySQL
		    Dim sql As String = "INSERT INTO Contacts " _
		    + "( City, Company, Domain, EmailAddress, GivenName, Occupation, State, StreetAddress, Surname, TelephoneNumber, Title, ZipCode) " _
		    + "VALUES ( ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ? )"
		  #elseif UsePostgreSQL
		    Dim sql As String = "INSERT INTO contacts " _
		    + "( city, company, domain, emailaddress, givenname, occupation, state, streetaddress, surname, telephonenumber, title, zipcode) " _
		    + "VALUES ( $1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12 )"
		  #endif
		  
		  // Create the prepared statement.
		  #if UseMySQL
		    APIRequest.SQLStatement = APIRequest.DatabaseConnection.Prepare(sql)
		  #elseif UsePostgreSQL
		    APIRequest.pgSQLStatement = APIRequest.pgDatabaseConnection.Prepare(sql)
		  #endif
		  
		  // Specify the binding types.
		  // For additional BindType methods, see:
		  // http://docs.xojo.com/index.php/MySQLPreparedStatement
		  #if UseMySQL
		    APIRequest.SQLStatement.BindType(0, MySQLPreparedStatement.MYSQL_TYPE_STRING)
		    APIRequest.SQLStatement.BindType(1, MySQLPreparedStatement.MYSQL_TYPE_STRING)
		    APIRequest.SQLStatement.BindType(2, MySQLPreparedStatement.MYSQL_TYPE_STRING)
		    APIRequest.SQLStatement.BindType(3, MySQLPreparedStatement.MYSQL_TYPE_STRING)
		    APIRequest.SQLStatement.BindType(4, MySQLPreparedStatement.MYSQL_TYPE_STRING)
		    APIRequest.SQLStatement.BindType(5, MySQLPreparedStatement.MYSQL_TYPE_STRING)
		    APIRequest.SQLStatement.BindType(6, MySQLPreparedStatement.MYSQL_TYPE_STRING)
		    APIRequest.SQLStatement.BindType(7, MySQLPreparedStatement.MYSQL_TYPE_STRING)
		    APIRequest.SQLStatement.BindType(8, MySQLPreparedStatement.MYSQL_TYPE_STRING)
		    APIRequest.SQLStatement.BindType(9, MySQLPreparedStatement.MYSQL_TYPE_STRING)
		    APIRequest.SQLStatement.BindType(10, MySQLPreparedStatement.MYSQL_TYPE_STRING)
		    APIRequest.SQLStatement.BindType(11, MySQLPreparedStatement.MYSQL_TYPE_STRING)
		  #endif
		  
		  
		  // Bind the values.
		  #if UseMySQL
		    APIRequest.SQLStatement.Bind(0, APIRequest.RequestJSON.Value("City"))
		    APIRequest.SQLStatement.Bind(1, APIRequest.RequestJSON.Value("Company"))
		    APIRequest.SQLStatement.Bind(2, APIRequest.RequestJSON.Value("Domain"))
		    APIRequest.SQLStatement.Bind(3, APIRequest.RequestJSON.Value("EmailAddress"))
		    APIRequest.SQLStatement.Bind(4, APIRequest.RequestJSON.Value("GivenName"))
		    APIRequest.SQLStatement.Bind(5, APIRequest.RequestJSON.Value("Occupation"))
		    APIRequest.SQLStatement.Bind(6, APIRequest.RequestJSON.Value("State"))
		    APIRequest.SQLStatement.Bind(7, APIRequest.RequestJSON.Value("StreetAddress"))
		    APIRequest.SQLStatement.Bind(8, APIRequest.RequestJSON.Value("Surname"))
		    APIRequest.SQLStatement.Bind(9, APIRequest.RequestJSON.Value("TelephoneNumber"))
		    APIRequest.SQLStatement.Bind(10, APIRequest.RequestJSON.Value("Title"))
		    APIRequest.SQLStatement.Bind(11, APIRequest.RequestJSON.Value("ZipCode"))
		  #elseif UsePostgreSQL
		    APIRequest.pgSQLStatement.Bind(0, APIRequest.RequestJSON.Value("City"))
		    APIRequest.pgSQLStatement.Bind(1, APIRequest.RequestJSON.Value("Company"))
		    APIRequest.pgSQLStatement.Bind(2, APIRequest.RequestJSON.Value("Domain"))
		    APIRequest.pgSQLStatement.Bind(3, APIRequest.RequestJSON.Value("EmailAddress"))
		    APIRequest.pgSQLStatement.Bind(4, APIRequest.RequestJSON.Value("GivenName"))
		    APIRequest.pgSQLStatement.Bind(5, APIRequest.RequestJSON.Value("Occupation"))
		    APIRequest.pgSQLStatement.Bind(6, APIRequest.RequestJSON.Value("State"))
		    APIRequest.pgSQLStatement.Bind(7, APIRequest.RequestJSON.Value("StreetAddress"))
		    APIRequest.pgSQLStatement.Bind(8, APIRequest.RequestJSON.Value("Surname"))
		    APIRequest.pgSQLStatement.Bind(9, APIRequest.RequestJSON.Value("TelephoneNumber"))
		    APIRequest.pgSQLStatement.Bind(10, APIRequest.RequestJSON.Value("Title"))
		    APIRequest.pgSQLStatement.Bind(11, APIRequest.RequestJSON.Value("ZipCode"))
		  #endif
		  
		  // Execute the statement.
		  #if UseMySQL
		    APIRequest.SQLStatement.SQLExecute
		  #elseif UsePostgreSQL
		    APIRequest.pgSQLStatement.SQLExecute
		  #endif
		  
		  
		  // If an error was thrown...
		  Dim bError As Boolean=False
		  #if UseMySQL
		    bError=APIRequest.DatabaseConnection.Error
		  #elseif UsePostgreSQL
		    bError=APIRequest.pgDatabaseConnection.Error
		  #endif
		  If bError Then
		    Response.Value("ResponseStatus") = 500
		    #if UseMySQL
		      Response.Value("ResponseBody") = APIRequest.ErrorResponseCreate ( "500", "SQL INSERT Failure", "Database error code: " + APIRequest.DatabaseConnection.ErrorCode.ToText) 
		    #elseif UsePostgreSQL
		      Response.Value("ResponseBody") = APIRequest.ErrorResponseCreate ( "500", "SQL INSERT Failure", "Database error code: " + APIRequest.pgDatabaseConnection.ErrorCode.ToText) 
		    #endif
		    Return Response
		  End If
		  
		  
		  // Prepare the SQL and prepared statement to get the record that was just added.
		  #if UseMySQL
		    sql = "SELECT * FROM Contacts WHERE EmailAddress = ?"
		    APIRequest.SQLStatement = APIRequest.DatabaseConnection.Prepare(sql)
		    APIRequest.SQLStatement.BindType(0, MySQLPreparedStatement.MYSQL_TYPE_STRING)
		    APIRequest.SQLStatement.Bind(0, APIRequest.RequestJSON.Value("EmailAddress"))
		  #elseif UsePostgreSQL
		    sql = "SELECT * FROM contacts WHERE emailaddress = $1"
		    APIRequest.pgSQLStatement = APIRequest.pgDatabaseConnection.Prepare(sql)
		    APIRequest.pgSQLStatement.Bind(0, APIRequest.RequestJSON.Value("EmailAddress"))
		  #endif
		  
		  // Get the newly added record.
		  Response = APIRequest.SQLSELECTProcess
		  
		  
		  // Update the status to 201 Created.
		  Response.Value("ResponseStatus") = 201
		  
		  
		  Return Response
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function ContactsPutV1(APIRequest As Luna) As Dictionary
		  Dim Response As New Dictionary
		  Dim sql As String
		  
		  // Check to see that all of the expected values have been provided.
		  If not APIRequest.RequestJSON.HasName("EmailAddress") Then
		    Response.Value("ResponseStatus") = 400
		    Response.Value("ResponseBody") = APIRequest.ErrorResponseCreate ( "400", "Required column is missing", "EmailAddress is missing from the request body.")
		    Return Response
		  End If
		  
		  
		  // Get the record to be updated.
		  #if UseMySQL
		    APIRequest.SQLStatement = APIRequest.DatabaseConnection.Prepare("SELECT * FROM Contacts WHERE EmailAddress = ?")
		    APIRequest.SQLStatement.BindType(0, MySQLPreparedStatement.MYSQL_TYPE_STRING)
		    APIRequest.SQLStatement.Bind(0, APIRequest.RequestPathComponents(2))
		  #elseif UsePostgreSQL
		    APIRequest.pgSQLStatement = APIRequest.pgDatabaseConnection.Prepare("SELECT * FROM contacts WHERE emailaddress = $1")
		    APIRequest.pgSQLStatement.Bind(0, APIRequest.RequestPathComponents(2))
		  #endif
		  Response = APIRequest.SQLSELECTProcess
		  
		  // If the attempt to get the record has failed...
		  If Response.Value("ResponseStatus") <> 200 Then
		    // Abort the request.
		    Return Response
		  End If
		  
		  
		  // Build the UPDATE statement.
		  #if UseMySQL
		    sql = "UPDATE Contacts SET " _
		    + "City = ?, " _
		    + "Company = ?, " _
		    + "Domain = ?, " _
		    + "EmailAddress = ?, " _
		    + "GivenName = ?, " _
		    + "Occupation = ?, " _
		    + "State = ?, " _
		    + "StreetAddress = ?, " _
		    + "Surname = ?, " _
		    + "TelephoneNumber = ?, " _
		    + "Title = ?, " _
		    + "ZipCode = ? " _
		    + "WHERE " _
		    + "EmailAddress = ?"
		  #elseif UsePostgreSQL
		    sql = "UPDATE contacts SET " _
		    + "city = $1, " _
		    + "company = $2, " _
		    + "domain = $3, " _
		    + "emailaddress = $4, " _
		    + "givenname = $5, " _
		    + "occupation = $6, " _
		    + "state = $7, " _
		    + "streetaddress = $8, " _
		    + "surname = $9, " _
		    + "telephonenumber = $10, " _
		    + "title = $11, " _
		    + "zipcode = $12 " _
		    + "WHERE " _
		    + "emailaddress = $13"
		  #endif
		  
		  // Create the prepared statement.
		  #if UseMySQL
		    APIRequest.SQLStatement = APIRequest.DatabaseConnection.Prepare(sql)
		  #elseif UsePostgreSQL
		    APIRequest.pgSQLStatement = APIRequest.pgDatabaseConnection.Prepare(sql)
		  #endif
		  
		  // Specify the binding types.
		  // For additional BindType methods, see:
		  // http://docs.xojo.com/index.php/MySQLPreparedStatement
		  #if UseMySQL
		    APIRequest.SQLStatement.BindType(0, MySQLPreparedStatement.MYSQL_TYPE_STRING)
		    APIRequest.SQLStatement.BindType(1, MySQLPreparedStatement.MYSQL_TYPE_STRING)
		    APIRequest.SQLStatement.BindType(2, MySQLPreparedStatement.MYSQL_TYPE_STRING)
		    APIRequest.SQLStatement.BindType(3, MySQLPreparedStatement.MYSQL_TYPE_STRING)
		    APIRequest.SQLStatement.BindType(4, MySQLPreparedStatement.MYSQL_TYPE_STRING)
		    APIRequest.SQLStatement.BindType(5, MySQLPreparedStatement.MYSQL_TYPE_STRING)
		    APIRequest.SQLStatement.BindType(6, MySQLPreparedStatement.MYSQL_TYPE_STRING)
		    APIRequest.SQLStatement.BindType(7, MySQLPreparedStatement.MYSQL_TYPE_STRING)
		    APIRequest.SQLStatement.BindType(8, MySQLPreparedStatement.MYSQL_TYPE_STRING)
		    APIRequest.SQLStatement.BindType(9, MySQLPreparedStatement.MYSQL_TYPE_STRING)
		    APIRequest.SQLStatement.BindType(10, MySQLPreparedStatement.MYSQL_TYPE_STRING)
		    APIRequest.SQLStatement.BindType(11, MySQLPreparedStatement.MYSQL_TYPE_STRING)
		    APIRequest.SQLStatement.BindType(12, MySQLPreparedStatement.MYSQL_TYPE_STRING)
		  #endif
		  
		  // Bind the values.
		  #if UseMySQL
		    APIRequest.SQLStatement.Bind(0, APIRequest.RequestJSON.Value("City"))
		    APIRequest.SQLStatement.Bind(1, APIRequest.RequestJSON.Value("Company"))
		    APIRequest.SQLStatement.Bind(2, APIRequest.RequestJSON.Value("Domain"))
		    APIRequest.SQLStatement.Bind(3, APIRequest.RequestJSON.Value("EmailAddress"))
		    APIRequest.SQLStatement.Bind(4, APIRequest.RequestJSON.Value("GivenName"))
		    APIRequest.SQLStatement.Bind(5, APIRequest.RequestJSON.Value("Occupation"))
		    APIRequest.SQLStatement.Bind(6, APIRequest.RequestJSON.Value("State"))
		    APIRequest.SQLStatement.Bind(7, APIRequest.RequestJSON.Value("StreetAddress"))
		    APIRequest.SQLStatement.Bind(8, APIRequest.RequestJSON.Value("Surname"))
		    APIRequest.SQLStatement.Bind(9, APIRequest.RequestJSON.Value("TelephoneNumber"))
		    APIRequest.SQLStatement.Bind(10, APIRequest.RequestJSON.Value("Title"))
		    APIRequest.SQLStatement.Bind(11, APIRequest.RequestJSON.Value("ZipCode"))
		    APIRequest.SQLStatement.Bind(12, APIRequest.RequestPathComponents(2))
		  #elseif UsePostgreSQL
		    APIRequest.pgSQLStatement.Bind(0, APIRequest.RequestJSON.Value("City"))
		    APIRequest.pgSQLStatement.Bind(1, APIRequest.RequestJSON.Value("Company"))
		    APIRequest.pgSQLStatement.Bind(2, APIRequest.RequestJSON.Value("Domain"))
		    APIRequest.pgSQLStatement.Bind(3, APIRequest.RequestJSON.Value("EmailAddress"))
		    APIRequest.pgSQLStatement.Bind(4, APIRequest.RequestJSON.Value("GivenName"))
		    APIRequest.pgSQLStatement.Bind(5, APIRequest.RequestJSON.Value("Occupation"))
		    APIRequest.pgSQLStatement.Bind(6, APIRequest.RequestJSON.Value("State"))
		    APIRequest.pgSQLStatement.Bind(7, APIRequest.RequestJSON.Value("StreetAddress"))
		    APIRequest.pgSQLStatement.Bind(8, APIRequest.RequestJSON.Value("Surname"))
		    APIRequest.pgSQLStatement.Bind(9, APIRequest.RequestJSON.Value("TelephoneNumber"))
		    APIRequest.pgSQLStatement.Bind(10, APIRequest.RequestJSON.Value("Title"))
		    APIRequest.pgSQLStatement.Bind(11, APIRequest.RequestJSON.Value("ZipCode"))
		    APIRequest.pgSQLStatement.Bind(12, APIRequest.RequestPathComponents(2))
		  #endif
		  
		  // Execute the statement.
		  #if UseMySQL
		    APIRequest.SQLStatement.SQLExecute
		  #elseif UsePostgreSQL
		    APIRequest.pgSQLStatement.SQLExecute
		  #endif
		  
		  
		  // If an error was thrown...
		  Dim bError As Boolean=False
		  #if UseMySQL
		    bError=APIRequest.DatabaseConnection.Error
		  #elseif UsePostgreSQL
		    bError=APIRequest.pgDatabaseConnection.Error
		  #endif
		  If bError Then
		    Response.Value("ResponseStatus") = 500
		    #if UseMySQL
		      Response.Value("ResponseBody") = APIRequest.ErrorResponseCreate ( "500", "SQL UPDATE Failure", "Database error code: " + APIRequest.DatabaseConnection.ErrorCode.ToText) 
		    #elseif UsePostgreSQL
		      Response.Value("ResponseBody") = APIRequest.ErrorResponseCreate ( "500", "SQL UPDATE Failure", "Database error code: " + APIRequest.pgDatabaseConnection.ErrorCode.ToText) 
		    #endif
		    Return Response
		  End If
		  
		  
		  // Prepare the SQL and prepared statement to get the record that was just udpated.
		  #if UseMySQL
		    sql = "SELECT * FROM Contacts WHERE EmailAddress = ?"
		    APIRequest.SQLStatement = APIRequest.DatabaseConnection.Prepare(sql)
		    APIRequest.SQLStatement.BindType(0, MySQLPreparedStatement.MYSQL_TYPE_STRING)
		    APIRequest.SQLStatement.Bind(0, APIRequest.RequestJSON.Value("EmailAddress"))
		  #elseif UsePostgreSQL
		    sql = "SELECT * FROM contacts WHERE emailaddress = $1"
		    APIRequest.pgSQLStatement = APIRequest.pgDatabaseConnection.Prepare(sql)
		    APIRequest.pgSQLStatement.Bind(0, APIRequest.RequestJSON.Value("EmailAddress"))
		  #endif
		  
		  
		  // Return the updated record.
		  Return APIRequest.SQLSELECTProcess
		  
		  
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function GetFieldName(strFieldname As String) As String
		  #if UseMySQL
		    Return strFieldname
		  #elseif UsePostgreSQL
		    Return Lowercase(strFieldname)
		  #endif
		  
		End Function
	#tag EndMethod

	#tag Method, Flags = &h0
		Function RequestAuthenticate(Request As WebRequest, APIRequest As Luna) As Boolean
		  // Implement your authentication scheme here.
		  // Note: This is a *very* simple example of an authentication scheme.
		  
		  
		  // Get the Authorization header.
		  Dim AuthorizationHeader As String = Request.GetRequestHeader("Authorization")
		  
		  
		  // If the Authorization has not been specified correctly...
		  If InStr(0, AuthorizationHeader, "Bearer ") <> 1 Then
		    Return False
		  End if
		  
		  
		  // Remove the "Bearer" prefix from the value.
		  AuthorizationHeader = Replace(AuthorizationHeader, "Bearer ", "")
		  
		  
		  // In this case, we have a single, hard-coded key that needs to be passed.
		  Dim APIKey As String = "taWFk8Z4gR8oGoYtG+7Kycm97UswXW8i87T]HnjcNCGQJgi8JD"
		  
		  If AuthorizationHeader = APIKey Then
		    Return True
		  Else
		    Return False
		  End If
		End Function
	#tag EndMethod


	#tag Property, Flags = &h0
		DatabaseHost As String = "your.database.server.address"
	#tag EndProperty

	#tag Property, Flags = &h0
		DatabaseName As String = "your.database.name"
	#tag EndProperty

	#tag Property, Flags = &h0
		DatabasePassword As String = "your.database.account.password"
	#tag EndProperty

	#tag Property, Flags = &h0
		DatabaseSchema As String = "your.database.schema"
	#tag EndProperty

	#tag Property, Flags = &h0
		DatabaseUserName As String = "your.database.account.username"
	#tag EndProperty

	#tag Property, Flags = &h0
		SecureConnectionsRequired As Boolean = False
	#tag EndProperty


	#tag ViewBehavior
		#tag ViewProperty
			Name="DatabaseHost"
			Group="Behavior"
			InitialValue="internal-db.s156317.gridserver.com"
			Type="String"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
		#tag ViewProperty
			Name="DatabaseName"
			Group="Behavior"
			InitialValue="db156317_prefireplan"
			Type="String"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
		#tag ViewProperty
			Name="DatabasePassword"
			Group="Behavior"
			InitialValue="2jrFFBWn2c^Qb4o#jDbC^QYnTFnoLYhh6?RRtdbZLBoLNateFe"
			Type="String"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
		#tag ViewProperty
			Name="DatabaseSchema"
			Group="Behavior"
			InitialValue="your.database.schema"
			Type="String"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
		#tag ViewProperty
			Name="DatabaseUserName"
			Group="Behavior"
			InitialValue="db156317_prefire"
			Type="String"
			EditorType="MultiLineEditor"
		#tag EndViewProperty
		#tag ViewProperty
			Name="SecureConnectionsRequired"
			Group="Behavior"
			InitialValue="False"
			Type="Boolean"
		#tag EndViewProperty
	#tag EndViewBehavior
End Class
#tag EndClass
