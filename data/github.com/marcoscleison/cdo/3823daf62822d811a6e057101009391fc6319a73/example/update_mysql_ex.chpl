module Main{
use Cdo;
use Mysql;
use Map;

proc main(){
    //Open connection with Postgres database. Parametrs are host,username, database, password
        var con = MysqlConnectionFactory("localhost", "root", "teste", "krishna");        //Open a cursor
        var cursor = con.cursor();
    
// class holding the data 
    class MyContact:Model{
      var name:string;
      var email:string;
      proc init(name:string, email:string){
          this.name = name;
          this.email = email;
          this.setTable("contacts");
          this.setup(this);
      }
    }
    var obj = new unmanaged MyContact("Carlos2", "Carlos2@carclos.com");
    // Update object in database.
  writeln(cursor.updateRecord("contacts","id='5'" ,obj));
//Select
    cursor.query("SELECT * FROM contacts");
    //Get results
    for row in  cursor{
        writeln(row["id"]," ",row["name"]," ", row["email"]);
    }
// Data associatave array
    var data: map(string,string, parSafe=true);

    data["name"]="Maria Josef";
    data["email"]="maria@josef.com";

    // Update the db data
    writeln(cursor.update("contacts","id='6'" ,data));

//Select
    cursor.query("SELECT * FROM contacts");
    //Get results
    for row in  cursor{
        writeln(row["id"]," ",row["name"]," ", row["email"]);
    }
    
    cursor.close();
    con.close();
    writeln("End");
  }
}