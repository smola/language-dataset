package com.remington.ejercicios;

import org.aspectj.lang.annotation.Pointcut;
import java.sql.*;

public aspect BaseDeDatos {
	
    static String bd = "test";
    static String login = "root";
    static String password = "";
    static String url = "jdbc:mysql://localhost/"+bd;
    
    Connection connection = null;
    private ResultSet rs;
    private PreparedStatement st;
    
	pointcut basededatos():
		call(void com.remington.ejercicios.General.demo(*));
	
	after(): basededatos(){
		try{
			st = connection.prepareStatement("SELECT * FROM users");
			rs = st.executeQuery();
			System.out.println("| ID | Nombre |");
			while (rs.next()) {
				System.out.println("| " + rs.getString(1) + " | " + rs.getString(2) + " | ");
			}
			
		}catch(SQLException e){
	         System.out.println(e);
	    }catch(NullPointerException e){
	         System.out.println(e);
	    }catch(Exception e){
	         System.out.println(e);
	     }
	}
	
	before(): basededatos(){
		try{
	         Class.forName("com.mysql.jdbc.Driver");
	         connection = DriverManager.getConnection(url,login,password);
	         if (connection!=null){
	            System.out.println("Conexión a base de datos "+bd+" OK\n");
	         }
	      }
	      catch(SQLException e){
	         System.out.println(e);
	      }catch(ClassNotFoundException e){
	         System.out.println(e);
	      }catch(Exception e){
	         System.out.println(e);
	      }
	}
	
}
