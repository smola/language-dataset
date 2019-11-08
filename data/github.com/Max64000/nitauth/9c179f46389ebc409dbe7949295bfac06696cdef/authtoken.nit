module authtoken
import sqlite3
import md5
import sha1
import sendmail

redef class Sqlite3DB
	#attribut salt
	var salt = "secretsaltmontreal1a2z3e4r5t6y7u8i9o"
	#INITIALISATION
	init
	do
		create_tables_users
		create_tables_token
		create_table_change_password
	end

	#CREATE TABLE USERS
	private fun create_tables_users
	do
		assert create_table("IF NOT EXISTS user (id_user INTEGER PRIMARY KEY AUTOINCREMENT, email_user TEXT, username_user TEXT, password_user TEXT)") else
			print error or else "?"
		end
	end

	#CREATE TABLE TOKEN
	private fun create_tables_token
	do
		assert create_table("IF NOT EXISTS user_token (id_user INTEGER, token_user TEXT)") else
			print error or else "?"
		end
	end

	#CREATE TABLE CHANGE PASSWORD
	private fun create_table_change_password
	do
		assert create_table("IF NOT EXISTS user_change_password (id_user INTEGER, id_change_password TEXT)") else
			print error or else "?"
		end
	end


	#CHECK IF ACCOUNT EXIST EMAIL AND USERNAME
	private fun check_account_exist(email,username : String) : Bool
	do
		var result:Bool = false
		var stmt = select("email_user, username_user FROM user WHERE email_user={email.to_sql_string} OR username_user={username.to_sql_string}")
		
		assert stmt != null else print error or else "?"

		for row in stmt do
			if row[0].to_s == email or row[1].to_s == username then
				result = true
			end
		end
		return result
	end

	#CREATE TOKEN
	private fun createToken: String
	do
		var token1 = 123456.rand.to_s
		var token2 = 123456.rand.to_s

		var token = token1 + token2
		token = token.md5
		return token
	end

	#ADD NEW ACCOUNT BY EMAIL USERNAME AND PASSWORD
	fun add_user_account(email,username,password : String) : Bool
	do
		#password sécurité
		password = password + salt
		password = password.sha1_hexdigest
		#compte exist
		if check_account_exist(email,username) == false then
			assert insert("INTO user(email_user, username_user, password_user) VALUES ({email.to_sql_string}, {username.to_sql_string}, {password.to_sql_string})") else
				print error or else "?"
			end
			return true
		end

		return false
	end

	#LOGIN ACCOUNT BY EMAIL OR USERNAME AND PASSWORD
	fun login(email,username,password :String) : String
	do
		var id_user = null
		var result = false

		password = password + salt
		password = password.sha1_hexdigest

		var stmt = select("id_user, email_user, username_user, password_user FROM user WHERE (email_user={email.to_sql_string} AND password_user={password.to_sql_string}) OR (username_user={username.to_sql_string} AND password_user={password.to_sql_string})")
		assert stmt != null else print error or else "?"
		for row in stmt do
			if row[1].to_s == email and row[3].to_s == password then
				result = true
				id_user = row[0].to_s 
			else if row[2].to_s == username and row[3].to_s == password then
				result = true
				id_user = row[0].to_s 
			end
		end

		if result == true then
			assert id_user != null else return "false"

			var token = createToken

			assert insert("INTO user_token(id_user, token_user) VALUES ({id_user}, {token.to_sql_string})") else
				print error or else "?"
			end
			
			
			return token
		end
		

		return "false"
	end

	#GET EMAIL BY IDUSER
	fun getEmailUser(iduser: String) : String
	do	
		var stmt = select("email_user FROM user WHERE id_user = {iduser.to_sql_string}")
		assert stmt != null else print error or else "?"
		for row in stmt do
			return row[0].to_s
		end
		return ""
	end

	#GET IDUSER BY EMAIL OR USERNAME
	fun getIdUserByEmailOrUsername(emailorusername : String) : String
	do
		var stmt = select("id_user,email_user,username_user FROM user WHERE (email_user = {emailorusername.to_sql_string} OR username_user = {emailorusername.to_sql_string})")
		assert stmt != null else print error or else "?"
		for row in stmt do
			if row[1].to_s == emailorusername then
				return row[0].to_s
			end
		end
		return ""
	end


	#LOOGOUT ACCOUNT BY TOKEN
	fun logout(token : String) : Bool
	do
		var resultExistUser = check_token(token)
		if resultExistUser != "false"
		then
			assert execute("DELETE FROM user_token WHERE token_user = {token.to_sql_string}") else
				print error or else "?"
			end
			return true
		end

		return false
	end
	
	#CHECK ACCOUNT EXIST BY TOKEN USER
	fun check_token(token : String) : String
	do
		var stmt = select("token_user,id_user FROM user_token WHERE token_user = {token.to_sql_string}")
		
		assert stmt != null else print error or else "?"
		for row in stmt do
			if row[0].to_s == token then
				return row[1].to_s
			end
		end
		return "false"
	end

	#CHANGE OLDPASSWORD BY NEW PASSWORD
	fun changepassword(idUser, oldPassword, newPassword : String) : Bool
	do
		var validinfo:Bool = false
		oldPassword = oldPassword + salt
		oldPassword = oldPassword.sha1_hexdigest

		var stmt = select("id_user, password_user FROM user WHERE id_user={idUser}")
		
		assert stmt != null else print error or else "?"
		for row in stmt do
			if row[0].to_s == idUser and row[1].to_s == oldPassword then
				validinfo = true
 			end
		end

		if validinfo == true then
			newPassword = newPassword + salt
			newPassword = newPassword.sha1_hexdigest

			assert execute("UPDATE user SET password_user={newPassword.to_sql_string} WHERE id_user = {idUser.to_sql_string}") else
				print error or else "?"
			end
			return true 
		else
			return false
		end

	end

	#GENERATION TABLE PASSWORD GENERE WITH IDUSER
	fun change_password_genere(iduser: String) : String
	do
		var token = createToken
		assert insert("INTO user_change_password(id_user, id_change_password) VALUES ({iduser}, {token.to_sql_string})") else
			print error or else "?"
		end

		return token
	end

	#CHANGE PASSWORD WITH TOKENCP AND PASSWORD
	fun change_password(tokencp, password : String) : Bool
	do
		var stmt = select("id_user, id_change_password FROM user_change_password WHERE id_change_password={tokencp.to_sql_string}")
		

		assert stmt != null else print error or else "?"

		for row in stmt do
			if row[1].to_s == tokencp then
			
				var iduser = row[0].to_s
				password = password + salt
				password = password.sha1_hexdigest

				assert execute("UPDATE user SET password_user={password.to_sql_string} WHERE id_user = {iduser.to_sql_string}") else
					print error or else "?"
				end

				assert execute("DELETE FROM user_change_password WHERE id_change_password = {tokencp.to_sql_string}") else
					print error or else "?"
				end

				return true

			end
		end

		return false
	end

	#TOKENCP EXIST ?
	fun check_token_cp(tokencp: String) : Bool
	do
		var stmt = select("id_change_password FROM user_change_password ")

		assert stmt != null else print error or else "?"

		for row in stmt do
			if tokencp == row[0].to_s then
				return true
			end
		end

		return false
	end



	fun sendMail(mymail : MyMail)
	do
		if sendmail_is_available then
		    var mail = new Mail("{mymail.usermail}", "{mymail.titre}", "{mymail.contenu}")
		    	mail.to.add "{mymail.usermail}"
		    	mail.send
		else print "please install sendmail"
		
	end

	#LIST USERS
	fun list_users : Array[Users]
	do
		var stmt = select("id_user, email_user, username_user FROM user")
		assert stmt != null else print error or else "?"
		
		var users = new Array[Users]

		for row in stmt do users.add new Users(row[0].to_s, row[1].to_s, row[2].to_s)
		
		return users

	end


	
end

class Users
	var iduser: String
	var email : String
	var username : String
end

class MyMail
	var usermail : String
	var titre : String
	var contenu : String
end