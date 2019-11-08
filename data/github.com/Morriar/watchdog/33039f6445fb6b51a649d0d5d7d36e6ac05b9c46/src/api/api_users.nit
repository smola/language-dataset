# Copyright 2017 Alexandre Terrasa <alexandre@moz-code.org>.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

module api_users

import api_base
import cron

# API Users Router
class APIUsersRouter
	super Router

	# App config
	var config: AppConfig

	redef init do
		super
		use("/email", new APIUserEmail(config))
		use("/password", new APIUserPassword(config))
		use("/alerts", new APIUserAlerts(config))
	end
end

# /user/email
#
# GET: get email status
# POST: update email
# PUT: resend validation email
class APIUserEmail
	super APIHandler

	redef type BODY: EmailForm
	redef fun new_body_object(d) do return new EmailForm.from_deserializer(d)
	redef var validator is lazy do return null

	redef fun get(req, res) do
		var user = require_authentification(req, res)
		if user == null then return

		res.json new EmailForm(user.email, user.email_is_verified)
	end

	redef fun post(req, res) do
		var user = require_authentification(req, res)
		if user == null then return

		validator = new EmailValidator(config.auth_repo, user)
		var post = validate_body(req, res)
		if post == null then return
		var form = deserialize_body(req, res)
		if form == null then return

		user.email = form.email
		config.send_verification_email user
		res.json new EmailForm(user.email, user.email_is_verified)
	end

	redef fun put(req, res) do
		var user = require_authentification(req, res)
		if user == null then return

		if user.email_is_verified then
			res.message("Email already verified", 400)
			return
		end

		config.send_verification_email(user)
		res.message("Email verification send", 200)
	end
end

class EmailForm
	serialize

	# User email
	var email: String

	# Is this email verified?
	var is_valid: nullable Bool
end

class EmailValidator
	super ObjectValidator

	# AuthRepository used to check email unicity
	var auth_repo: AuthRepository

	# Current user
	var user: User

	redef init do
		add new UserUniqueEmailField("email", auth_repo=auth_repo, user=user, required=true)
	end
end

# Check email format and unicity
class UserUniqueEmailField
	super UniqueEmailField

	# Current user
	var user: User

	redef fun check_unicity(v, field, val) do
		var user = auth_repo.find_by_email(val)
		if user != null and user.login != self.user.login then
			v.validation.add_error(field, "Email `{val}` already used")
			return false
		end
		return true
	end
end

# /user/password
#
# POST: update the current user password
class APIUserPassword
	super APIHandler

	redef type BODY: PasswordForm
	redef fun new_body_object(d) do return new PasswordForm.from_deserializer(d)
	redef var validator is lazy do return new PasswordValidator

	redef fun post(req, res) do
		var user = require_authentification(req, res)
		if user == null then return
		var post = validate_body(req, res)
		if post == null then return
		var form = deserialize_body(req, res)
		if form == null then return

		var auth = config.try_credentials(user.login, form.old)
		if auth == null then
			res.message("Wrong password", 403)
			return
		end

		var creds = config.encrypt_password(form.password)
		user.password_hash = creds.first
		user.password_salt = creds.second
		config.auth_repo.save user
		res.message("Password updated", 200)
	end
end

class PasswordForm
	serialize

	# Old password
	var old: String

	# New password
	var password: String
end

class PasswordValidator
	super ObjectValidator

	redef init do
		add new StringField("old", min_size=6, max_size=255)
		add new StringField("password", min_size=6, max_size=255)
	end
end

# /user/alerts
#
# POST: update the current user alerts settings
class APIUserAlerts
	super APIHandler

	redef type BODY: AlertsForm
	redef fun new_body_object(d) do return new AlertsForm.from_deserializer(d)
	redef var validator is lazy do return new AlertsValidator

	redef fun post(req, res) do
		var user = require_authentification(req, res)
		if user == null then return
		var post = validate_body(req, res)
		if post == null then return
		var form = deserialize_body(req, res)
		if form == null then return

		user.alerts = form.alerts
		config.users.save user
		res.message("Alert updated", 200)
	end
end

class AlertsForm
	serialize

	# Activate alerts?
	var alerts: Bool
end

class AlertsValidator
	super ObjectValidator

	redef init do
		add new BoolField("alerts", required=true)
	end
end
