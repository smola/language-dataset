component {
	this.name = 'lucee-aws_' & Hash( GetCurrentTemplatePath() );

	this.mappings[ '/testbox' ] = ExpandPath( '../testbox' );
	this.mappings[ '/tests'   ] = ExpandPath( './' );
	this.mappings[ '/aws'   ] = ExpandPath( '../' );

	public boolean function onApplicationStart() {

		var env = CreateObject( 'java' , 'java.lang.System' ).getenv();
			
	        application.aws_settings = {
	            'aws_accountid': '',	// self explanatory, DO NOT COMMIT TO A PUBLIC REPO
	            'aws_secretkey': '', 	// self explanatory, DO NOT COMMIT TO A PUBLIC REPO
	            'dynamodb_table': '', 	// a dynamodb table with some keys that i can't remember
	            'elb_name': '', 		// subdomain of a loadbalancer
	            'elb_region': '', 		// region that the elb_name is in
	            'lambda_method': '', 	// name of a lambda method that takes in {key1='',key2='',key3=''} and returns a concat string
	            'route53_tld': '', 		// a hosted zone domain name i.e. something.com
	            'ses_from': '', 		// email address that can send from SES for you
	            'ses_to': '',			// email address that can receive email from SES for you
	            's3_bucket': ''  		// name of an s3 bucket
	        };

		for( key in env ) {
			if (
				StructKeyExists( application.aws_settings , key )
			) {
				application.aws_settings[ key ] = env[ key ];
			}
		}

		return true;
	}

}
