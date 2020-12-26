<%@ Application Language="C#" %>

<script runat="server">

    void Application_Start(object sender, EventArgs e) 
    {
            // WCF
        System.Web.Routing.RouteTable.Routes.Add(new System.ServiceModel.Activation.ServiceRoute
            ("CrudSvc", new System.ServiceModel.Activation.WebServiceHostFactory(), typeof(gov.va.medora.mdws.CrudSvc)));
            // end WCF

        gov.va.medora.mdws.ApplicationSessions sessions = new gov.va.medora.mdws.ApplicationSessions();
        sessions.Start = DateTime.Now;
        Application.Lock();
        Application["APPLICATION_SESSIONS"] = sessions;
        Application.UnLock();
    }
    
    void Application_End(object sender, EventArgs e) 
    {
        // get sessions try/catch block
        gov.va.medora.mdws.ApplicationSessions sessions = null;
        try
        {
            sessions = Application["APPLICATION_SESSIONS"] as gov.va.medora.mdws.ApplicationSessions;
            if (sessions == null || sessions.Sessions == null || sessions.Sessions.Count == 0)
            {
                return;
            }
        }
        catch (Exception)
        {
            // TBD - should log the failure to a flat file maybe?
            return;
        }
        
        // finally, if we're logging then save the sessions to DB
        try
        {
            gov.va.medora.mdws.dao.sql.UsageDao dao = new gov.va.medora.mdws.dao.sql.UsageDao(sessions.ConfigurationSettings.SqlConnectionString);

            foreach (gov.va.medora.mdws.ApplicationSession session in sessions.Sessions.Values)
            {
                if (sessions.ConfigurationSettings.ApplicationSessionsLogging)
                {
                    session.End = session.Requests[session.Requests.Count - 1].ResponseTimestamp; // set end to last call time
                    gov.va.medora.mdws.dto.BoolTO success = dao.saveSession(session);
                    if (!success.trueOrFalse && success.fault != null)
                    {
                        // couldn't save to database - what should we do with these?
                    }
                    Application.Lock();
                    sessions.Sessions.Remove(session.AspNetSessionId);
                    Application.UnLock();
                }
            }
        }
        catch (Exception)
        {
            // just catch
        }
        finally
        {
            Application.UnLock();
        }
    }
        
    void Application_Error(object sender, EventArgs e) 
    { 
        // Code that runs when an unhandled error occurs

    }

    /// <summary>
    /// Attaches a custom filter to the response stream for obtaining the XML later
    /// </summary>
    /// <param name="sender"></param>
    /// <param name="e"></param>
    void Application_BeginRequest(object sender, EventArgs e)
    {
        try
        {
            gov.va.medora.mdws.ApplicationSessions sessions = Application["APPLICATION_SESSIONS"] as gov.va.medora.mdws.ApplicationSessions;
            if (sessions.ConfigurationSettings.ApplicationSessionsLogLevel == gov.va.medora.mdws.ApplicationSessionsLogLevel.info)
            {
                return; // don't do anything with request and response streams since we are just at info level
            }
            // apparently there is a bug in the framework where you must read this property to instantiate some internal stuff
            object bugWorkaround = Response.Filter;
            // this adds our custom filter to the response object so we can cache the response text for logging
            Response.Filter = new gov.va.medora.mdws.ResponseReader(Response.OutputStream);
            // JSONP support!
            if (!String.IsNullOrEmpty(Request.QueryString["callback"]))
            {
                ((gov.va.medora.mdws.ResponseReader)Response.Filter).JsonpCallback = Request.Params["callback"];
            }
            // end JSONP
        }
        catch (Exception)
        {
            // TBD: Do I care?   
        }
        finally
        {
            // cleanup
        }
    }

    /// <summary>
    /// When MDWS finished processing a request, it checks the log level. If we are debugging, MDWS will save the
    /// request and response bodies to the ApplicationRequest object for persisting to the database. If the 
    /// log level is set to "INFO", MDWS only saves basic information about the call for view on the dashboard
    /// </summary>
    /// <param name="sender"></param>
    /// <param name="e"></param>
    void Application_EndRequest(object sender, EventArgs e)
    {
        string sessionId = "";
        try
        {
            sessionId = Request.Params["ASP.NET_SessionId"]; // Session.SessionID is not available in Application_EndRequest context but we get this from the request parameters instead
            if (String.IsNullOrEmpty(sessionId))
            {
                return;
            }
        }
        catch (Exception)
        {
            return; // if we don't have session id we can't do anything
        }
        // get response from custom filter
        gov.va.medora.mdws.ResponseReader response = null;
        try
        {
            response = Response.Filter as gov.va.medora.mdws.ResponseReader;
        }
        catch (Exception) { /* nothing we can do - just make sure to check for null below */ }
        gov.va.medora.mdws.ApplicationRequest request = new gov.va.medora.mdws.ApplicationRequest();
        if (response != null)
        {
            request.RequestTimestamp = response.RequestTimestamp;
        }
        if (response == null)
        {
            request.RequestTimestamp = DateTime.Now; // needs to be set - ResponseReader instantiation was missed somehow
        }
        request.AspNetSessionId = sessionId;
        request.ResponseTimestamp = DateTime.Now;
        // if log level set to debug, get the request and response body of the HTTP/SOAP messages
        try
        {
            gov.va.medora.mdws.ApplicationSessions sessions = Application["APPLICATION_SESSIONS"] as gov.va.medora.mdws.ApplicationSessions;
            if (sessions.ConfigurationSettings.ApplicationSessionsLogLevel == gov.va.medora.mdws.ApplicationSessionsLogLevel.debug)
            {
                long placeHolder = Request.InputStream.Position;
                Request.InputStream.Position = 0;
                System.IO.TextReader reader = new System.IO.StreamReader(Request.InputStream);
                request.RequestBody = reader.ReadToEnd();
                Request.InputStream.Position = placeHolder; // reset stream position
                if (response != null)
                {
                    request.ResponseBody = response.ResponseString; // cached response string
                }
            }
            
            // second, get the request session and add the current request
            if (sessions.Sessions.ContainsKey(sessionId))
            {
                // will be namespaced SOAP action (e.g. http://mdws.medora.va.gov/getVHA)
                string soapAction = Request.Headers["SOAPAction"];
                // if request was a post and the SOAPAction header was used, the request came from a client app
                if (Request.HttpMethod.Equals("POST", StringComparison.CurrentCultureIgnoreCase) &&
                    !String.IsNullOrEmpty(soapAction))
                {
                    // read the request and response bodies for logging
                    // get the SOAP action header
                    char[] goofyChars = new char[] { '"' }; // SOAPAction is sometimes in quotes
                    string function = soapAction.Substring(soapAction.LastIndexOf('/'));
                    function = function.Trim(goofyChars);
                    request.Uri = new Uri(Request.Url.ToString() + function);
                }
                // these come from the test page's GETS and POSTS    
                else
                {
                    request.Uri = Request.Url;
                }
                Application.Lock();
                sessions.Sessions[sessionId].Requests.Add(request);
                Application.UnLock();
            }
        }
        catch (Exception)
        {
            // TBD - what to do here
        }
        finally
        {
            Application.UnLock();
        }
        // JSONP support - for some reason, IIS was not returning correct content-length... this is the only thing i could get working to fix it
        // so that the entire javascript function would be returned
        if (response.JsonpResponseLength > 0)
        {
            Response.ClearHeaders();
            Response.AppendHeader("Content-Length", response.JsonpResponseLength.ToString());
            Response.AppendHeader("Content-Type", "application/json; charset=utf-8");
            //NameValueCollection nvc = Response.Headers;
            //nvc["Content-Length"] = response.JsonpResponseLength.ToString();
        }
    }

   
    void Session_Start(object sender, EventArgs e) 
    {
        try
        {
            gov.va.medora.mdws.ApplicationSessions sessions = 
                Application["APPLICATION_SESSIONS"] as gov.va.medora.mdws.ApplicationSessions;
            gov.va.medora.mdws.ApplicationSession newSession = 
                new gov.va.medora.mdws.ApplicationSession(Session.SessionID, Request.UserHostAddress, DateTime.Now);
            try
            {
                newSession.LocalhostName = System.Net.Dns.GetHostName();
            }
            catch (Exception)
            {
                newSession.LocalhostName = "localhost";
            }

            if (!sessions.Sessions.ContainsKey(Session.SessionID))
            {
                Application.Lock();
                sessions.Sessions.Add(Session.SessionID, newSession);
                Application.UnLock();
            }
        }
        catch (Exception)
        {
            // just catch
        }
        finally
        {
            Application.UnLock();
        }
    }

    void Session_End(object sender, EventArgs e) 
    {
        try
        {
            // first save the session to the database
            gov.va.medora.mdws.ApplicationSessions sessions = 
                Application["APPLICATION_SESSIONS"] as gov.va.medora.mdws.ApplicationSessions;
            gov.va.medora.mdws.ApplicationSession session = sessions.Sessions[Session.SessionID];
            if (session != null)
            {
                session.End = session.Requests[session.Requests.Count - 1].ResponseTimestamp; // set end to last call time
                if (sessions.ConfigurationSettings.ApplicationSessionsLogging)
                {
                    gov.va.medora.mdws.dao.sql.UsageDao dao = new gov.va.medora.mdws.dao.sql.UsageDao(sessions.ConfigurationSettings.SqlConnectionString);
                    gov.va.medora.mdws.dto.BoolTO success = dao.saveSession(session);
                    if (!success.trueOrFalse && success.fault != null)
                    {
                        // couldn't save to database - what to do with data?
                    }
                }
                Application.Lock();
                // then remove the session from the global session table
                sessions.Sessions.Remove(Session.SessionID);
                Application.UnLock();
            }
        }
        catch (Exception)
        {
            // just catch
        }
        finally
        {
            Application.UnLock();
        }
    }

</script>
