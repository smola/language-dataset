import ceylon.html {
    Html,
    Doctype {
        html5
    },
    H2,
    Body,
    H1,
    Head,
    Div,
    Title
}
import ceylon.locale {
    locale
}
import ceylon.time {
    now
}
import ceylon.time.timezone {
    timeZone
}

import javax.servlet.http {
    HttpServlet,
    HttpServletRequest,
    HttpServletResponse
}

shared class SimpleServlet()
        extends HttpServlet() {
    shared actual void doGet(HttpServletRequest req, HttpServletResponse resp) {
        resp.contentType = "text/html";
        resp.writer.write(Html {
            doctype = html5;
            Head {
                Title { "Ceylon-OSGI Examples - Servlet with standard HttpService" }
            },
            Body {
               Div {
                   style = "text-align: center";
                   H1 { "Hello `` req.getParameter("name") else "Anybody" `` from Ceylon code" },
                   H2 {
                       "It's time to discover Ceylon-OSGI interoperability : `` 
                            locale(req.locale.toLanguageTag() else "en_US")
                               ?.formats?.longFormatTime(
                                    now().time(), timeZone.system) 
                            else "" ``"
                   }
               },
               Div {
                   style = "margin:auto; width:50%";
                   "This Ceylon module is also an OSGI bundle that provides a simple servlet,
                    registered at startup in the standard OSGI HTTP service."
               }
            }
        }.string);
    }
}
