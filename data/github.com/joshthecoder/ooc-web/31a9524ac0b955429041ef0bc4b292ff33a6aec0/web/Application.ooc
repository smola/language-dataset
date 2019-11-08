import io/[Reader, Writer]


Request: abstract class {
    path: String
    method: String
    remoteAddress: String
    remoteHost: String
    remotePort: String

    /**
        Returns the value for the HTTP request header for the given key.
    */
    getHeader: abstract func(name: String) -> String

    /**
        Returns a Reader object for reading the request body.
    */
    body: abstract func -> Reader
}

Response: abstract class {
    /**
        Set the HTTP response status code and message.
    */
    setStatus: abstract func(code: Int, message: String)

    /**
        Set a HTTP response header.
    */
    setHeader: abstract func(name: String, value: String)

    /**
        Returns a writer object used for sending
        the response body.

        Note: once you call this method, not further
        headers way be sent or the status code/message changed.
    */
    body: abstract func -> Writer
}

Application: abstract class {
    request: Request
    response: Response

    /**
        Invoked by the server when ever there is a request to be processed.
    */
    processRequest: abstract func

    /**
        This is called by the server when ever it spawns a new thread.

        If your application is not thread-safe, override this method
        to perform a "cloning" of the application to make it thread safe
        and return a reference to the cloned copy.

        :return: A copy of the Application to use in the new server thread.
    */
    spawn: func -> This { this }
}

