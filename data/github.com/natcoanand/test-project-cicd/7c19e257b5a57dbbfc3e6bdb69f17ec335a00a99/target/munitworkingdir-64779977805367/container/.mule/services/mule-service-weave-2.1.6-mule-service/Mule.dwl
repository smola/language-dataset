/**
* Module that contains functions that allows the user to interact with mule runtime
*/
%dw 2.0

/**
* Type that represents an Error
*/
type Error = {
    /**
    * Concise description of the error.
    */
    description?: String,
    /**
    * Detailed description of the error. This message may include Java exception specific information.
    */
    detailedDescription?: String,
    /**
    * Returns the type of the error
    */
    errorType?: ErrorType,
    /**
    * Lists any child Errors, if any.
    * For instance, the scatter-gather router may throw an error aggregating all of its routes errors as children.
    *
    * Not all failing components aggregate errors so this may return an empty collection.
    */
    childErrors?: Array<Error>
}

/**
* An ErrorType describes an type of error that may be thrown by a mule component.
*
* The error type has a string representation `identifier` which is used
* directly by the user in the mule configuration.
*
* Every error belongs to a `namespace`  in order to avoid collisions of error
* with the same string representation but that belong to different namespace.
*
* Error types may be an specialization of a more general error type in which case the `parentErrorType`
* should return the more general error type. This is used when doing error type matching within error handlers
* so when selecting the general error type for error handling it will also handle the more specialized error types.
*/
type ErrorType = {
    identifier?: String,
    namespace?: String,
    parentErrorType?: ErrorType
}

/**
*The lookup function allows for executing a flow within your app and retrieves the resulting payload, taking the flow’s name and an input payload as parameters.
*
* The specified flow is executed using the current attributes, variables, and error. It only replaces the payload.
*/
fun lookup(flowName: String, payload: Any, timeoutMillis: Number = 2000) =
    dw::mule::internal::Bindings::callFunction("", "lookup", [flowName, payload, timeoutMillis])


/**
* The `p` function provides access to properties, whether these are:
*
*  * Mule property placeholders
*  * System properties
*  * Environment properties
*/
fun p(propertyName: String): String =
    dw::mule::internal::Bindings::callFunction("", "p", [propertyName])

/*
* The causedBy function matches an error by its type, like an error handler does. This is useful when matching by a super type is required but specific sub-type logic also needed or when handling a COMPOSITE_ROUTING error that contains child errors of different types.
*
* The error to match against can be implicit, but the type is always a required parameter.
*
* In the following example, a global error handler is set up to handle SECURITY errors in a general way, while specific actions are set up for HTTP:UNAUTHORIZED and HTTP:FORBIDDEN errors.
**/
fun causedBy(error: Error, errorType: String): Boolean =
    dw::mule::internal::Bindings::callFunction("", "causedBy", [error, errorType])
