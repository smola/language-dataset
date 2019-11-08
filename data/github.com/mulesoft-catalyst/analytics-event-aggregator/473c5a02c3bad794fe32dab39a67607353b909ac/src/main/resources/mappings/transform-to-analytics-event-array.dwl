%dw 1.0
%output application/java

%function getEnvSuffix(environmentName)
  lower environmentName match {
    env: "production" -> "prod",
    env: "acceptance" -> "acc",
    default -> "dev"
  }

---
payload map ((event) -> {
  index: flowVars.config.splunkIndex,
  source: flowVars.config.splunkIndex ++ '_' ++ getEnvSuffix(flowVars.environmentName),
  sourcetype: flowVars.config.splunkSourceType,
  time: event.Timestamp as :datetime as :number {unit: "seconds"} as :string,
  event: {
    apiId: event."API ID",
    apiName: event."API Name",
    apiVersion: event."API Version Name" ++ ":" ++ event."API Version ID",
    apiVersionId: event."API Version ID",
    clientIp: event."Client IP",
    eventId: event."Message ID",
    orgId: flowVars.organizationId,
    path: event."Resource Path",
    policyViolation: event."Violated Policy Name",
    receivedTs: event.Timestamp,
    responseTime: event."Response Time",
    requestBytes: event."Request Size",
    requestDisposition: event."Request Outcome",
    responseBytes: event."Response Size",
    statusCode: event."Status Code",
    userAgent: event."User Agent Name",
    userAgentVersion: event."User Agent Version",
    browser: event.Browser,
    osFamily: event."OS Family",
    osMajorVersion: event."OS Major Version",
    osMinorVersion: event."OS Minor Version",
    osVersion: event."OS Version",
    city: event.City,
    hardwarePlatform: event."Hardware Platform",
    country: event.Country,
    continent: event.Continent,
    postalCode: event."Postal Code",
    verb: event.Verb
  }
})