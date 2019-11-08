import ballerina/config;
import ballerina/log;
import ldclakmal/committer;

committer:CommitterReportConfiguration committerReportConfig = {
    githubToken: config:getAsString("GITHUB_TOKEN"),
    gmailAccessToken: config:getAsString("GOOGLE_ACCESS_TOKEN"),
    gmailClientId: config:getAsString("GOOGLE_CLIENT_ID"),
    gmailClientSecret: config:getAsString("GOOGLE_CLIENT_SECRET"),
    gmailRefreshToken: config:getAsString("GOOGLE_REFRESH_TOKEN")
};

committer:Client committerReportClient = new(committerReportConfig);

public function main() {
    string githubUser = "ldclakmal";
    var prDetails = committerReportClient->printPullRequestList(githubUser, committer:STATE_ALL);
    if (prDetails is error) {
        log:printError("Failed to print PR list", err = prDetails);
    }

    var issueDetails = committerReportClient->printIssueList(githubUser, committer:STATE_ALL);
    if (issueDetails is error) {
        log:printError("Failed to print issue list", err = issueDetails);
    }

    string userEmail = "b7a.demo@gmail.com";
    string[] excludeEmails = ["mygroup@abc.com"];
    var emailDetails = committerReportClient->printEmailList(userEmail, excludeEmails);
    if (emailDetails is error) {
        log:printError("Failed to print email list", err = emailDetails);
    }
}
