/*
    Copyright (c) 2019, Salesforce.org
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of Salesforce.org nor the names of
      its contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/
@IsTest // (IsParallel=true) - Cannot run test in parallel when sending feature params to LMO
private class TelemetryService_TEST {
    private static TelemetryService telemetryService = new TelemetryService();
    private static Telemetry.Stub mockTelemetry = new Telemetry.Stub();
    private static final Integer TOTAL_TELEMETRIES = Telemetry.DeveloperName.values().size();

    @IsTest
    private static void configReturnsAllTelemetryRequests() {
        Integer actualTelemetries = telemetryService.telemetries.size();

        System.assertEquals(TOTAL_TELEMETRIES, actualTelemetries, 'Expected the config to return the total number of telemetries.');
    }

    @IsTest
    private static void sendMetricsSendsEachTelemetry() {
        telemetryService.telemetries = new List<Telemetry>();
        for (Integer i = 0; i < TOTAL_TELEMETRIES; i++) {
            telemetryService.telemetries.add((Telemetry) Test.createStub(Telemetry.class, mockTelemetry));
        }
        telemetryService.sendUsageMetrics();

        System.assertEquals(TOTAL_TELEMETRIES, mockTelemetry.sendCount, 'Expected each telemetry to be sent.');
    }

    @IsTest
    private static void sendMetricsIntegrationTest() {
        Campaign campaignRecord = new Campaign(
            IsActive = true,
            Status = 'In Progress',
            Name = 'Telemetry Campaign'
        );
        insert campaignRecord;

        Volunteer_Job__c job = new Volunteer_Job__c(
            Name = 'Telemetry Job',
            Campaign__c = campaignRecord.Id,
            Display_on_Website__c = true
        );
        insert job;

        Test.startTest();
        telemetryService.sendUsageMetrics();
        Test.stopTest();

        // Unable to assert the future job was added to async jobs in test context or the results of Feature Management.
        // We can only verify that an exception is not thrown.

    }


}
