// Copyright (c) 2018 WSO2 Inc. (http://www.wso2.org) All Rights Reserved.
//
// WSO2 Inc. licenses this file to you under the Apache License,
// Version 2.0 (the "License"); you may not use this file except
// in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied.  See the License for the
// specific language governing permissions and limitations
// under the License.

# Define the status.
# + createdAt - Created time of the status
# + id - Id of the status
# + text - Text message of the status
# + source - Source app of the status
# + truncated - Whether the status is truncated or not
# + inReplyToStatusId - The ID of an existing status that the update is in reply to
# + geo - Geo location details (longitude and latitude)
# + favorited - Whether the status is favorited or not
# + retweeted - Whether the status is retweeted or not
# + favouritesCount - Count of the favourites
# + retweetCount - Count of the retweeted status
# + lang - Language
public type Status record {
    string createdAt = "";
    int id = 0;
    string text = "";
    string 'source = "";
    boolean truncated = false;
    int inReplyToStatusId = 0;
    GeoLocation geo = {};
    boolean favorited = false;
    boolean retweeted = false;
    int favouritesCount = 0;
    int retweetCount = 0;
    string lang = "";
};

# Define the geo location details.
# + latitude - Latitude of the location
# + longitude - Longitude of the location
public type GeoLocation record {
    float latitude = 0.0;
    float longitude = 0.0;
};

# Define the location details.
# + woeid - Where On Earth IDentifier
# + countryName - Country name
# + countryCode - Country code
# + name - Name of the location
# + placeType - Longitude of the location
# + url - Location URL
public type Location record {
    int woeid = 0;
    string countryName = "";
    string countryCode = "";
    string name = "";
    PlaceType placeType = {};
    string url = "";
};

# Define the place type.
# + name - Name of the place
# + code - Location code of the place
public type PlaceType record {
    string name = "";
    int code = 0;
};

# Define the trends type.
# + trends - List of Trending object
# + location - List of Locations object
# + createdAt - Created time
public type Trends record {
    Trend[] trends = [];
    Location[] location = [];
    string createdAt = "";
};

# Define the trend type.
# + name - Name of trend object
# + url - URL of trend object
# + trendQuery - Query of the trend object
# + promotedContent - Promoted content
# + tweetVolume - Volume of the tweet
public type Trend record {
    string name = "";
    string url = "";
    string trendQuery = "";
    string promotedContent = "";
    int tweetVolume = 0;
};

# Define the search request.
# + tweetsCount - The number of tweets to return per page, up to a maximum of 100
public type SearchRequest record {
    string tweetsCount = "";
};
