IMPORT Std;

EXPORT Util := MODULE

    EXPORT AllDatesInRange(Std.Date.Date_t date1, Std.Date.Date_t date2) := FUNCTION
        firstDate := MIN(date1, date2);
        lastDate := MAX(date1, date2);
        daysBetween := Std.Date.DaysBetween(firstDate, lastDate);
        allDates := DATASET
            (
                daysBetween + 1,
                TRANSFORM
                    (
                        {
                            Std.Date.Date_t d
                        },
                        SELF.d := Std.Date.AdjustDate(firstDate, day_delta := COUNTER - 1)
                    )
            );
        
        RETURN allDates;
    END;

    EXPORT AllHours() := FUNCTION
        RETURN DATASET
            (
                24,
                TRANSFORM
                    (
                        {
                            UNSIGNED1   h
                        },
                        SELF.h := COUNTER - 1
                    )
            );
    END;

    EXPORT AllDaysAllHours(Std.Date.Date_t date1, Std.Date.Date_t date2) := JOIN
        (
            AllDatesInRange(date1, date2),
            AllHours(),
            TRUE,
            TRANSFORM
                (
                    {
                        Std.Date.Date_t     date,
                        UNSIGNED1           hour
                    },
                    SELF.date := LEFT.d,
                    SELF.hour := RIGHT.h
                ),
            ALL
        );
    
    EXPORT DescribeDayOfWeek(UNSIGNED1 dayOfWeekNum) := CHOOSE
        (
            dayOfWeekNum,
            'Sunday',
            'Monday',
            'Tuesday',
            'Wednesday',
            'Thursday',
            'Friday',
            'Saturday',
            '<Unknown>'
        );
    
    EXPORT HolidayDates := DATASET
        (
            [
                {20150101},
                {20150119},
                {20150216},
                {20150525},
                {20150703},
                {20150907},
                {20151012},
                {20151111},
                {20151126},
                {20151225},
                {20160101},
                {20160118},
                {20160215},
                {20160530}
            ],
            {Std.Date.Date_t holiday}
        );
    
    EXPORT boroughsBoundingBoxes := DATASET
        (
            [
                {1, 'The Bronx', 40.917577, 40.785743, -73.748060, -73.933808},
                {2, 'Manhattan', 40.882214, 40.680396, -73.907000, -74.047285},
                {3, 'Brooklyn', 40.739446, 40.551042, -73.833365, -74.056630},
                {4, 'Staten Island', 40.651812, 40.477399, -74.034547, -74.259090},
                {5, 'Queens', 40.812242, 40.489794, -73.700272, -73.833365}
            ],
            {
                UNSIGNED1   id;
                STRING      burroughs_name;
                DECIMAL9_6  north;
                DECIMAL9_6  south;
                DECIMAL9_6  east;
                DECIMAL9_6  west;
            }
        );
    
    EXPORT precipTypes := DATASET
        (
            [
                {0, ''},
                {1, 'rain'},
                {2, 'snow'}
            ],
            {UNSIGNED1 id, STRING precipType}
        );
    
    EXPORT precipTypeDictionary := DICTIONARY(precipTypes, {precipType => id});

END;
