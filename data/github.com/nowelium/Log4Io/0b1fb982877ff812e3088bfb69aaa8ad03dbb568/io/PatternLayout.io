Log4Io PatternLayout := Log4Io Layout clone do(
    pattern ::= nil
    Formatter := Object clone do(
        loggingEvent ::= nil
        format := method(source,
            buf := Sequence clone asBuffer
            regex := Regex with("%(-?[0-9]+)?(\.?[0-9]+)?([cdmnpr%])(\{([^\}]+)\})?|([^%]+)")
            regex matchesIn(source) map(match,
                holder := match string
                value := if(hasSlot(holder), perform(holder), holder)
                buf appendSeq(value)
            )
            buf asString
        )
        setSlot("%c", method(
            loggingEvent categoryName
        ))
        setSlot("%d", method(
            format := Log4Io SimpleDateFormat with(Log4Io PatternLayout ISO8601_DATEFORMAT)
            format format(loggingEvent startTime)
        ))
        setSlot("%m", method(
            loggingEvent message
        ))
        setSlot("%n", method(
            "\n"
        ))
        setSlot("%p", method(
            loggingEvent level asString
        ))
        setSlot("%r", method(
            loggingEvent startTime asString
        ))
        setSlot("%%", method(
            "%"
        ))
    )
    with := method(pattern,
        c := self clone
        if(pattern isNil not) then (
            c pattern = pattern
        ) else (
            c pattern = Log4Io PatternLayout DEFAULT_CONVERSION_PATTERN
        )
        c
    )
    format := method(event,
        formatter := Formatter clone
        formatter setLoggingEvent(event)
        formatter format(pattern)
    )
)

Log4Io PatternLayout do (
    TTCC_CONVERSION_PATTERN := "%r %p %c - %m%n"
    DEFAULT_CONVERSION_PATTERN := "%m%n"

    ISO8601_DATEFORMAT := "yyyy-MM-dd HH:mm:ss,SSS"
    DATETIME_DATEFORMAT := "dd MMM YYY HH:mm:ss,SSS"
    ABSOLUTETIME_DATEFORMAT := "HH:mm:ss,SSS"
)

