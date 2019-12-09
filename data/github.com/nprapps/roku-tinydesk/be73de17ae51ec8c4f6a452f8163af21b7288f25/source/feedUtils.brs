'
' Utils related to fetching the feed.
'

' Fetch and parse the video feed
function fetchFeed() as Object 

    feed = httpGetWithRetry("http://apps.npr.org/roku-tinydesk/feed.json", 3000, 3)

    if feed = "" then
        return invalid
    endif

    return ParseJSON(feed)

end function

' Set last watch timestamp in the registry
function setLastWatched(contentItem)

    now = createObject("roDateTime").asSeconds().toStr()
    RegWrite(contentItem.id, now, "recent")

    return now.toInt()

end function

' Get the timestamp the  video was last watched
function getLastWatched(contentItem)

    lastWatched = RegRead(contentItem.id, "recent")
    
    if lastWatched = invalid
        return invalid
    end if
    
    return lastWatched.toInt()

end function

' Mark a video watched in the registry
function markAsFinished(contentItem)

    RegWrite(contentItem.id, "true", "watched")

end function

' Check the registry to see if a feed item has been watched
function isFinished(contentItem) as Boolean

    read = RegRead(contentItem.id, "watched")

    return read = "true"

end function

' Save playback position
Function savePosition(contentItem, position)

    RegWrite(contentItem.id, position.toStr(), "position")

End Function

' Load playback position
Function loadPosition(contentItem) as Integer

    position = RegRead(contentItem.id, "position")
    
    if position = invalid then
        return 0
    end if

    return position.toInt()

End Function
