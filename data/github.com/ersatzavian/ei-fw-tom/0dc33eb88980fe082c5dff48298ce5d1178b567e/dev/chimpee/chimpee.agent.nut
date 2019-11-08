/*
Copyright (C) 2013 electric imp, inc.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software 
and associated documentation files (the "Software"), to deal in the Software without restriction, 
including without limitation the rights to use, copy, modify, merge, publish, distribute, 
sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is 
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial 
portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, 
INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE 
AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, 
DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, 
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/* Chimpee

 Tom Buttner, August 2013
 Electric Imp, inc
 tom@electricimp.com
*/

/* GLOBAL PARAMETERS AND FLAGS ----------------------------------------------*/

// size of chunks to pull from device when fetching new recorded message
const CHUNKSIZE = 8192;

// parameters read from WAV headers on inbound files
inParams <- {
    fmtChunkOffset = null,
    fmtChunkDataSize = null,
    dataChunkOffset = null,
    dataChunkSize = null,
    /* two supported compression codes:
        0x01 = 16-bit PCM
        0x06 = 8-bit ITU G.711 A-Law (yep, the imp does that)
    */
    compressionCode = null,
    /* character to use in blob operations; depends on sample width:
        'b' = 1 byte per sample (A-law)
        'w' = 2 bytes per sample (16-bit PCM)
    */
    width = null,
    // if the inbound file is multi-channel, we send only the first channel to the imp
    channels = null,
    samplerate = null,
    avgBytesPerSec = null,
    blockAlign = null,
    sigBits = null,
}

// global buffer for audio data; we keep this at global scope so that it can be asynchronously
// accessed by device event handlers
wavBlob <- blob(50000);
// new message flag so that we can respond appropriately when polled
newMessage <- false;
// pointer to mark position while downloading audio files
downloadPtr <- 0;
// url to fetch file from while sending down to imp
downloadURL <- "";

/* GENERAL FUNCTIONS --------------------------------------------------------*/

// clean up global parameters after downloading a new message to the device
function resetAfterDownload() {
    server.log("Agent: file downloaded to imp, resetting pointers.");
    downloadURL = "";
    downloadPtr = 0;
    inParams.fmtChunkOffset = null;
    inParams.fmtChunkDataSize = null;
    inParams.dataChunkOffset = null;
    inParams.dataChunkSize = null;
    inParams.compressionCode = null;
    inParams.width = null;
    inParams.channels = null;
    inParams.samplerate = null;
    inParams.avgBytesPerSec = null;
    inParams.blockAlign = null;
    inParams.sigBits = null;
}

// a bad, hacky function to copy a string into a blob. Used only shamefully.
function copyToBlob(str) {
    local myBlob = blob(str.len());
    for(local i = 0; i < str.len(); i++) {
        //server.log(format("0x%02x",str[i]));
        myBlob.writen(str[i],'b');
    }
    myBlob.seek(0,'b');
    return myBlob;
}

// parse the format chunk header on an inbound wav file 
function parseFmtChunk(chunk) {
    server.log("Parsing format data");
    chunk.seek(4,'b');

    inParams.fmtChunkDataSize = chunk.readn('i');
    server.log("Format chunk is "+inParams.fmtChunkDataSize+" bytes");
    inParams.compressionCode = chunk.readn('w');
    if (inParams.compressionCode == 0x01) {
        // 16-bit PCM
        inParams.width = 'w';
    } else if (inParams.compressionCode == 0x06) {
        // A-law
        inParams.width = 'b';
    } else {
        server.log(format("Audio uses unsupported compression code 0x%02x",
            inParams.compressionCode));
        return 1;
    }
    inParams.channels = chunk.readn('w');
    inParams.samplerate = chunk.readn('i');
    inParams.avgBytesPerSec = chunk.readn('i');
    inParams.blockAlign = chunk.readn('w');
    inParams.sigBits = chunk.readn('w');

    server.log(format("Compression Code: %x", inParams.compressionCode));
    server.log(format("Channels: %d",inParams.channels));
    server.log(format("Sample rate: %d", inParams.samplerate));

    return 0;
}

function fetch(url) {
    offset <- 0;
    server.log("Fetching content from "+url);
    do {
        response <- http.get(url, 
            {Range=format("bytes=%u-%u", offset, offset+CHUNKSIZE-1) }
        ).sendsync();
        got <- response.body.len();
        
        /* Since the response is a string, use string "find" to locate
        chunk offsets before we convert to a blob
        */
        local fmtOffset = response.body.find("fmt ");
        if (fmtOffset) {
            inParams.fmtChunkOffset = fmtOffset + offset;
            server.log("Located format chunk at offset "+inParams.fmtChunkOffset);
            server.log("Fetching complete format chunk");
            local fmtChunk = http.get(url, 
                {Range=format("bytes=%u-%u", fmtOffset, fmtOffset+25)}).sendsync().body;
            parseFmtChunk(copyToBlob(fmtChunk));
        }
        local dataOffset = response.body.find("data");
        if (dataOffset) {
            inParams.dataChunkOffset = dataOffset + offset;
            server.log("Located data chunk at offset "+inParams.dataChunkOffset);
            if ((dataOffset + 8) > got) {
                local dataHeader = http.get(url,
                    {Range=format("bytes=%u-%u", dataOffset, dataOffset+8)}).sendsync().body;
                inParams.dataChunkSize = ((response.body[7] << 24) | (response.body[6] << 16) | (response.body[5] << 8) | response.body[4]);
            } else {
                local dataHeader = response.body.slice(dataOffset, dataOffset+8);
                inParams.dataChunkSize = ((dataHeader[7] << 24) | (dataHeader[6] << 16) | (dataHeader[5] << 8) | dataHeader[4]);
            }
            server.log("Agent: Data Chunk of length "+inParams.dataChunkSize+" bytes");
        }  
        offset += got;
    } while (response.statuscode == 206 && inParams.dataChunkSize == 0);

    if (inParams.dataChunkSize == 0) {
        server.log("Agent: failed to parse wav file headers");
        return 1;
    } else {
        downloadPtr = inParams.dataChunkOffset + 8;
        downloadURL = url;
        device.send("newAudio", inParams);
    }
    
    server.log("Done, got "+offset+" bytes total");
}

/* AGENT EVENT HANDLERS -----------------------------------------------------*/

// Serve up a chunk of audio data from an inbound wav file when the device signals it is ready to download a chunk
device.on("pull", function(size) {
    local buffer = blob(size);
    local dlchunk = copyToBlob(http.get(downloadURL,{Range=format("bytes=%u-%u", downloadPtr, downloadPtr+size-1)}).sendsync().body);
    server.log("Fetched chunk of size "+dlchunk.len());

    // make a "sequence number" out of our position in audioData
    local chunkIndex = ((downloadPtr - inParams.dataChunkOffset) / size)+1;
    server.log("Agent: sending chunk "+chunkIndex+" of "+(inParams.dataChunkSize/size));
    
    // wav data is interlaced
    // skip channels if there are more than one; we'll always take the first
    local max = size;
    local bytesLeft = (inParams.dataChunkSize - (downloadPtr - inParams.dataChunkOffset + 8)) / inParams.channels;
    if (inParams.width == 'w') {
        // if we're A-law encoded, it's 1 byte per sample; if we're 16-bit PCM, it's two
        bytesLeft = bytesLeft * 2;
    }
    if (size > bytesLeft) {
        max = bytesLeft;
    }
    // the data chunk of a wav file is interlaced; the first sample for each channel, then the second for each, etc...
    // grab only the first channel if this is a multi-channel file
    // sending single-channel files is recommended as the agent's memory is constrained
    for (local i = 0; i < max; i += inParams.channels) {
        buffer.writen(dlchunk.readn(inParams.width), inParams.width);
    }
    //server.log(format("Wrote %d bytes to device", buffer.tell())); 

    // pack up the sequence number and the buffer in a table
    local data = {
        index = chunkIndex,
        chunk = buffer,
    }
    
    // send the data out to the device
    device.send("push", data);

    downloadPtr += size;
    //server.log(format("Download pointer at %d of %d", downloadPtr, (inParams.dataChunkSize + inParams.dataChunkOffset)));
    if (downloadPtr >= (inParams.dataChunkSize + inParams.dataChunkOffset)) {
        resetAfterDownload();
    } 
});

/* HTTP EVENT HANDLERS ------------------------------------------------------*/

http.onrequest(function(request, res) {
    server.log("Agent got new HTTP Request");
    // we need to set headers and respond to empty requests as they are usually preflight checks
    res.header("Access-Control-Allow-Origin", "*");
    res.header("Access-Control-Allow-Headers","Origin, X-Requested-With, Content-Type, Accept");
    res.header("Access-Control-Allow-Methods", "POST, GET, OPTIONS");

    if (request.path == "/newmsg") {
        local fetchURL = request.body;
        server.log("Agent: requested to fetch a new message from "+fetchURL);
        res.send(200, "OK");
        try {
            fetch(fetchURL);
        } catch (err) {
            server.log("Agent: failed to fetch new message");
            return 1;
        }
        server.log("Agent: done fetching message");
        // Notify the device we have audio waiting, and wait for a pull request to serve up data
        // device.send("newAudio", inParams);
    } else if (request.path == "/motor") {
        server.log("Agent got motor state set request: "+request.body);
        device.send("motor", request.body.tofloat());
        res.send(200, "OK");
    } else {
        // send a generic response to prevent browser hang
        res.send(200, "OK");
    }
});

/* EXECUTION BEGINS HERE ----------------------------------------------------*/

server.log("Chimpee agent running");
server.log("Agent: free memory: "+imp.getmemoryfree());
