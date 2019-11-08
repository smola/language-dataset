
class utils.SocketClient inherits utils.Socket :
   define variable lRC as logical no-undo.
   define variable mHeader as memptr no-undo.
   define variable mData as memptr no-undo.
   define variable iDataSize as integer no-undo.

   constructor public SocketClient (host as char, port_no as char):
      create socket connection_handle.
      connection_handle:connect("-H " + host + " -S " + port_no).

      connection_handle:set-read-response-procedure('process_io', callback_handle).
      
      /*--------------------------------------------------------*/
      /* Allocate 4 byte header block once (for performance) */
      /* and specify the order of the bytes within the pointer */
      /* which is needed to guarantee proper interpretation of */
      /* the data between machines whose CPU's have different */
      /* byte orders */
      /*--------------------------------------------------------*/
      SET-SIZE(mHeader) = 4.
      SET-BYTE-ORDER(mHeader) = big-endian.
      
      kickStart().      
   end constructor.
      

   method private void kickStart ():
   /*----------------------------------------------------*/
   /* Fill the memory pointer with data (header + the */
   /* real data) and write the data to the socket */
   /*----------------------------------------------------*/
      if iDataSize < 13 then
      do:
         SET-SIZE(mData) = 0.
         SET-SIZE(mData) = 13.
         iDataSize = 13.
         SET-BYTE-ORDER(mData) = big-endian.
      end.
      
      PUT-LONG(mData,1) = length('WhatEver') + 1.
      PUT-STRING(mData,5) = 'WhatEver'.
      lRC = connection_handle:write(mData,1,13) no-error.
      
      if lRC = false or error-status:get-message(1) <> '' then
      do:
         display 'Unable To Write Detail Bytes'.
         return.
      end.      
      
      repeat on stop undo, leave on quit undo, leave:
         if connection_handle:connected() then 
            wait-for read-response of connection_handle. 
         else leave. 
      end.      
   end method.      


   method public void process_io ():
      define variable cTime as character no-undo.
      define variable iMessageSize as integer no-undo.
      /*----------------------------------------------------*/
      /* Check to see if the socket is still connected, if */
      /* not, exit from this internal procedure */
      /* */
      /* Please note that this check is done because it is */
      /* normal for a client connection to get a message */
      /* sent to it that indicates that the server has */
      /* disconnected from the client */
      /*----------------------------------------------------*/
      if self:CONNECTED() = false then
      return.
      /*----------------------------------------------------*/
      /* The following code reads the header block we are */
      /* using on each packet of data being sent. We are */
      /* prefixing each packet with a 4 byte integer that */
      /* tells us how much "real" data to expect. This is */
      /* done because we cannot be guaranteed that all of */
      /* the data that was sent actually reached Progress */
      /* at the same time. Since we are invoking a */
      /* blocking read method, we want to be sure that we */
      /* know how much data to read so that we don't sit */
      /* around in the blocking read method forever waiting */
      /* for data that will never arrive */
      /*----------------------------------------------------*/
      /*----------------------------------------------------*/
      /* Read 4 byte header block */
      /*----------------------------------------------------*/
      lRC = self:READ(mHeader,1,4,2) no-error.
      if lRC = false or error-status:get-message(1) <> '' then
      do:
         display 'Unable To Read Header Bytes'.
         return.
      end.
      /*----------------------------------------------------*/
      /* Now that the header block has been successfully */
      /* read, we know how much real data to read, so we go */
      /* do the read */
      /*----------------------------------------------------*/
      iMessageSize = get-long(mHeader,1).
      if iDataSize < iMessageSize then
      do:
         SET-SIZE(mData) = 0.
         SET-SIZE(mData) = iMessageSize.
         iDataSize = iMessageSize.
         SET-BYTE-ORDER(mData) = big-endian.
      end.
      
      lRC = self:READ(mData,1,iMessageSize,2) no-error.
      if lRC = false or error-status:get-message(1) <> '' then
      do:
         display 'Unable To Read Detail Bytes'.
         return.
      end.
      /*----------------------------------------------------*/
      /* At this point, we have successfully read the data */
      /* from the server (8 bytes as the server should have */
      /* sent a string containing the current time in the */
      /* following format: HH:MM:SS), so pull the data out */
      /* of the memory pointer and display it on the */
      /* screen */
      /*----------------------------------------------------*/
      cTime = get-string(mData,1).
      display cTime.
      pause 1 no-message.
      /*----------------------------------------------------*/
      /* Now we reuse the mData memory pointer and send a */
      /* new message back to the server (it doesn't matter */
      /* what we send because the server will ignore what */
      /* is sent and just do a reply by sending ba.ck the */
      /* current time) */
      /*----------------------------------------------------*/
      /*----------------------------------------------------*/
      /* Make sure that the mData memory pointer is sized */
      /* large enough for our purposes */
      /*----------------------------------------------------*/
      if iDataSize < 13 then
      do:
         SET-SIZE(mData) = 0.
         SET-SIZE(mData) = 13.
         iDataSize = 13.
         SET-BYTE-ORDER(mData) = big-endian.
      end.
      /*----------------------------------------------------*/
      /* Fill the memory pointer with data (header + the */
      /* real data) and write the data to the socket */
      /*----------------------------------------------------*/
      PUT-LONG(mData,1) = length('WhatEver') + 1.
      PUT-STRING(mData,5) = 'WhatEver'.
      lRC = self:WRITE(mData,1,13) no-error.
      if lRC = false or error-status:get-message(1) <> '' then
      do:
         display 'Unable To Write Detail Bytes'.
         return.
      end.
   end method.
   
   
   method protected void wait_for_response():
      if valid-handle(connection_handle) then
         wait-for read-response of connection_handle.
   end method.


   destructor SocketClient ():
      connection_handle:disconnect() no-error.
      delete object connection_handle.
      SET-SIZE(mHeader) = 0.
      SET-SIZE(mData) = 0.
   end destructor.
end class.
