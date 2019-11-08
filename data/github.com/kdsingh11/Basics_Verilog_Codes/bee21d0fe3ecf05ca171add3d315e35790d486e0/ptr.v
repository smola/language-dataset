//**********************************************************************************************
//**************************** Author: Kuldeep Singh Deshwal ***********************************
//**********************************************************************************************


module ptr;

reg [5:0]ptr;	// ptr range is from {0,63} and then it again counts in a cyclic order
reg [6:0]i;

initial 
begin
 	ptr=0;	
$display("------------------------- Initial value of ptr=%d ",ptr);
	for (i=0;i<=65;i=i+1)
	   begin	
		ptr=ptr+1;
$display("*%d----------------------Count of %dth times  ptr=%d ",i,i,ptr);
	   end
$display("---------------------- Final value of ptr=%d ",ptr);
end

endmodule

