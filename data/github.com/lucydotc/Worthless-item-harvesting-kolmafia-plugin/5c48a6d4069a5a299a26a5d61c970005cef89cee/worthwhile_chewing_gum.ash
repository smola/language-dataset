## this is my worthless thingymagit hoarding script

## It will prompt you for the number of chewing gums on a string you wish to use.
## NOTE: The number of worthless items gained at the end may be less than  the gums you used because we have to get all the other starter items too. However, if you started with all the starter items already, then you should receive as many worthless items as gums used.

## Script basically buys gums for you, then gets all starter items. It will place the worthless item in your closet and fish for it again. This is repeated until you have no more gum.
## But if you lack sufficient gums to ensure a 100% worthless item fish, the script will take a gamble and use all your gums on hand anyway. Cross your fingers!.

##  - Tea Chugger (#2908305)



## Checks how many starter items you have CURRENTLY IN YOUR INVENTORY

int checkStarterItems(){
	

	int numOfStarterItems = 0;

	if ( item_amount( $item[seal-skull helmet] ) > 0)
  	{
	
		((numOfStarterItems++));
	}

	if (item_amount($item[seal-clubbing club]) > 0){
	
		((numOfStarterItems++));
	}

	if (item_amount ($item[helmet turtle]) > 0){
	
	((numOfStarterItems++));
	}

	if (item_amount ($item[turtle totem]) > 0){
	
		((numOfStarterItems++));
	}

	if (item_amount ($item[ravioli hat]) > 0){
	
		((numOfStarterItems++));
	}

	if (item_amount ($item[pasta spoon]) > 0){
	
		((numOfStarterItems++));
	}

	if (item_amount ($item[Hollandaise helmet]) > 0){
	
		((numOfStarterItems++));
	}

	if (item_amount ($item[saucepan]) > 0){
	
		((numOfStarterItems++));
	}

	if (item_amount ($item[disco mask]) > 0){
	
		((numOfStarterItems++));
	}

	if (item_amount ($item[disco ball]) > 0){
	
		((numOfStarterItems++));
	}

	if (item_amount ($item[mariachi hat]) > 0){
	
		((numOfStarterItems++));
	}

	if (item_amount ($item[stolen accordion]) > 0){
	
		((numOfStarterItems++));
	}

	if (item_amount ($item[old sweatpants]) > 0){
	
		((numOfStarterItems++));
	}

	if (item_amount ($item[worthless trinket]) > 0){
	
		((numOfStarterItems++));
	}

	if (item_amount ($item[worthless gewgaw]) > 0){
	
		((numOfStarterItems++));
	}


	if (item_amount ($item[worthless knick-knack]) > 0){
	
		((numOfStarterItems++));
	}


	return numOfStarterItems;


}

void useGumsAnyway(){
	
	int currentWorth = ( 	item_amount ($item[worthless knick-knack]) + 		item_amount ($item[worthless gewgaw])   + 	item_amount ($item[worthless trinket])		 ) ;
	
	print("Using all your chewing gums on hand anyway. You might get lucky!", "purple");
	cli_execute("use * chewing gum on a string");	

	int andNowWorth = ( 	item_amount ($item[worthless knick-knack]) + 		item_amount ($item[worthless gewgaw])   + 	item_amount ($item[worthless trinket])		 ) ;

	int difference = andNowWorth - currentWorth;

	if(andNowWorth > currentWorth){
		print("Yay! You got " +difference+ "worthless items" , "purple");
	}
	else{
		print("Lol no. Tough luck, dude.", "purple");
	}
}


int getAllStarterItems(int numOfStarterItems, int numGumsYouWannaUse){
	
	 ## gets you to a state where you have one of every starter item, including the 3 worthless things themselves.

	 int gumsLeft = numGumsYouWannaUse - ((16-numOfStarterItems)) ;

	 if (gumsLeft <=0) {

	 	print("Woah hey, you don't have enough gum for starter items to ensure you get worthless treasure!", "purple");
	 	gumsLeft *= -1;
	 	print("You are short by " +gumsLeft+ " gums, yo.", "purple");
	 	gumsLeft = gumsLeft - 99999;

	 	useGumsAnyway();

	 }
	 else{
 	
  		use( 16 - numOfStarterItems, $item[Chewing gum on a String] );
 
	 }

 

  return gumsLeft;

}


void goFish(int gumsLeft){
	
		while(gumsLeft > 0){
 			cli_execute("closet put * worthless trinket, * worthless gewgaw, * worthless knick-knack");
 			cli_execute("use chewing gum on a string");	
  		((gumsLeft--));
  		}

  		if(gumsLeft == 0){
  			cli_execute("closet take * worthless trinket, * worthless gewgaw, * worthless knick-knack");
  		}
 
}





void main(int numGumsYouWannaUse)
{
	if(numGumsYouWannaUse <= 0){
		  print("Is that you, Sneaky Pete? Valid numbers only, please.", "purple");
		 exit;

	}
	

 ## If we do not have enough CGOAS, buy more.
  if ( item_amount( $item[Chewing gum on a String] ) <= numGumsYouWannaUse )
  {
    buy( numGumsYouWannaUse - item_amount( $item[Chewing gum on a String] ), $item[Chewing gum on a String] );
  }

 
 ## gets you to a state where you have one of every starter item, including the 3 worthless things themselves.
  int gumsLeft = getAllStarterItems(checkStarterItems(), numGumsYouWannaUse);  
 
  ## Get those worthless thingies from the drain
  goFish(gumsLeft);


  if(gumsLeft > 0){
   print("You managed to get "+gumsLeft+" worthless thingies!", "purple");
  }
 

}

