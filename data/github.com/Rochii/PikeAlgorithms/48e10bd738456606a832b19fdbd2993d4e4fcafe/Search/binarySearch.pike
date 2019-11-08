#!/usr/bin/pike

/*
*
* Created on Mon Nov 19 16:04:30 2018
*
* @author: Roger Truchero
*
*/

class BinarySearch
{
	int x;
	array(int) a = ({});

	//Constructor
	void create(int num, array(int) arr){
		x = num;
		a = arr;
	}

	//Function that invokes the binarySearch function
	int binarySearch()
	{
	   	write("\n" + log() + "Starting binary search of element %d in an array of %d elements.\n", x, sizeof(a));
	   	int result = binarySearching(0, sizeof(a) - 1, x); 
	   	if(result == -1)
	   		write(log() + "Element is not present in array.\n");
	   	else
	   		write(log() + "Element %d is present at index %d.\n", a[result], result);
	}

	//Main binarySearch function that uses a divide-and-conquer strategy to find a number x
	int binarySearching(int left, int right, int x) 
	{ 
	    if (right >= left)
	    {
	    	int mid = left + (right - left) / 2; 
	  
	        if (a[mid] == x) return mid;
	      
	        if (a[mid] > x) return binarySearching(left, mid - 1, x); 
	     
	        return binarySearching(mid + 1, right, x); 
	   	} 
	   	return -1; 
	} 

	//Function that obtains the current string time format to make the log
	string log()
	{
		return ctime(time()) - "\n" + " => ";
	}
} 