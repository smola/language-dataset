// CFK 08/18/2015 charles.kaminski@lexisnexis.com
// This code shows a Roxie query using a prefix tree to 
//   significantly improve the performance of an 
//   edit distance algorythm.  Without the prefix tree
//   performance would be too slow for an interactive query.
// The code is designed to run on the Roxie.
// It's companion Thor code needs to be run first to build
//   the Roxie index.
// Read more on the HPCC Systems blog
// https://hpccsystems.com/resources/blog?uid=225 

query_word := 'TESTWORD':STORED('QueryWord');
MAX_DISTANCE := 2 :STORED('MaxDistanceLessThan');

WordLayout := RECORD
   STRING   word;
END;

PTIndexLayout := RECORD
	UNSIGNED8 id;               // Primary Key
	UNSIGNED8 parent_id;        // The parent for this node.  Zero is a root node
	STRING    node;             // Contains the payload for this node   
	BOOLEAN   is_word;          // Indicates if this node is a word (an end-cap with no children)   
	UNSIGNED4 compute_node;
	UNSIGNED8 recptr {virtual(fileposition)};
END;

pt_ds := DATASET('~OUT::CFK::LNames_Atl_GA_PT', PTIndexLayout, FLAT);

pt_index := INDEX(pt_ds, {parent_id}, {id, is_word, node, RecPtr}, '~OUT::CFK::LNames_Atl_GA_PT_Key');

STRING CalculateLevenshteinVector(STRING word, STRING node, STRING state) := BEGINC++
	/// CFK 08/20/2015 charles.kaminski@lexisnexis.com                 
	/// This C++ returns the vector used to calculate
	///  the Levenshtein distance in an interative process so that
	///  an efficient trie structure can be traversed when finding candidates
	/// Node values can be passed in and the current state can be saved 
	///   in the return vector so that other nodes can be further processed in series
	/// We're using char arrays to keep the size down
	/// That also means that all our words (both in the trie and our query words)
	///  must be less than 255 characters in length.  If that limitation becomes a problem
	///  we can easily use an array of integers or another other structure to store
	///  intermediate values.
	#option pure
	#include <algorithm>
	#body
	//  The last element in the array will hold
	//   the min value.  The min value will be used later.
	int v_size = lenWord + 2;
	
	unsigned char min_value = 255;
	unsigned char new_value = 0;
	
	// Since v0 is not returned, we should not use rtlMalloc
	//unsigned char * v0 = (unsigned char*)rtlMalloc(v_size);	
	unsigned char v0[v_size];
	// We use rtlMalloc helper function when a variable is returned by the function
	unsigned char * v1 = (unsigned char*)rtlMalloc(v_size);
	
	if (lenState < 1){
		for (int i = 0; i < v_size; i++){
			v0[i] = i;
		}
	}
	else{
		memcpy(&v0[0], &state[0], v_size);
	}
	
	int cost = 0;
	int k = v0[0];
					
	for (unsigned int i=k; i<k+lenNode; i++)
	{
		min_value = 255;
		v1[0] = i + 1;
		for (unsigned int j=0; j<lenWord; j++)
		{
			cost = (node[i-k] == word[j]) ? 0 : 1;
			new_value = std::min(v1[j] + 1, std::min(v0[j+1] + 1, v0[j] + cost));
			v1[j+1] = new_value;
			if (new_value < min_value){
			  min_value=new_value;
      }
		}
		memcpy(&v0[0], &v1[0], lenState);
	}
	
	// Store the min_value;
	v1[v_size-1] = min_value;
	
	__lenResult = v_size;
  __result    = (char *) v1;
                                                                
ENDC++;
 
UNSIGNED1 GetMinDistance(STRING state) := BEGINC++
  /// CFK 08/20/2015 charles.kaminski@lexisnexis.com	
	///  Get the Minimum Edit Distance
	#option pure
	#body		
	//return (unsigned char) state[(unsigned int) position];
	return (unsigned char) state[lenState-1];
ENDC++;

UNSIGNED1 GetFinalDistance(STRING state) := BEGINC++
  /// CFK 08/20/2015 charles.kaminski@lexisnexis.com	
	///  Get the Final Edit Distance
	#option pure
	#body		
	//return (unsigned char) state[(unsigned int) position];
	return (unsigned char) state[lenState-2];
ENDC++;
 
QueryPTLayout := RECORD
	STRING    word;
	STRING    state                := '';
	DATA      state_data           := (DATA)'';
	UNSIGNED  node_id              := 0;
	STRING    node                 := '';
	BOOLEAN   is_word              := False;
	STRING    cumulative_nodes     := '';
	UNSIGNED1 cumulative_node_size := 0;
	UNSIGNED1 current_distance     := 0;
	UNSIGNED1 final_distance       := 0;
END;

manual_input_ds:= DATASET([{query_word}], WordLayout);
query_ds := PROJECT(manual_input_ds, QueryPTLayout);

//query_ds := PROJECT(input_ds, QueryPTLayout); 
//output(query_ds, named('Query_DS'));

QueryPTLayout QueryPTTransform(QueryPTLayout L, RECORDOF(pt_index) R) := TRANSFORM
	SELF.word                 := L.word;
	SELF.state                := IF(R.is_word, L.state, CalculateLevenshteinVector(L.word, R.node, L.state));
	SELF.state_data           := (DATA)SELF.state;
	SELF.node_id              := R.id;
	SELF.node                 := R.node;
	SELF.is_word              := R.is_word;
	SELF.cumulative_node_size := IF(R.is_word, LENGTH(R.node), LENGTH(R.node) + L.cumulative_node_size);
	SELF.cumulative_nodes     := IF(R.is_word, R.node, L.cumulative_nodes + R.node);
	SELF.current_distance     := IF(R.is_word, L.current_distance, GetMinDistance(SELF.state));
	SELF.final_distance       := IF(R.is_word, GetFinalDistance(SELF.state), L.final_distance);	
END;
 
looped_ds := LOOP(query_ds, 
					LEFT.is_word = False,
					EXISTS(ROWS(LEFT)) = True,
					JOIN(ROWS(LEFT), pt_index, LEFT.node_id = RIGHT.parent_id and LEFT.current_distance <= MAX_DISTANCE, 
							 QueryPTTransform(LEFT,RIGHT), INNER)); 														 

results := OUTPUT(COUNT(looped_ds(looped_ds.final_distance <= MAX_DISTANCE)), NAMED('Final'));

INTEGER8 ms() := BEGINC++
  #option pure
	#include <sys/time.h>

	#body
	struct timeval tp;
    gettimeofday(&tp, NULL);
	//get current timestamp in milliseconds
    long long mslong = (long long) tp.tv_sec * 1000L + tp.tv_usec / 1000;
  return mslong;
ENDC++;

start_time := output(ms(), NAMED('Start_Time'));
finish_time := output(ms(), NAMED('End_Time'));

SEQUENTIAL(start_time, results, finish_time);
