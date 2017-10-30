package id3;

import java.util.HashMap;
import java.util.List;
import tree.Attribute;
import tree.Decision;
import tree.TreeNode;

public class ID3Helpers {
	
//returns if the given list of examples all have the same 
	public boolean areAllSame(List<Example> myList) {
		int len = myList.size();
		Example compareEx = myList.get(0);
		for(int i = 0; i<len; i++) {
			Example currentEx = myList.get(i);
			if(!compareEx.equals(currentEx)) {
				return false;
			}
		}
		return true;
	}
	
//Majority Value
	//returns the majority value of the list of examples given
	public Decision majorityValue(List<Example> examples) {
		//thing to return 
		Decision mostCommon = examples.get(0).getDecision();
		//create hashmap to contain each decision from the example and how often each decision is made 
		HashMap<Decision, Integer> possibleDecisions = new HashMap<Decision, Integer>();
		
		//get each example and its decision
		//for each Example in examples, add the Decision it made to the HashMap
		for (Example e: examples) {
			Decision key = e.getDecision();
			if(possibleDecisions.containsKey(key)) {
				//get the key
				Integer timesOccured = possibleDecisions.get(key);
				//add one to the value
				possibleDecisions.replace(key, timesOccured++);
			}
			//if not in the hashmap, add a key to the hashmap with key "e" and value 1
			else {
				possibleDecisions.put(e.getDecision(), 1);
			}
		}
		
		//at the end of this loop, find the key with the biggest value, and return that key
		int maxValue = 0;
		//find the max value in the hashmap and return the decision it is associated with
		for(HashMap.Entry<Decision, Integer> decision : possibleDecisions.entrySet()) {
			if (decision.getValue() > maxValue) {
				mostCommon = decision.getKey();
				maxValue = decision.getValue();
			}
		}
		
		return mostCommon;
	}
}
