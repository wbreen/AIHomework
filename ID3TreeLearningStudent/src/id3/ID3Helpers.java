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
		Decision mostCommon = examples.get(0).getDecision();
		HashMap<Decision, Integer> possibleDecisions = new HashMap<Decision, Integer>();
		//get each example and its decision
		//for each Example in examples, add it to the HashMap
		for (Example e: examples) {
			if(possibleDecisions.containsKey(e)) {
				//get the key
				//add one to the value
				possibleDecisions.getKey(e);
			}
			//if not in the hashmap, add a key to the hashmap with key "e" and value 1
		}
		//at the end of this loop, find the key with the biggest value, and return that key
		
		//create a hashmap  of the examples and how often each has happened
		//keep track of how many times each example has occurred
		//after you have gone through all the possible decisions in the Example list, return the one 
			//that has happened most often
		
		return mostCommon;
	}
}
