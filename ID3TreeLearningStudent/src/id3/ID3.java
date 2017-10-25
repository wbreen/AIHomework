package id3;
/*
 * Author: William Breen
 * ID3 Algorithm Implemented
 */
import java.util.*;

import tree.Attribute;
import tree.Decision;
import tree.TreeNode;
public class ID3 {
	
	// The ID3 Algorithm takes a collection of examples, the collection of Attributes used for making 
	// the decision, and a default Decision.  
	// It returns the decision tree.
	public TreeNode id3(List<Example> examples, List<Attribute> attributes, Decision defaultDecision){
		//if examples is empty, then return default
		if(examples.isEmpty()) {
			return defaultDecision;
		}
		
		//else if all examples have the same classification, then return the classification
		//TODO: finish this elif statement
		else if(areAllSame(examples)) {
			//needs to change, this is just a temporary value
			return defaultDecision;
		}
		//else if attributes is empty, then return Majority-Value(examples)
		
		//else (see pseudocode)
		
		return defaultDecision;
	}
	
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
}
