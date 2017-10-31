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
	
	//Import the helper methods
	ID3Helpers helpers = new ID3Helpers();
	
	// The ID3 Algorithm takes a collection of examples, the collection of Attributes used for making 
	// the decision, and a default Decision.  
	// It returns the decision tree.
	public TreeNode id3(List<Example> examples, List<Attribute> attributes, Decision defaultDecision){
		//if examples is empty, then return default
		if(examples.isEmpty()) {
			return defaultDecision;
		}
		
		//else if all examples have the same classification, then return the classification (decision)
		//(if all remaining values are neg or pos, then return neg or positive)
		else if(helpers.areAllSame(examples)) {
			return examples.get(0).getDecision();
		}
		//else if attributes is empty, then return Majority-Value(examples)
		else if(attributes.isEmpty()) {
			return helpers.majorityValue(examples);
		}
		//else (see pseudocode)
		else {
			//best <-- ChooseAttribute (attributes, examples)
					//whatever removes the most noise is the best attribute (the most gain)
			Attribute best = helpers.chooseAttribute(attributes, examples);
			//tree <-- a new decision tree with root test best
			
			//for each value vi of best do:
				//examples(i) {elements of examples where best = vi}
				//subtree <-- id3(examples(i), attributes -- best, majorityValue(examples))
				//add a branch to tree with label vi and subtree 'subtree'
			//end for loop
			//return tree
		}
		//when adding to a tree, you need a value and the tree to add it to
		//value is vi
		
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
	
	//returns the majority value of the list of examples given
	
}
