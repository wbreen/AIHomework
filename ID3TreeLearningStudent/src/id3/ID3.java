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
			System.out.println("The list of examples is empty ");
			return defaultDecision;
		}
		
		//else if all examples have the same classification, then return the classification (decision)
		//(if all remaining values are neg or pos, then return neg or positive)
		else if(helpers.areAllSame(examples)) {
			System.out.println("all the examples have the same outcome ");
			return examples.get(0).getDecision();
		}
		//else if attributes is empty, then return Majority-Value(examples)
		else if(attributes.isEmpty()) {
			System.out.println("There are no more attributes to narrow down the decision tree by");
			return helpers.majorityValue(examples);
		}
		//else (see pseudocode)
		else {
			//best <-- ChooseAttribute (attributes, examples)
				//whatever removes the most noise is the best attribute (the most gain)
				//information gain = Entropy(parent) - Weighted Sum of Entropy(Children)
				//best is a list of attributes, going from the attributes with the most gain down to the attributes with the least gain
			
				//1. compute entropy and info gain for each attribute
				//2. sort attributes from most gain to least gain
				//3. select the first attribute from the tree and use that for the base of the tree/subtree, then remove it from the sorted list
			
			//List<Attribute> best = helpers.chooseAttribute(attributes, examples);
			
			Attribute best = helpers.chooseAttribute(attributes, examples);
			
			//tree <-- a new decision tree with root test best
			//Attribute tree = best.get(0);
			
			int bestLoc = attributes.indexOf(best);
			Attribute tree = attributes.get(bestLoc);
			attributes.remove(bestLoc);
			
			//best.remove(0);
			
			//for each value vi of best do:
			//or
			Set<String> possibleAnswers = best.getPossibleAnswers();
			for (String v : possibleAnswers) {
				
				//create a list of the examples to contain the list of examples that correspond with the value of 'best' that you are looking at
				List<Example> newExamps = new ArrayList<Example>();
				
				//populate the new list
				for (Example e : examples) {
					if (e.getValue(best) == v) {
						newExamps.add(e);
					}
				}
				//subtree <-- id3(examples(i), attributes -(not including)- best, majorityValue(examples))
					//dont include 'best' in attributes when recursing because you have already seperated by that attribute, and you can't get any better or you might go in circles if you keep it in there
				TreeNode subTree = id3(newExamps, attributes, helpers.majorityValue(examples));
				//add a branch to tree with label vi and subtree 'subtree'
				tree.addChild(v, subTree);
				
			}
//			for (Attribute v : best) {
//				//examples(i) {elements of examples where best = vi}
//				//there are a certain amount of possible answers for "best" attribute, so for each possible answer there is a smaller list of examples in which each of the possible answers for "best" is expressed
//				//get that list and use it when you are recursing
//				for(int i = 0; i < examples.size(); i++) {
//					
//				}
//				List<Example> newExamplesList = new ArrayList<Example>();
//				//subtree <-- id3(examples(i), attributes -- best, majorityValue(examples))
//				TreeNode subTree = id3(newExamplesList, best , helpers.majorityValue(examples));
//				//add a branch to tree with label vi and subtree 'subtree'
//				tree.addChild(v.toString(), subTree);
//			}
				
			//end for loop
			return tree;
		}
		//when adding to a tree, you need a value and the tree to add it to
		//value is vi
		
		//return defaultDecision;
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
