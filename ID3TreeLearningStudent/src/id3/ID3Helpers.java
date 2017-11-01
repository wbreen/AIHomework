package id3;

import java.util.*;
import tree.*;

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
	
	
	//returns the attribute that will give you the most gain and the lowest remainder
	//TODO: finish this method
	//will need to return an attribute with a String name and String[] of answers
	public Attribute chooseAttribute(List<Attribute> attributes, List<Example> examples) {
		Attribute toReturn = null;
		//the hashmap should contain the entropy of the attribute, and the attribute that 
			//corresponds with that entropy
		
		//List<HashMap<Double, Attribute>> entropy;
		double entropySys = getEntropy(examples);
		double maxGain = 0;
		for(Attribute a : attributes) {
			double gain = findGain(entropySys, a, examples);
			if (gain > maxGain) {
				maxGain = gain;
				toReturn = a;
			}
		}
		
		return toReturn;
	}
	
	//given a list of examples, calculate the total entropy in the system
	//have getEntropy return the info left from the attribute and the list of examples given
	public double getEntropy(List<Example> examples) {
		//what to return
		double entropy = 0.0;
		//denominator (sum of all/number of decisions made)
		double sum=0;
		//numerator of each
		//int numThisVal;
		
		double[] numDecisions = new double[10];
		List<String> decisionNames = new ArrayList<>();
		
		//for each example, add it to the array+list if it isnt in [], or increase [] if it is
		for(Example e : examples) {
			String decE = e.getDecisionName();
			if(decisionNames.contains(decE)) {
				int index = decisionNames.indexOf(decE);
				numDecisions[index] = numDecisions[index] + 1;
			}
			else {
				decisionNames.add(decE);
				int index = decisionNames.indexOf(decE);
				numDecisions[index] = numDecisions[index] + 1;
			}
		}
	//now numDecisions contains the number of all decisions made and decisionNames= their names
		for (int i=0; i<numDecisions.length-1; i++) {
			sum = sum + numDecisions[i];
		}
		
		//now sum contains the total number of decisions made
		for (int i=0; i<decisionNames.size()-1; i++) {
			double frac = ((Double) numDecisions[i])/sum;
			double logB2 = (Math.log(frac)/Math.log(2));
			entropy = entropy + ((frac)*logB2);
		}
		//now entropy contains the negative value that is the entropy in the system
		entropy = (-1)*entropy;
		//now entropy is positive and we can use that as the base for future calculations
		
		return entropy;
	}
	
	
	//returns the gain of entropy if asked about a certain attribute
	//TODO
	public double findGain(double entropyPar, Attribute attr, List<Example> examples) {
		double whatsUpDoc = 0;
		double weightChild = 0;
		
//Gain is entropy of the parent minus the total weighted remaining entropy of the children
		//first have the entropy of the system (DONE)
		//then find the weighted entropy of the remaining information left
			// (value of the child * how often that child appears relative to the rest)
		//get the specific value of the child attribute you are looking for
		int[] timesVoAtt = new int [attr.getNumValues()];
		List<String> attNames = new ArrayList<>();
		Set<String> possibleVals = attr.getPossibleAnswers();
		List<List<Example>> listOfEx = new ArrayList<>();
		
		for (String val : possibleVals) {
			attNames.add(val);
			listOfEx.add(new ArrayList<Example>() );
		}
		for(Example e: examples) {
			//for each value, if the example is equal to an attribute value seen before,
			if(attNames.contains(e.getValue(attr))) {
				int index = attNames.indexOf(e.getValue(attr));
				timesVoAtt[index] = timesVoAtt[index] +1;
				listOfEx.get(index).add(e);
			}
				//add it to the array at the correct spot
			//if not, add the example string to the ArrayList and also add it to the appropriate
				//location in the []
		}
		double numChildren = 0;
		for(int i = 0; i< timesVoAtt.length; i++) {
			numChildren = numChildren + timesVoAtt[i];
		}
		//create a new list of only the specific examples that contain the child attribute
		double weight = 0;
		
		for(List<Example> examp : listOfEx) {
			weight = examp.size()/numChildren;
			double entChild = getEntropy(examp);
			weightChild = weightChild + (weight*entChild);
		}
		//for that list, find the info left
		
		
		//find the weight of each
		//multiply the weight of each by the info left for it
		//add together total weighted info left (infoLeft)
		//subtract infoLeft from originalEntropy aka entropy of parent and return
		whatsUpDoc = entropyPar - weightChild;
		
		return whatsUpDoc;
	}
	
	//TODO
	//finds the amount of information that a certain attribute can give
	public double findInfoLeft(Attribute attribute, List<Example> examples) {
		double nerd = 0;
		//first you need to see how many of each different possible answer you have
		//then you 
		Set<String> possibleVals = attribute.getPossibleAnswers();
		int numAns = attribute.getNumValues();
		for(Example e : examples) {
//			if(e.getDecisionName()) {
//				
//			}
		}
		for (int i = 0; i < numAns; i++) {
			
		}
		
		
		return nerd;
	}
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
}
