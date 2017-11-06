package ppic.model.algorithms;

import ppic.model.*;
import ppic.model.operators.APPExtImage;
import ppic.util.Reflection;


public class Breed extends Algorithm
{
	/*
	 * Pseudocode for breeding:
	 * apply(mom, dad, rate):
	 * 		if mom and dad are same Expression:
	 * 			return copy of mom (rate) % of the time and copy of dad (1-rate)% of the time.
	 * 		else:
	 * 			return splice(dad, mom, rate) (rate) % of the time and splice (mom, dad, rate) (1-rate) % of the time.
	 * 
	 * splice(p1, p2, rate):
	 *       make a copy of a random part of p2 as follows:
	 *       	rate % of the time follow the left child if it has one
	 *       	else rate% of the time follow the right child if it has one
	 *          else, copy out the spot where you are.
	 *       Call this part of p2 "p2Part"
	 *        
	 *       Then splice the two together as follows:
	 *          rate% of the time, or if p1 has no children, return copy of p1 with no change
	 *          otherwise, if p1 has 1 child, make a copy of p1 with p2Part as the child
	 *          otherwise, if p1 has 2 children, half the time return copy of p1 with p2Part as left child
	 *              and half the time return copy of p1 with p2Part as right child.
	 * 	     
	 */
	public Expression apply (Expression mom, Expression dad, double rate)
	{
		return null;
	}


}
