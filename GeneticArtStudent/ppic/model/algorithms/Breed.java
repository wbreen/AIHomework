package ppic.model.algorithms;

/**
 *  Author: William Breen
 */

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
//		System.out.println("mom: " + mom.toString());
//		System.out.println("dad: " + dad.toString());
//		System.out.println("rate: " + rate);
		if(mom.equals(dad)) {
//			System.out.println("mom equals dad");
			double giveMom = random.nextDouble();
			if(rate >= giveMom) {
				return mom.copy();
			}
			else {
				return dad.copy();
			}
		}
		else {
//			System.out.println("Mom doesnt equal dad");
			double spliceWhich = random.nextDouble();
//			System.out.println("spliceWhich is : " +spliceWhich);
			if(rate >= spliceWhich) {
				return splice(dad.copy(), mom.copy(), rate);
			}
			else {
				return splice(mom.copy(), dad.copy(), rate);
			}
		}
		//return null;
	}
	
	public Expression splice(Expression p1, Expression p2, double rate) {
		//part 1
		Expression p2Part = findCopiedPart(p2.copy(), rate);
		
		//part 2
		double randomSpl = random.nextDouble();
		Expression p1Copy = p1.copy();
		
		//rate % of time or if p1 has no children
		if(rate > randomSpl || p1.getDegree()==0) {
			//p1IsntDone = true;
//			System.out.println("Just returning a copy of p1");
			p1Copy = p1.copy();
			return p1Copy;
		}
		//if p1 has one child
		else if(p1.getDegree() == 1) {
			p1Copy.setLeft(p2Part);
//			System.out.println("set the left child to p2Part, degree was 1");
			return p1Copy;
		}
		//if p1 has two children
		else {
			double randomSide = random.nextDouble();
			if(randomSide >=.5) {
				p1Copy.setLeft(p2Part);
				//p1IsntDone = true;
//				System.out.println("set the left child to p2Part, degree was 2");
				return p1Copy;
			}
			else {
				p1Copy.setRight(p2Part);
//				System.out.println("set the right child to p2Part, degree was 2");
				//p1IsntDone = true;
				return p1Copy;
			}
		}
		//return p1Copy;
	}
	
	
	
//	make a copy of a random part of p2 as follows:
//		 *       	rate % of the time follow the left child if it has one
//		 *       	else rate % of the time follow the right child if it has one
//		 *          else, copy out the spot where you are.
//		 *       Call this part of p2 "p2Part"
	
	public Expression findCopiedPart(Expression p2, double rate) {
		Expression p2Part = null;
		double left = random.nextDouble();
		
					//follow left child if it has one
		if(rate > left && p2.getLeft() != null) {
			p2Part = p2.getLeft();
		}			//follow the right child if it has one
		double right = random.nextDouble();
		if(rate > right && p2.getRight() != null) {
			p2Part = p2.getRight();
		} else {		//copy out the spot where you are 
			p2Part = p2.copy();
		}
		return p2Part;
	}


}
