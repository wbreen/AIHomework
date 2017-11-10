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
		if(mom.equals(dad)) {
			double giveMom = random.nextDouble();
			if(rate >= giveMom) {
				return mom;
			}
			else {
				return dad;
			}
		}
		else {
			double spliceWhich = random.nextDouble();
			if(rate >= spliceWhich) {
				return splice(dad, mom, rate);
			}
			else {
				return splice(mom, dad, rate);
			}
		}
		//return null;
	}
	
	public Expression splice(Expression p1, Expression p2, double rate) {
		//part 1
		Expression p2Part = findCopiedPart(p2.copy(), rate);
//		boolean p2isCopied = false;
//		while (!p2isCopied) {
//			double randomL = random.nextDouble();
//			double randomR = random.nextDouble();
//			if(rate >= randomL) {
//				p2Part = p2Part.getLeft();
//			}
//			else if (rate >= randomR) {
//				p2Part = p2Part.getRight();
//			}
//			else {
//				p2Part = p2Part.copy();
//				//p2isCopied = true;
//			}
//		}
		
		//part 2
		double randomSpl = random.nextDouble();
		//double randomSpl2 = random.nextDouble();
		Expression p1Copy = p1.copy();
		boolean p1IsntDone = false;
		while(p1IsntDone == false) {
			if(rate >= randomSpl || p1.getDegree()==0) {
				p1IsntDone = true;
			}
			else if(p1.getDegree() == 1) {
				p1Copy.setRight(p2Part);
			}
			else {
				double randomSide = random.nextDouble();
				if(randomSide >=.5) {
					p1Copy.setLeft(p2Part);
					p1IsntDone = true;
				}
				else {
					p1Copy.setRight(p2Part);
					p1IsntDone = true;
				}
			}
		}
		return p1Copy;
	}
	
	
	
//	make a copy of a random part of p2 as follows:
//		 *       	rate % of the time follow the left child if it has one
//		 *       	else rate % of the time follow the right child if it has one
//		 *          else, copy out the spot where you are.
//		 *       Call this part of p2 "p2Part"
	
	public Expression findCopiedPart(Expression p2, double rate) {
		Expression p2Part = null;
		double left = random.nextDouble();
		double right = random.nextDouble();
		
		if(rate >= left) {
			p2Part = findCopiedPart(p2.getLeft(), rate);
		}
		else if(rate >= right){
			p2Part = findCopiedPart(p2.getRight(), rate);
		} else {
			return p2Part = p2.copy();
		}
		return p2Part;
	}


}
