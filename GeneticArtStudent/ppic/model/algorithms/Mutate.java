package ppic.model.algorithms;

import ppic.model.*;
import ppic.model.operators.PPConstant;


public class Mutate extends Algorithm
{


	Randomize rand = new Randomize();
	/*
	 * Pseudocode for mutating:
	 * apply (ex, rate):
	 *   rate% of the time return a copy of ex with no change. 
	 *   else:
	 *      if ex has no children, choose randomly (evenly) between the following choices:
	 *           - return completely new random expression (use your Randomize class, and the same rate as input) 
	 *           - return result of mutateConstant method.  This method takes an expression, and if it represents a 
	 *             Constant, it changes the R, G, B values by a little bit. 
	 *      if ex has one child, choose randomly (evenly) between the following choices:
	 *           - return a copy of the expression with no change
	 *           - return a copy of the child of the expression
	 *           - return a new 1-arg Expression with the base of ex and an entirely new child
	 *           - return a new 1-arg Expression with a random base and ex as a child
	 *           - return a new 2-arg Expression with a random base and right child and ex as a left child
	 *           - return a new 2-arg Expression with a random base and left child and ex as a right child
	 *      if ex has 2 children, choose randomly (evenly) between the following choices:
	 *           - return a copy of the expression with no change
	 *           - return a copy of the left child with no change
	 *           - return a copy of the right child with no change
	 *           - return Randomize.replaceBase() with the left then right child
	 *           - return Randomize.replaceBase() with the right then left child
	 */

	public Expression apply (Expression expr, double rate)
	{
		double noChange = random.nextDouble();
		Expression myCopy = expr.copy();
		System.out.println("rate was: " + rate);
		System.out.println("noChange was " + noChange);
		System.out.println("The degree of the expression is: " + expr.getDegree());
		if(rate >= noChange) {
			System.out.println("Didn't change anything in mutate");
			return myCopy;
		}
		else {
			//no children
			int degree = expr.getDegree();
			if(degree==0) {
				double choose = random.nextDouble();
				//new random expression
				if(choose >= .5) {
					return rand.getRandomExpression(rate);
				}
				//mutate the expression a little
				else {
					return mutateConstant(expr);
				}
			}
			//only has one child
			if(degree==1) {
				return hasOneChild(expr, rate);
			}
			//has two children
			else {
				return hasTwoChild(expr, rate);
			}
			
		}
		//return null;
	}


	/*
	 *  If the input expression is a constant, it returns a new Constant with one of its r, g, or b 
	 *  values altered by a small amount.
	 *  Otherwise, it returns a copy of the input expression.    
	 */
	public Expression mutateConstant(Expression tfunc) {
		if (tfunc.getClass().equals(ppic.model.operators.PPConstant.class)) {
			RGBColor rgb = tfunc.evaluate(0, 0, 0);
			double change = random.nextDouble()*40 - 20;
			int chooser = random.nextInt(2);
			double[] color = {rgb.getR(), rgb.getG(), rgb.getB()};
			color[chooser]+= change;
			color[chooser] = Math.min(color[chooser], 255);
			color[chooser] = Math.max(color[chooser], 0);
			return new PPConstant(color[0], color[1], color[2]);

		}

		else return tfunc.copy();
	}
	
	//returns an expression if the expression only has one child
	public Expression hasOneChild(Expression expr, double rate) {
		Expression myCopy = expr.copy();
		double choose = random.nextDouble();
		double divideOn = 1.0/6.0;
		System.out.println(divideOn);
		//no change
		if(divideOn > choose) {
//		if(rate > choose) {
			return myCopy;
		}
		//child of expression
		if(2*divideOn > choose && choose > divideOn) {
//		choose = random.nextDouble();
//		if(rate > choose) {
			myCopy = expr.getLeft();
			return myCopy;
		}
		//new 1-arg with base of expr and new child
		if(3*divideOn > choose && choose > 2*divideOn) {
//		choose = random.nextDouble();
//		if(rate > choose) {
			myCopy.setLeft(rand.getRandomExpression(rate));
			return myCopy;
		}
		//new 1-arg with random base and expr as child
		if(4*divideOn > choose && choose > 3*divideOn) {
//		choose = random.nextDouble();
//		if(rate > choose) {
			Expression newExp = rand.getRandomExpression(rate);
			newExp.setLeft(myCopy);
			return newExp;
		}
		//new 2-arg with random base + right child, and expr as left child
		if(5*divideOn > choose && choose > 4*divideOn) {
//		choose = random.nextDouble();
//		if(rate > choose) {
			Expression newExp = rand.getRandomExpression(rate);
			newExp.setRight(rand.getRandomExpression(rate));
			newExp.setLeft(myCopy);
			return newExp;
		}
		//new 2-arg with random base + left child, and expr as right child
		else {
			Expression newExp = rand.getRandomExpression(rate);
			newExp.setLeft(rand.getRandomExpression(rate));
			newExp.setRight(myCopy);
			return newExp;
		}
	}
	
	
	//returns an expression if the exp given has two children
	public Expression hasTwoChild(Expression expr, double rate) {
		Expression myCopy = expr.copy();
		Expression rightChild = myCopy.getRight();
		Expression leftChild = myCopy.getLeft();
		double which = random.nextDouble();
		double divideBy = 1.0/5.0;
		System.out.println("DivideBy is :" + divideBy);
		System.out.println("which is : " + which);
		//return a copy with no change
//		if(divideBy > which) {
		if(rate > which) {
			return myCopy;
		}
		//return a copy of the left child with no change
		if(2*divideBy > which && which > divideBy) {
//		which = random.nextDouble();
//		if(rate > which) {
			return myCopy.getLeft();
		}
		//return a copy of the right child with no change
		if(3*divideBy > which && which > 2* divideBy) {
//		which = random.nextDouble();
//		if(rate > which) {
			return myCopy.getRight();
		}
		//return replaceBase() with the left then right child
		if(4*divideBy > which && which > 3*divideBy) {
//		which = random.nextDouble();
//		if(rate > which) {
			return rand.randomReplaceBase(leftChild, rightChild, rate);
		}
		//return replaceBase() with right then left child
		else {
			return rand.randomReplaceBase(rightChild, leftChild, rate);
		}
	}

}
