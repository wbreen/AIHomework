package ppic.model.algorithms;



import ppic.model.*;
import ppic.model.operators.PPConstant;
import ppic.util.Reflection;



public class Randomize extends Algorithm
{

	/*
	 * To "apply" this algorithm, return a random expression 
	 * according to the pseudocode.
	 */
	public Expression apply (double rate)
	{
		Expression result = getRandomExpression(rate);
		return result;
	}

	/*
	 * getRandomExpression(rate):
	 *   with (rate) probability return a new 0-argument expression
	 *   else, with (rate) probability return a new 1-argument expression with a random child
	 *   else, return a new 2-argument expression with 2 random children.
	 *   
	 *   Each time you recurse, the rate should increase by 33% to ensure the base case
	 *   gets hit.
	 */
	public Expression getRandomExpression (double rate)
	{
		double possiblityOfZero = random.nextDouble();
		double possiblityOfOne = random.nextDouble();

		if(rate < possiblityOfZero) {
			return makeZeroArgumentExpression();
		}
		else if(rate < possiblityOfOne) {
			return makeOneArgumentExpression(getRandomExpression(rate*1.33));
		}
		else {
			return makeTwoArgumentExpression(getRandomExpression(rate*1.33), getRandomExpression(rate*1.33));
		}
		//return null;
	}




	/*
	 * Create a new random expression with given left and right subtrees.
	 * Therefore, only the base is really random. 
	 * The rate is the preference toward small trees.  The higher the rate, 
	 * the more probable the zero and one argument expressions are.
	 * 
	 * with rate probability, return a new 0-argument expression
	 * else with rate probability, return a new 1-argument expression with left as the child
	 * else return a new 2-argument expression with the given children.
	 */
	public Expression randomReplaceBase (Expression left, Expression right, double rate)
	{
		double zeroArgument = random.nextDouble();
		double oneArgument = random.nextDouble();
		
		if(rate < zeroArgument) {
			return makeZeroArgumentExpression();
		}
		else if(rate < oneArgument) {
			return makeOneArgumentExpression(left);
		}
		else {
			return makeTwoArgumentExpression(left, right);
		}
		//return null;
	}




	/*
	 * Returns a random zero-argument expression
	 */
	public Expression makeZeroArgumentExpression ()
	{
		try
		{
			int idx = random.nextInt(ZERO_ARG_EXPRESSIONS.length);
			if (ZERO_ARG_EXPRESSIONS[idx].startsWith("PPConstant"))
			{
				return (Expression)Reflection.createInstance(PACKAGE + ZERO_ARG_EXPRESSIONS[idx], (random.nextDouble()*2 - 1), 
						(random.nextDouble()*2 - 1), (random.nextDouble()*2 - 1));
			}                
			return (Expression)Reflection.createInstance(PACKAGE + ZERO_ARG_EXPRESSIONS[idx]);
		}
		catch (Exception e)
		{
			// should never happen
			e.printStackTrace();
			return new PPConstant();
		}
	}


	/*
	 * Returns a new 1-argument expression, with a random base and the given expression as the child.
	 */
	public Expression makeOneArgumentExpression (Expression expr)
	{
		try
		{
			int idx = random.nextInt(ONE_ARG_EXPRESSIONS.length);
			return (Expression)Reflection.createInstance(PACKAGE + ONE_ARG_EXPRESSIONS[idx], expr);
		}
		catch (Exception e)
		{
			// should never happen
			e.printStackTrace();
			return new PPConstant();
		}
	}

	/*
	 * Returns a new 2-argument expression, with a random base and the given expressions as the children.
	 */
	public Expression makeTwoArgumentExpression (Expression left, Expression right)
	{
		try
		{
			int idx = random.nextInt(TWO_ARG_EXPRESSIONS.length);
			if (TWO_ARG_EXPRESSIONS[idx].startsWith("PPExtImage") && 
					Parser.singleton.getImageList().size() > 0)
			{
				return (Expression)Reflection.createInstance(PACKAGE + TWO_ARG_EXPRESSIONS[idx], left, right,
						Parser.singleton.getImageList().get(random.nextInt(Parser.singleton.getImageList().size())));
			}
			else
			{
				while (TWO_ARG_EXPRESSIONS[idx].startsWith("PPExtImage"))
				{
					idx = random.nextInt(TWO_ARG_EXPRESSIONS.length);
				}
				return (Expression)Reflection.createInstance(PACKAGE + TWO_ARG_EXPRESSIONS[idx], left, right);
			}
		}
		catch (Exception e)
		{
			// should never happen
			e.printStackTrace();
			return new PPConstant();
		}
	}
}
