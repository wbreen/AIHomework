/**
 * This is the template of a class that should run value iteration on
 * a given MDP to compute the optimal policy which is returned in the
 * public <tt>policy</tt> field.  The computed optimal utility is also
 * returned in the public <tt>utility</tt> field.  You need to fill in
 * the constructor.  You may wish to add other fields with other
 * useful information that you want this class to return (for
 * instance, number of iterations before convergence).
 */
public class ValueIteration {

	/** the computed optimal policy for the given MDP **/
	public int policy[];

	/** the computed optimal utility for the given MDP **/
	public double utility[];

	/** the precision used to determine when to stop iterating (called
	 * epsilon in lecture) **/
	public static double precision = 1e-10;

	/**
	 * The constructor for this class.  Computes the optimal policy
	 * for the given <tt>mdp</tt> with given <tt>discount</tt> factor,
	 * and stores the answer in <tt>policy</tt>.  Also stores the
	 * optimal utility in <tt>utility</tt>.
	 */
	public ValueIteration(Mdp mdp, double discount) {
		//vector of utilities for mdp states in S
		double utility = 0;
		double uPrime = 0;
		//max change in utility of any state in an iteration
		double delta = 0;
		double compareDiscount = precision*(1-discount)/discount;

		while(delta >= compareDiscount) {
			utility = uPrime;
			delta = 0;
			for(int i = 0; i < mdp.stateName.length; i++) {
				
				uPrime = BellEquation(mdp.reward[i], discount);
				if(uPrime - utility > delta) {
					delta = uPrime-utility;
				}
			}

		}
	}

	//this is the value after the sigma in the Bellman Equation
	public double expectedUtility(int state, int action, Mdp mdp) {
		double expectedValue = 0;
		//list of all possible next states after taking this action
		int[] nextStates = mdp.nextState[state][action];
		for(int i = 0; i <nextStates.length; i++) {
			int nextState = nextStates[i];
			double prob = mdp.transProb[state][action][i];
			double util = utility[nextState];
			expectedValue += prob*util;
		}
		return expectedValue;
	}

	//returns the value of the new vector
	public double BellEquation(double reward, double disc) {
		double vector = 0;


		return vector;
	}

}
