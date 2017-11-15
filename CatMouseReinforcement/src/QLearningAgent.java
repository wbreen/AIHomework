/**
 * This is the template of a class that should define a Q-learning
 * agent for an MDP with parameters passed into the constructor.  You
 * will need to fill in three parts: <ol> <li> The constructor to
 * initialize any fields you need.  <li> The exploration function to
 * evaluate the trade-off between exploration and exploitation. <li>
 * The <tt>doStep</tt> function in which you can update any of the
 * Q-learning agent's internal structures based on the state and
 * reward, and return the next action to take.  </ol> You may wish to
 * add other fields with other useful information.
 */

import java.util.*;

public class QLearningAgent {

	/** the Q-value table. <tt>q[a][s]</tt> is the action value where
	 * <tt>a</tt> is the action and <tt>s</tt> is the state.
	 **/
	public double Q[][];

	/** the table of frequencies for state-action pairs.
	 * <tt>N[s][a]</tt> is the frequency of state <tt>s</tt> and
	 * action <tt>a</tt>.
	 **/
	public int N[][];

	public double highestUtility = 2;

	public double discount;

	public int numStates;

	public int numActions;

	public int prevAction = -1;
	public int prevState = -1;
	//public double prevReward = -1;

	public final double ALPHA = .2;

	public double[] utility;
	public int[] policy;

	/**
	 * The constructor for this class.  Initializes any internal
	 * structures needed for an MDP problem having <tt>numStates</tt>
	 * states and <tt>numActions</tt> actions.  The reward discount
	 * factor of this system is given by <tt>discount</tt>.
	 **/
	public QLearningAgent(int numSta, int numAct, double discountFactor) {
		//System.out.println("Discount is "+discountFactor);
		Q = new double[numAct][numSta];
		for(int i = 0; i< Q.length; i++) {
			for(int j = 0; j<Q[i].length; j++) {
				Q[i][j] = Math.random()*10-5;
			}
		}
		N = new int[numSta][numAct];
		discount = discountFactor;
		//discount = 1;
		numStates = numSta;
		numActions = numAct;
	}


	/**
	 * This function should return the utility of each state.
	 **/
	public double[] getUtility() {
		if(utility == null)
			getPolicy();
		return utility;
	}


	/**
	 * This function should return the policy of each state.
	 **/
	public int[] getPolicy() {
		if(policy == null) {
			policy = new int[numStates];
			utility = new double[numStates];
			//System.out.println("In getPolicy with "+numStates);
			for(int s = 0; s< numStates; s++) {
				int bestAction = 0;
				double bestQ = Double.NEGATIVE_INFINITY;
				for(int a = 0; a < numActions; a++) {
					if(Q[a][s]>bestQ) {
						bestQ = Q[a][s];
						bestAction = a;
					}
				}
				utility[s]=bestQ;
				policy[s]=bestAction;
			}
		}
		return policy;
	}



	/**
	 * The exploration function, as a function of utility <tt>u</tt>
	 * and the number of times an action-state pair has been taken,
	 * <tt>n</tt>.
	 **/
	public double explorationFunction(double u, int n) {
		if(u > highestUtility){
			highestUtility = u;
		}
		if (n < 5){
			return highestUtility;
		}
		else{
			return u;
		}
		//return u;
	}


	/**
	 * Do a single step of the Q-learning agent.  The inputs to the
	 * agent are the current state <tt>state</tt> and the reward
	 * signal <tt>reward</tt>.  This function should return the action
	 * taken by the agent.
	 **/
	public int doStep(int state, double reward) {
		//System.out.println("State is "+(state+1));
		//System.out.println("Reward is "+reward);
		if(prevState != -1) {
			//	System.out.println("In the if statement");
			N[prevState][prevAction] = N[prevState][prevAction]+1;
			//int aPrime = -1;
			double bestQ = Double.NEGATIVE_INFINITY;
			for(int a = 0; a < numActions; a++) {
				if(Q[a][state]>bestQ) {
					bestQ = Q[a][state];
					//aPrime = a;
				}
			}
			//System.out.println("Best Q: "+bestQ);
			//double value = Q[prevAction][prevState] + ALPHA*N[prevState][prevAction]*
			//  (reward + discount*bestQ - Q[prevAction][prevState]);
			double value = Q[prevAction][prevState] + ALPHA*
					(reward + discount*bestQ - Q[prevAction][prevState]);
			//System.out.println("Value = "+value);
			Q[prevAction][prevState] = value;
		}
		prevState = state;
		//prevReward = reward;
		int bestAction = 0;
		double bestValue = Double.NEGATIVE_INFINITY;
		for(int a = 0; a < numActions; a++) {

			if(explorationFunction(Q[a][state],N[state][a]) > bestValue) {
				bestValue = explorationFunction(Q[a][state],N[state][a]);
				bestAction = a;
			}
		}
		//int total = 0;
		//for(int i = 0; i< numActions; i++) {
		//for(int j = 0; j < numStates; j++) {
		//		total = total + N[j][i];
		//	}
		//}
		//if (total == 1) bestAction = 1;
		prevAction = bestAction;

		//double[] nums = this.getUtility();
		//System.out.println(Arrays.toString(nums));
		//System.out.println("End Iteration");
		//System.out.println("Q:");
		//for(int a = 0; a < numActions; a++) {
		//	for(int s = 0; s < numStates; s++) {
		//		System.out.println("Q["+a+"]["+s+"] =" + Q[a][s]);
		//	}
		//}
		//System.out.println("Returning action "+(prevAction+1));
		return prevAction;


	}
};
