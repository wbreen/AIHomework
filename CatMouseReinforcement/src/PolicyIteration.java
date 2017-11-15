/**
 * This is the template of a class that should run policy iteration on
 * a given MDP to compute the optimal policy which is returned in the
 * public <tt>policy</tt> field.  You need to fill in the constructor.
 * You may wish to add other fields with other useful information that
 * you want this class to return (for instance, number of iterations
 * before convergence).
 */
public class PolicyIteration {

	/** the computed optimal policy for the given MDP **/
	public int policy[];
	
	public double utility[];

	/**
	 * The constructor for this class.  Computes the optimal policy
	 * for the given <tt>mdp</tt> with given <tt>discount</tt> factor,
	 * and stores the answer in <tt>policy</tt>.
	 */
	public PolicyIteration(Mdp mdp, double discount) {
		utility = new double[mdp.numStates];
		policy = new int[mdp.numStates];
		for(int i =0; i< policy.length; i++){
			policy[i] = (int)(Math.random()*(mdp.numActions));
		}
		boolean unchanged = false;
		while(!unchanged){
			PolicyEvaluation eval = new PolicyEvaluation(mdp, discount, policy);
			for(int i = 0; i< utility.length; i++){
				utility[i] = eval.utility[i];
			}
			unchanged = true;
			for(int s = 0; s<mdp.numStates; s++){
				if (findBiggestSum(s, mdp) > computeSum(policy[s],s,mdp)){
					policy[s] = findBestAction(s,mdp);
					unchanged = false;
				}
			}
		}
	}  

	public double computeSum(int action, int state, Mdp mdp){
		double sum = 0;
		for(int i = 0; i<mdp.nextState[state][action].length; i++){
			sum = sum + mdp.transProb[state][action][i]*utility[mdp.nextState[state][action][i]];
		}
		return sum;
	}
	public double findBiggestSum(int state, Mdp mdp){
		double max = Double.NEGATIVE_INFINITY;
		for(int i = 0; i< mdp.numActions; i++){
			double sum = computeSum(i, state, mdp);
			if(sum > max){
				max = sum;
			}
		}
		return max;
	}
	public int findBestAction(int state, Mdp mdp){
		double max = Double.NEGATIVE_INFINITY;
		int action = 0;
		for(int i = 0; i< mdp.numActions; i++){
			double sum = computeSum(i, state, mdp);
			if(sum > max){
				max = sum;
				action = i;
			}
		}
		return action;
	}
}
