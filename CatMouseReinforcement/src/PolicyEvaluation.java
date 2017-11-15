/**
 * This is the template of a class that evaluates a given policy,
 * i.e., computes the utility of each state when actions are chosen
 * according to it.  The utility is returned in the public
 * <tt>utility</tt> field.  You need to fill in the constructor.  You
 * may wish to add other fields with other useful information that 
 * you want this class to return (for instance, number of iterations
 * before convergence).
 */
public class PolicyEvaluation {

    /** the computed utility of each state under the given policy */
    public double utility[];

    /** the precision used to determine when to stop iterating (called
     * epsilon in lecture) **/
    public static double precision = 1e-10;

    /**
     * The constructor for this class.  Computes the utility of policy
     * <tt>pi</tt> for the given <tt>mdp</tt> with given
     * <tt>discount</tt> factor, and stores the answer in
     * <tt>utility</tt>.
     */
    public PolicyEvaluation(Mdp mdp, double discount, int pi[]) {

	// your code here
    	utility = new double[pi.length];
		double[] nextUtil = new double[pi.length];
		boolean done = false;
		while(!done){
			done = true;
			for(int s = 0; s < utility.length; s++){
				int action = pi[s];
				nextUtil[s] = mdp.reward[s] + discount*computeSum(action,s,mdp);
				if(Math.abs(nextUtil[s]-utility[s])>precision){
					done = false;
				}
			}
			for(int i =0; i< utility.length; i++){
				utility[i] = nextUtil[i];
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

}
