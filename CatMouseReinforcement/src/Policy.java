/**
 * This class encapsulates the policy functionality for different
 * policies, be they of the static variety or the dynamic
 * reinforcement-learning variety.  You should not touch this file.
 */
public class Policy {
    private Mdp mdp;
    private int policy[] = null;
    private QLearningAgent qla = null;

    public Policy(Mdp mdp, int[] policy, QLearningAgent qla) {
	this.mdp = mdp;
	this.policy = policy;
	this.qla = qla;
    }

    public Policy(Mdp mdp, int[] policy) {
	this(mdp,policy,null);
    }

    public Policy(Mdp mdp, QLearningAgent qla) {
	this(mdp,null,qla);
    }

    public int getAction(int state) {
	if (qla != null) 
	    return qla.doStep(state,mdp.reward[state]);
	else
	    return policy[state];
    }
};
