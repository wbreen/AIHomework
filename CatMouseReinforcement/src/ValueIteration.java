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
    		
    }

}
