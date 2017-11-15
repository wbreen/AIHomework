import java.util.*;
import java.lang.*;
import java.io.*;

/**
 * This class provides a simple main for running on cat and mouse
 * data.
 */
public class RunCatMouse {
    
    /** This is a simple main.  When invoked, it does the following:
     * (1) loads an MDP from a file named in the command line; (2)
     * runs value iteration and policy iteration; (3) runs policy
     * evaluation on the policy returned by policy iteration; (4)
     * prints out the optimal policy and utilities returned by both
     * value iteration and policy iteration; and (5) animates the cat
     * chasing the mouse, depending on the command-line arguments.
     * (Note that this animation will almost certainly crash if run on
     * MDP's other than those for a cat chasing a mouse.)
     *
     * <p>The command-line arguments should consist of a possible list
     * of options, followed by the name of a data file containing a
     * description of the MDP.  By default, a GUI (graphical user
     * interface) based animation will be invoked.  However, this can
     * be changed with the appropriate command-line options: Using the
     * "<tt>-b</tt> option will run the GUI while simultaneously
     * printing a transcript of all states visited.  Using the <tt>-p
     * &lt;num&gt;</tt> option will not invoke the GUI at all but will
     * instead run the MDP for <tt>&lt;num&gt;</tt> steps, while printing
     * the results.  Finally, using the <tt>-n</tt> option will
     * neither invoke the GUI nor print any results.
     *
     * <p>It is okay to change this main as you wish.  However, your
     * code should still work properly when using this one.
     */
    public static void main(String argv[])
    throws FileNotFoundException, IOException {
        
        double discount = 0.95;
        
        // parse options
        Options options = null;
        try {
            options = new Options(argv);
        } catch (Exception e) {
            printCommandLineError();
            return;
        }
        
        // build MDP
        Mdp mdp = new Mdp(options.filename);
        QLearningAgent qla = null;
        int policy[][] = new int[3][];
	double util[][] = new double[3][];
        int numMethods = 0;
	Policy pi = null;

	if (options.doValueIteration) {
	    System.out.println("Running Value Iteration");
            // run value iteration
            ValueIteration vpi = new ValueIteration(mdp, discount); 
	    util[numMethods] = vpi.utility;
	    policy[numMethods] = vpi.policy;
	    numMethods++;
	    pi = new Policy(mdp,vpi.policy);
	}

	if (options.doPolicyIteration) {
	    System.out.println("Running Policy Iteration");
            // run policy iteration
            PolicyIteration ppi = new PolicyIteration(mdp, discount);
            // evaluate returned policy
            util[numMethods] =(new PolicyEvaluation(mdp, discount, ppi.policy)).utility;
	    policy[numMethods] = ppi.policy;
	    numMethods++;
	    pi = new Policy(mdp,ppi.policy);
	}

	if (options.doQLearner) {
	    System.out.println("Training Q-Learning Agent");
	    qla = new QLearningAgent(mdp.numStates,mdp.numActions,discount); 
	    pi = new Policy(mdp,qla);
	
	    /* Do the training */
	
	    int cur_state = mdp.startState;
	    for (int i=0;i<options.skip_steps;i++)
		cur_state = mdp.computeNextState(cur_state,
						 pi.getAction(cur_state));

	    util[numMethods] = qla.getUtility();
	    policy[numMethods] = qla.getPolicy();
	    numMethods++;
	}
	
    
	/* Print out the policies and the utilities*/
	for(int s = 0; s < mdp.numStates; s++) {
	    System.out.print(mdp.stateName[s] + "\t");
	    for (int i=0;i<numMethods;i++) {
	    	//System.out.println("i is "+i+" s is "+s);
		System.out.print(mdp.actionName[policy[i][s]]+"\t");
	    }
	    for (int i=0;i<numMethods;i++) 
		System.out.print(util[i][s] + "\t");
	    System.out.println("");
	}
	System.out.println();
	System.out.println();


	// animate cat chasing mouse
        
        if (options.mode == NO_ANIMATION)
            return;
        
        CatMouseAnimator animator = new CatMouseAnimator(mdp);
        
        switch (options.mode) {
            case GUI_ONLY:
                animator.animateGuiOnly(pi);
                break;
            case GUI_WITH_TRANS:
                animator.animateGuiAndPrint(pi);
                break;
            case PRINT_ONLY:
                animator.animatePrintOnly(pi, options.anim_steps);
                break;
        }
        
    }
    
    // private stuff for parsing command line options and printing
    // error messages
    
    private static final int GUI_ONLY       = 0;
    private static final int GUI_WITH_TRANS = 1;
    private static final int PRINT_ONLY     = 2;
    private static final int NO_ANIMATION   = 3;
    
    private static class Options {
        private String filename = null;
        private int mode = GUI_ONLY;
        private int anim_steps = 0;
        private int skip_steps = 0;
        private boolean doQLearner = false;
	private boolean doValueIteration = false;
	private boolean doPolicyIteration = false;
        
        private Options(String argv[]) {
            for (int i = 0; i < argv.length; i++) {
                if (argv[i].equals("-g")) {
                    mode = GUI_ONLY;
                } else if (argv[i].equals("-p")) {
                    mode = PRINT_ONLY;
                    anim_steps = Integer.parseInt(argv[++i]);
                } else if (argv[i].equals("-b")) {
                    mode = GUI_WITH_TRANS;
                } else if (argv[i].equals("-n")) {
                    mode = NO_ANIMATION;
                } else if (argv[i].equals("-q")) {
                    doQLearner = true;
                    skip_steps = Integer.parseInt(argv[++i]);
                } else if (argv[i].equals("-vi")) {
                    doValueIteration = true;
                } else if (argv[i].equals("-pi")) {
                    doPolicyIteration = true;
                } else if (filename == null) {
                    filename = argv[i];
                } else
                    throw new RuntimeException("filename specified twice");
            }
            if (filename == null)
                throw new RuntimeException("no filename specified");

	    if (!doQLearner && !doValueIteration && !doPolicyIteration) 
		throw new RuntimeException("at least one of -q, -vi, and -pi must be specified");
        }
        
    }

    
    private static void printCommandLineError() {
        System.err.println("error parsing command-line arguments.");
        System.err.println("arguments: [options] <filename>");
        System.err.println("  options:  -g         run GUI only, but do not print results (default)");
        System.err.println("            -p <num>   do not invoke GUI, but print results for <num> steps");
        System.err.println("            -b         run GUI, and also print results");
        System.err.println("            -n         do not invoke GUI and do not print results");

	// Which steps to do?
        System.err.println("            -q <num>   run a Q-learning agent that has already been trained for <num> steps");
        System.err.println("            -vi        run value iteration");
        System.err.println("            -pi        run policy iteration");
    }    
}
