package game;
import java.util.*;

public class Minimax {
	int bestMove;
	
	/*
	 * Create a heuristic with the knowledge that the North player is 
	 * max and the South player is Min.
	 */
	public int heuristic(GameBoard game) {
		// use this line for testing.  Change to play Mancala.
		return game.defaultHeuristic();
	}

	
	 /* This method should return the MOVE that gives the best value (don't return the best value itself.)
	  * In Mancala, it's a pit number.
	  * In a Node game, it's the branch number 
	  */
	
	public int getMove(GameBoard game, int level, boolean max, double alpha, double beta){
		getValue(game,level,max, alpha, beta);
		return bestMove;
	}
	
	/*
	 * Call at the beginning of the method:
	 * StringWriter.println("Getting value for state: "+game.getStateDescription());
	 * 
	 * When you default to the heuristic:
	 * StringWriter.println("Returning heurustic: "+game.heuristic());
	 * 
	 * Just before you return:
	 * StringWriter.println("Returning for state: "+game.getStateDescription()+": move "+bestMoveSoFar+" evaluating at "+bestValueSoFar);
	 */
	
	/*
	 * Method getMove:
	 * @param game: The game logic
	 * @param level: how deep the search should go before using the heuristic
	 * @param max: true if this player is "max" and false if this player is "min"
	 * @param alpha & beta: values that percolate up the tree.  Added here for recursion ease.
	 * 
	 * You should use the above 3 lines as you process so that the tester can check not only the final answer but
	 * also the process.
	 */
	
	public double getValue(GameBoard game, int level, boolean max, double alpha, double beta){
		return 0;	
	}
}
