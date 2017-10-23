package game;
import java.util.*;

public class Minimax {
	int bestMove;
	//double bestVal;
	//int bestMoveHere;
	
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
	
	//this returns the value of each 
	public double getValue(GameBoard game, int level, boolean max, double alpha, double beta) {
		//get the list of possible moves for the cloned state
		List<Integer> posMoves = game.getPossibleMoves();
		//set up a bestVal variable to be used and reset each time through the algo
		double bestVal;
		int bestMoveHere = 0;
		int newLevel = level;
		newLevel--;
		StringWriter.println("Getting value for state: "+game.getStateDescription());
		
		//base case for the recursion
		//so when the search gets deep enough, it uses the heuristic its found
		if(level==0 || game.gameOver()|| newLevel < 0) {
			StringWriter.println("Returning heurustic: " + heuristic(game));
			return heuristic(game);
		}
		else if(max) {
			//start by reducing the level you are looking at
			//newLevel--;
			//bestVal is set really low to begin with 
			bestVal = -100000000;
			//int bestMoveHere;
			int possibleMove;
			//translated from pseudocode
			for(int i = 0; i < posMoves.size(); i++) {
				GameBoard cloned = game.clone();
				cloned.moveMade(posMoves.get(i));
				bestMoveHere = posMoves.get(i);
				//possibleMove=posMoves.get(i);
				bestVal = Max(bestVal, getValue(cloned, newLevel, !max, alpha, beta));
				if (bestVal >= beta) {
					StringWriter.println("Returning for state: "+cloned.getStateDescription()+": move "+bestMoveHere+" evaluating at "+bestVal);
					//bestMove = bestMoveHere;
					return bestVal;
				}
				//bestVal = Max(bestVal, getValue(cloned, newLevel, !max, alpha, beta));
				alpha = Max(alpha, bestVal);
			}
			StringWriter.println("Returning for state: "+game.getStateDescription()+": move "+bestMoveHere+" evaluating at "+bestVal);
			bestMove = bestMoveHere;
			return bestVal;
		}
		
		else if(!max) {
			//newLevel--;
			bestVal = 100000000;
			//int bestMoveHere;
			int possibleMove;
			//translated from pseudocode
			for(int i = 0; i < posMoves.size(); i++) {
				GameBoard cloned = game.clone();
				cloned.moveMade(posMoves.get(i));
				bestMoveHere = posMoves.get(i);
				//possibleMove=posMoves.get(i);
				bestVal = Min(bestVal, getValue(cloned, newLevel, !max, alpha, beta));
				if (bestVal <= alpha) {
					StringWriter.println("Returning for state: "+cloned.getStateDescription()+": move "+bestMoveHere+" evaluating at "+bestVal);
					//bestMove = bestMoveHere;
					return bestVal;
				}
				//bestVal = Min(bestVal, getValue(cloned, newLevel, max, alpha, beta));
				beta = Min(bestVal, beta);
				
			}
			StringWriter.println("Returning for state: "+game.getStateDescription()+": move "+bestMoveHere+" evaluating at "+bestVal);
			bestMove = bestMoveHere;
			return bestVal;
			
		}
		else {
			return 0;
		}
	}
	
	//helper method to tell if one value is bigger than another value
	public double Max(double val1, double val2) {
		if(val1 > val2) {
			return val1;
		}
		else {
			return val2;
		}
	}
	
	//helper method to tell if one value is smaller than another value
	public double Min(double val1, double val2) {
		if(val1 < val2) {
			return val1;
		}
		else {
			return val2;
		}
	}
	
	public double goThroughMoves(List<Integer> possibleMoves, int bestVal,GameBoard game, int level, boolean max, double alpha, double beta){
		for(int i=0; i < possibleMoves.size()-1; i++) {
			GameBoard clone = game.clone();
			clone.moveMade(possibleMoves.get(i));
			
		}
		return 0;
	}


}

	
