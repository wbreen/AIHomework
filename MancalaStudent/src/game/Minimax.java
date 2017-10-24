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
	
	//this returns the value of each 
	public double getValue(GameBoard game, int level, boolean max, double alpha, double beta) {
		
		//get the list of possible moves for the cloned state
		List<Integer> posMoves = game.getPossibleMoves();
		
		//set up a bestVal variable to be used and reset each time through the alg
		double bestVal;			//to return
		int bestMoveHere = -1;	//to see which move is the best move from the array of possible moves
		double localCurrVal;
		
		//go down to the next level for later in recursion
		int newLevel = level-1;
		StringWriter.println("Getting value for state: "+game.getStateDescription());
		
		//base case for the recursion
		//so when the search gets deep enough, it uses the heuristic its found
		if(level==0 || game.gameOver()) {
			StringWriter.println("Returning heurustic: " + heuristic(game));
			return heuristic(game);
		}
		else if(max) {
			//bestVal is set really low to begin with 
			bestVal = -100000000;
			localCurrVal = -100000000;
			int moveMade=0;
			
			//translated from pseudocode
			for(int i = 0; i < posMoves.size(); i++) {
				//clone the game so you start with a fresh look at the original board each time
				GameBoard cloned = game.clone();
				//move through each of the possible moves
				cloned.moveMade(posMoves.get(i));
				
				//the move that is being made right now
				moveMade = posMoves.get(i);
				
				//see if the possible move is the best move, and if it is, set possible move to best move
				//how do you determine if the move is the best move?
				//if a move changes the alpha value, then it is the best possible move for it (for max) 
				//if the move changes the beta value (for min), then it is the best possible move for min
				localCurrVal = getValue(cloned, newLevel, !max, alpha, beta);
				bestVal = Max(bestVal, localCurrVal);
				
				if(bestVal <= localCurrVal) {
					alpha = Max(alpha, bestVal);
					bestMoveHere = moveMade;
				} 
//				alpha = Max(alpha, bestVal);
				if (bestVal >= beta) {
					bestMoveHere  = moveMade;
					StringWriter.println("Returning for state: "+game.getStateDescription()+": move "+bestMoveHere+" evaluating at "+bestVal);
					bestMove = bestMoveHere;
					return bestVal;
				}
				//set alpha to its new better value
//				if(bestVal > localCurrVal) {
//					alpha = Max(alpha, bestVal);
//					bestMoveHere = moveMade;
//				} 
				
			}
//			if(bestMoveHere==-1) {
//				bestMoveHere = moveMade;
//			}
			StringWriter.println("Returning for state: "+game.getStateDescription()+": move "+bestMoveHere+" evaluating at "+bestVal);
			bestMove = bestMoveHere;
			return bestVal;
		}
		
		else if(!max) {
			//newLevel--;
			bestVal = 100000000;
			int moveMade=0;
			localCurrVal = 100000000;
			//change this so it will change with the first move, but if the second move is not, I need a way to keep up with the first move through the recursion
			
			//translated from pseudocode
			for(int i = 0; i < posMoves.size(); i++) {
				GameBoard cloned = game.clone();
				cloned.moveMade(posMoves.get(i));
				
				//bestMoveHere = posMoves.get(i);
				moveMade = posMoves.get(i);
				localCurrVal = getValue(cloned, newLevel, !max, alpha, beta);
				
				bestVal = Min(bestVal, localCurrVal);
				
				if(bestVal >= localCurrVal) {
					beta = Min(bestVal, beta);
					bestMoveHere = moveMade;
				}
//				beta = Min(bestVal, beta);
				if (bestVal <= alpha) {
					bestMoveHere = moveMade;
					StringWriter.println("Returning for state: "+game.getStateDescription()+": move "+bestMoveHere+" evaluating at "+bestVal);
					//bestMoveHere  = moveMade;
					bestMove = bestMoveHere;
					return bestVal;
				}
				
				//set beta to its new better value and change bestMoveHere to moveMade
//				if(bestVal < localCurrVal) {
//					beta = Min(bestVal, beta);
//					bestMoveHere = moveMade;
//				}
			}
//			if(bestMoveHere==-1) {
//				bestMoveHere = moveMade;
//			}
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
		if(val1 >= val2) {
			return val1;
		}
		else {
			return val2;
		}
	}
	
	//helper method to tell if one value is smaller than another value
	public double Min(double val1, double val2) {
		if(val1 <= val2) {
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

	
