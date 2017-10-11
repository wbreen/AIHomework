package game;
/*
 * Shannon Duvall
 * The abstract description of what a game's board is.
 */
import java.util.*;

public interface GameBoard {

	// List all possible moves
	public List<Integer> getPossibleMoves();
	// Get a copy of the board
	public GameBoard clone();
	// Move in the game.  Returns whether or not 
	// the move resulted in a "go again" state.
	public boolean moveMade(int move);
	// Report if the game is done
	public boolean gameOver();
	// Print out state of the board for debugging purposes
	public String getStateDescription();
	// Give a default heuristic value for the board
	public int defaultHeuristic();
}
