package game;
/*
 * Shannon Duvall
 * Interface for a player.
 * A player needs to tell its name and to pick a move.
 */
public interface Player {
	// Return player's name
	public String getName();
	// Returns move value on game board
	public int move(GameBoard game);
	// Give the heuristic for the board
	public int heuristic(GameBoard game);
	
}
