package mancala;
/*
 * Shannon Duvall
 * Mancala Game
 * Implements the Game interface.
 */

import game.*;

import javax.swing.JOptionPane;
import java.util.*;

public class MancalaGame implements GameBoard{
	public final int NUM_PITS;
	public final int NORTH_PIT_START;
	public final int NORTH_PIT_END;
	public final int SOUTH_PIT_START;
	public final int SOUTH_PIT_END;
	public final int MANCALA_NORTH;
	public final int MANCALA_SOUTH;

	// The start state of the mancala board.  The first 
	// pots are the south pits, then the south mancala, then the north pits, then the north mancala.
	// This is the default start state of the board.
	private int[] state = {3,3,3,3,3,3,0,3,3,3,3,3,3,0};
	private boolean northsTurn;
	private Player playerNorth;
	private Player playerSouth;
	private int lastPit;

	public MancalaGame(){
		NUM_PITS = 6;
		NORTH_PIT_START = NUM_PITS+1;
		NORTH_PIT_END = NUM_PITS*2;
		SOUTH_PIT_START = 0;
		SOUTH_PIT_END = NUM_PITS-1;
		MANCALA_NORTH = state.length-1;
		MANCALA_SOUTH = NUM_PITS;
		playerNorth = new GUIPlayer("User");
		//playerSouth = new GUIPlayer("South");
		playerSouth = new MinimaxPlayer("Computer", false,10);
		northsTurn = false;
		// ask who should go first
		int ans = JOptionPane.showConfirmDialog(null,
				"North Player is "+playerNorth.getName()+" \n"+
				"South Player is "+playerSouth.getName()+" \n"+
				"Should North go first?", 
				"Who's First?",JOptionPane.YES_NO_OPTION);
		if (ans == JOptionPane.YES_OPTION) {
			northsTurn = true;
		} 
		StringWriter.turnOff();
		// Now the mouse click will take care of playing the 
		// rest of the game...
	}

	/*
	 * Used by Minimax algorithm to create board scenarios.
	 */
	private MancalaGame(int[] newState, int last, boolean whoseTurn, Player n, Player s){
		state = newState;
		NUM_PITS = (state.length-2)/2;
		NORTH_PIT_START = NUM_PITS+1;
		NORTH_PIT_END = NUM_PITS*2;
		SOUTH_PIT_START = 0;
		SOUTH_PIT_END = NUM_PITS-1;
		MANCALA_NORTH = state.length-1;
		MANCALA_SOUTH = NUM_PITS;
		northsTurn = whoseTurn;
		playerNorth = n;
		playerSouth = s;
		lastPit = last;
	}
	
	/*
	 * Used by Minimax algorithm to create board scenarios.
	 */
	public MancalaGame clone(){
		return new MancalaGame(this.state.clone(), this.lastPit, this.northsTurn, this.playerNorth, this.playerSouth);	
	}
	
	/*
	 * Gives a string description of the board.  It is a string showing the value of each pit.
	 */
	public String getStateDescription(){
		String description = "";
		for(int stateNum:state){
			description+=stateNum+" ";
		}
		return description;
	}
	
	
	
	/*
	 * Tells whose turn it is
	 */
	public boolean getTurn(){
		return northsTurn;
	}
	
	/*
	 * Gets the state of the board
	 */
	public int[] getState(){
		return state;
	}
	
	public boolean isNorth(int pit){
		return NORTH_PIT_START <= pit && pit <= NORTH_PIT_END;
	}
	
	public boolean isSouth(int pit){
		return SOUTH_PIT_START <= pit && pit <= SOUTH_PIT_END;
	}
	
	public int opposite(int pit){
		return state.length-1 -pit - 1;
	}
	
	public boolean northWins(){
		return state[MANCALA_NORTH]>state[MANCALA_SOUTH];
	}
	
	// Get all possible moves.  Each move is its numbered pit.
	// A move is possible if it belongs to the current player and
	// it's not empty.
	public List<Integer> getPossibleMoves(){
		List<Integer> moves = new ArrayList<Integer>();
		for(int i = 0; i<state.length; i++){
			if(isLegalMove(i)){
				moves.add(i);
			}
		}
		return moves;
	}
	
	// Tell if a move is legal.  It is legal if it belongs to the 
	// current player and it's not empty.
	public boolean isLegalMove(int pit){
		return (northsTurn && isNorth(pit) && state[pit]!=0) || (!northsTurn && isSouth(pit) && state[pit]!=0);
	}
	
	// When a click is received, move for the current player.  Call moveMade, 
	// which will change the current state.  When this is over, the applet
	// checks for a winner.
	public void clickReceived(int pitClicked){
        lastPit = pitClicked;
		int move = northsTurn ? playerNorth.move(this.clone()) : playerSouth.move(this.clone());
		if(isLegalMove(move)){
			String player= northsTurn?"North":"South";
			System.out.println("Move made by: "+player+" "+move);
		}
		moveMade(move);
	}

	public int getLastPit(){
		return lastPit;
	}
	
	// Change the state of the board based on the pit that was clicked on. 
	public boolean moveMade(int pit){
		// First make sure it's a legal move.
		if (isLegalMove(pit)){
			int stones = state[pit];
			// Take all the stones from that pit and put them in the upcoming pits.
			state[pit]=0;
			while(stones > 0){
				pit = (pit+1)%state.length;
				// Make sure to skip your opponent's mancala.
				if (!((pit==MANCALA_NORTH && !northsTurn) ||
					pit == MANCALA_SOUTH && northsTurn)){
					state[pit]++;
					stones--;
				}
			}
			int myMancala = MANCALA_SOUTH;
			if(northsTurn){
				myMancala = MANCALA_NORTH;
			}
			
			if (state[pit]==1 && ((northsTurn && isNorth(pit)) || (!northsTurn && isSouth(pit)))) {
				state[pit] = 0;
				state[myMancala]++;
				int opposite = opposite(pit);
				int captured = state[opposite];
				state[opposite] = 0;
				state[myMancala] += captured;
				
			}
			if (pit != myMancala) {
				// Swap whose turn it is.
				northsTurn = !northsTurn;	
				return false;
			}
		}
		return true;
	}

	/*
	 * Tell who the winner is, if the game is over.
	 */
	public Player getWinner(){
		if(gameOver()){
			return northWins() ? playerNorth : playerSouth;
		}
		return null;
	}

	/*
	 * The game is over when someone's pits are all empty.
	 */
	public boolean gameOver(){
		return isSideDone(SOUTH_PIT_START, SOUTH_PIT_END) ||
		       isSideDone(NORTH_PIT_START, NORTH_PIT_END);
	}

	private boolean isSideDone(int sideStart, int sideEnd) {
		for(int i=sideStart; i<=sideEnd; i++){
			if(state[i]!=0){
				return false;
			}
		}
		return true;
	}
	
	public int defaultHeuristic() {
		return 0;
	}
}
