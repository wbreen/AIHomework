package mancala;
import game.GameBoard;
import game.Player;

/*
 * Shannon Duvall
 * A GUIPlayer for the mancala game.  
 * To move, just give the pot clicked on.
 */
public class GUIPlayer implements Player{
	public String name;
	
	public GUIPlayer(String name){
		this.name = name;
	}
	
	public String getName(){
		return name;
	}
	
	public int move(GameBoard game){
		return ((MancalaGame)game).getLastPit();
	}
	
	public int heuristic(GameBoard game) {
		return 0;
	}
}
