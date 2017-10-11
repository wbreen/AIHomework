import nodes.Node;
import nodes.NodeGame;
import game.Minimax;
import game.StringWriter;

/*
 * Runs a nodegame with a minimax player for purposes of testing
 * the minimax algorithm.
 */
public class Main {
	public static void main(String[] args){
		NodeGame game = new NodeGame();
		Minimax mini = new Minimax();
		double value = mini.getValue(game, 3, true, -1*Double.MAX_VALUE, Double.MAX_VALUE);
		System.out.println(value);		
	}
}
