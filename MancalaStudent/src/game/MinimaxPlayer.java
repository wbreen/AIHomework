package game;

public class MinimaxPlayer implements Player{
	Minimax algorithm;
	String name;
	boolean max;
	int depth;
	
	public MinimaxPlayer(String n, boolean isNorth, int level){
		name = n;
		algorithm = new Minimax();
		max = isNorth;
		depth = level;
	}
	
	public String getName(){
		return name;
	}
	
	public int move(GameBoard game) {
		return algorithm.getMove(game, depth, max,-1*Double.MAX_VALUE, Double.MAX_VALUE);
	}
	
	// This number should be high if I'm max (North) and low if I'm min (South).
	public int heuristic(GameBoard game) {
		return algorithm.heuristic(game);
	}
}
