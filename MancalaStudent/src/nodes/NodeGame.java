package nodes;
/*
 * Shannon Duvall
 * NodeGame, along with Node, is made mainly to test 
 * whether or not the alpha-beta pruning works. 
 */

import game.GameBoard;
import game.StringWriter;

import java.util.*;
public class NodeGame implements GameBoard{
	
	// The root of the tree - holds all the info for the whole tree.
	Node root;
	
	public NodeGame(){
		// Build an example tree.  The first is the name of the node, and the second
		// number is the default value heuristic value for that node.
		// Feel free to play with this code.  Test your algorithm on various trees.
		root = new Node("root",0);
		Node one = new Node("a",1);
		Node two = new Node("b",2);
		Node three = new Node("c",3);
		Node four = new Node("d",4);
		Node five = new Node("e",5);
		Node six = new Node("f",6);
		Node seven = new Node("g",7);
		Node eight = new Node("h",8);
		Node nine = new Node("i",9);
		Node ten = new Node("j",2);
		Node eleven = new Node("k",3);
		Node twelve = new Node("l",5);
		Node thirteen = new Node("m",8);
		Node fourteen = new Node("n",0);
		Node fifteen = new Node("o",5);
		Node sixteen = new Node("p",2);
		Node seventeen = new Node("q",2);
		Node eighteen = new Node("r",1);
		Node nineteen = new Node("s",8);
		Node twenty = new Node("t",10);
		four.addChild(ten);
		four.addChild(eleven);
		five.addChild(twelve);
		five.addChild(thirteen);
		six.addChild(fourteen);
		seven.addChild(fifteen);
		seven.addChild(sixteen);
		eight.addChild(seventeen);
		eight.addChild(eighteen);
		nine.addChild(nineteen);
		nine.addChild(twenty);
		one.addChild(four);
		one.addChild(five);
		two.addChild(six);
		two.addChild(seven);
		three.addChild(eight);
		three.addChild(nine);
		root.addChild(one);
		root.addChild(two);
		root.addChild(three);
		
		StringWriter.turnOn();
	}
	
	// Constructor if the root is already given.
	public NodeGame(Node rt){
		root = rt;
	}
	
	// The state of the game is the current node being used as the root.
	public String getStateDescription(){
		return root.getName()+"";
	}
	
	// The heuristic is the current node's value.
	public int defaultHeuristic(){
		return root.getValue();
	}
	
	// The possible moves are the children of the current node.
	// Move 0 is the first child, 1 the second, and so on.
	public List<Integer> getPossibleMoves(){
		List<Integer> moves = new ArrayList<Integer>();
		for(int i = 0; i < root.getChildren().size(); i++){
			moves.add(i);
		}
		return moves;
	}
	
	public NodeGame clone(){
		return new NodeGame(root);
	}
	
	// To move in the game, reset the root to the chosen child.
	public boolean moveMade(int move){
		Node child = root.getChildren().get(move);
		root = child;
		// There is no "go again" option.
		return false;
	}
	
	// The game is over when I'm at a leaf.
	public boolean gameOver(){
		return root.isLeaf();
	}
}
