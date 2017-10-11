package nodes;
/*
 * Shannon Duvall
 * This object represents a minimax node inside a game tree.  It has a 
 * name (Node 0, for example) a value according to Minimax,
 * and a list of children.
 */
import java.util.*;

public class Node {
	String name;
	int value;
	ArrayList<Node> children;
	
	// Inputs are the node's name and value.  Children
	// will be added one at a time.
	public Node(String n, int v){
		name = n;
		value = v;
		children = new ArrayList<Node>();
	}
	
	public String getName(){
		return name;
	}
	
	public int getValue(){
		return value;
	}
	
	public ArrayList<Node> getChildren(){
		return children;
	}
	
	public void addChild(Node n){
		children.add(n);
	}
	
	// A tree leaf is a node with no children.
	public boolean isLeaf(){
		return children.isEmpty();
	}
}
