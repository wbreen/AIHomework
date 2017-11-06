package main;

import id3.*;
import problems.RestaurantProblem;
import tree.*;

/*
 * Shannon Duvall
 * Main.java
 */
public class Main {
	public static void main(String[] args) {
		RestaurantProblem rest = new RestaurantProblem();
//		ID3 id3 = new ID3();
//		StringWriter.turnOff();
//		TreeNode root = id3.id3(rest.getExamples(), rest.getAttributes(), rest.getDefaultDecision());
//		System.out.println("Print Tree: ");
//		StringWriter.turnOn();
//		root.printMe();
		ID3Helpers helpers = new ID3Helpers();
		System.out.println(helpers.getEntropy(rest.getExamples()));
	}

}
