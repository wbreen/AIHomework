import game.Minimax;
import game.StringWriter;
import junit.framework.TestCase;
import nodes.*;

public class AlphaBetaTester extends TestCase {
	Minimax myMinimax;
	public AlphaBetaTester() {
		myMinimax = new Minimax();
	}
	
	public void testAlphaBeta() {
		Node root = buildGame();
		NodeGame game = new NodeGame(root);
		StringWriter.turnOn();
		StringWriter.setToString(true);
		double value = myMinimax.getValue(game, 3, true, -1*Double.MAX_VALUE, Double.MAX_VALUE);
		assertEquals(value, 3.0);
		String answer = "Getting value for state: root\n" + 
				"Getting value for state: a\n" + 
				"Getting value for state: d\n" + 
				"Getting value for state: j\n" + 
				"Returning heurustic: 2\n" + 
				"Getting value for state: k\n" + 
				"Returning heurustic: 3\n" + 
				"Returning for state: d: move 1 evaluating at 3.0\n" + 
				"Getting value for state: e\n" + 
				"Getting value for state: l\n" + 
				"Returning heurustic: 5\n" + 
				"Returning for state: e: move 0 evaluating at 5.0\n" + 
				"Returning for state: a: move 0 evaluating at 3.0\n" + 
				"Getting value for state: b\n" + 
				"Getting value for state: f\n" + 
				"Getting value for state: n\n" + 
				"Returning heurustic: 0\n" + 
				"Returning for state: f: move 0 evaluating at 0.0\n" + 
				"Returning for state: b: move 0 evaluating at 0.0\n" + 
				"Getting value for state: c\n" + 
				"Getting value for state: h\n" + 
				"Getting value for state: q\n" + 
				"Returning heurustic: 2\n" + 
				"Getting value for state: r\n" + 
				"Returning heurustic: 1\n" + 
				"Returning for state: h: move 0 evaluating at 2.0\n" + 
				"Returning for state: c: move 0 evaluating at 2.0\n" + 
				"Returning for state: root: move 0 evaluating at 3.0\n";
		String flush = StringWriter.flushString();
		assertEquals(answer,flush);
		
		Node root2 = buildGame2();
		NodeGame game2 = new NodeGame(root2);

		value = myMinimax.getValue(game2, 4, true, -1*Double.MAX_VALUE, Double.MAX_VALUE);
		assertEquals(value, 5.0);
		answer = "Getting value for state: a\n" + 
				"Getting value for state: b\n" + 
				"Getting value for state: d\n" + 
				"Getting value for state: h\n" + 
				"Getting value for state: p\n" + 
				"Returning heurustic: 10\n" + 
				"Getting value for state: q\n" + 
				"Returning heurustic: 2\n" + 
				"Returning for state: h: move 1 evaluating at 2.0\n" + 
				"Getting value for state: i\n" + 
				"Getting value for state: r\n" + 
				"Returning heurustic: 5\n" + 
				"Getting value for state: s\n" + 
				"Returning heurustic: 12\n" + 
				"Returning for state: i: move 0 evaluating at 5.0\n" + 
				"Returning for state: d: move 1 evaluating at 5.0\n" + 
				"Getting value for state: e\n" + 
				"Getting value for state: j\n" + 
				"Getting value for state: t\n" + 
				"Returning heurustic: 7\n" + 
				"Getting value for state: u\n" + 
				"Returning heurustic: 4\n" + 
				"Returning for state: j: move 1 evaluating at 4.0\n" + 
				"Getting value for state: k\n" + 
				"Getting value for state: v\n" + 
				"Returning heurustic: 6\n" + 
				"Getting value for state: w\n" + 
				"Returning heurustic: 9\n" + 
				"Returning for state: k: move 0 evaluating at 6.0\n" + 
				"Returning for state: e: move 1 evaluating at 6.0\n" + 
				"Returning for state: b: move 0 evaluating at 5.0\n" + 
				"Getting value for state: c\n" + 
				"Getting value for state: f\n" + 
				"Getting value for state: l\n" + 
				"Getting value for state: x\n" + 
				"Returning heurustic: 5\n" + 
				"Returning for state: l: move 0 evaluating at 5.0\n" + 
				"Getting value for state: m\n" + 
				"Getting value for state: z\n" + 
				"Returning heurustic: 4\n" + 
				"Returning for state: m: move 0 evaluating at 4.0\n" + 
				"Returning for state: f: move 0 evaluating at 5.0\n" + 
				"Returning for state: c: move 0 evaluating at 5.0\n" + 
				"Returning for state: a: move 0 evaluating at 5.0\n";
		flush = StringWriter.flushString();
		assertEquals(answer,flush);
		
		Node root3 = buildGame3();
		NodeGame game3 = new NodeGame(root3);

		value = myMinimax.getValue(game3, 14, false, -1*Double.MAX_VALUE, Double.MAX_VALUE);
		assertEquals(value, 3.0);
		answer = "Getting value for state: a\n" + 
				"Getting value for state: b\n" + 
				"Getting value for state: e\n" + 
				"Getting value for state: j\n" + 
				"Returning heurustic: 7\n" + 
				"Returning for state: e: move 0 evaluating at 7.0\n" + 
				"Getting value for state: f\n" + 
				"Getting value for state: k\n" + 
				"Returning heurustic: 5\n" + 
				"Returning for state: f: move 0 evaluating at 5.0\n" + 
				"Returning for state: b: move 0 evaluating at 7.0\n" + 
				"Getting value for state: c\n" + 
				"Getting value for state: g\n" + 
				"Getting value for state: n\n" + 
				"Returning heurustic: 3\n" + 
				"Getting value for state: o\n" + 
				"Returning heurustic: 4\n" + 
				"Returning for state: g: move 0 evaluating at 3.0\n" + 
				"Returning for state: c: move 0 evaluating at 3.0\n" + 
				"Getting value for state: d\n" + 
				"Getting value for state: h\n" + 
				"Getting value for state: p\n" + 
				"Returning heurustic: 9\n" + 
				"Getting value for state: q\n" + 
				"Returning heurustic: 8\n" + 
				"Returning for state: h: move 1 evaluating at 8.0\n" + 
				"Returning for state: d: move 0 evaluating at 8.0\n" + 
				"Returning for state: a: move 1 evaluating at 3.0\n";
		flush = StringWriter.flushString();
		assertEquals(answer,flush);
	}

	public Node buildGame() {
		Node root = new Node("root",0);
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
		return root;
	}
	
	public Node buildGame2() {
		Node a = new Node("a",0);
		Node b = new Node("b",0);
		Node c = new Node("c",0);
		Node d = new Node("d",0);
		Node e = new Node("e",0);
		Node f = new Node("f",0);
		Node g = new Node("g",0);
		Node h = new Node("h",0);
		Node i = new Node("i",0);
		Node j = new Node("j",0);
		Node k = new Node("k",0);
		Node l = new Node("l",0);
		Node m = new Node("m",0);
		Node n = new Node("n",0);
		Node o = new Node("o",0);
		Node p = new Node("p",10);
		Node q = new Node("q",2);
		Node r = new Node("r",5);
		Node s = new Node("s",12);
		Node t = new Node("t",7);
		Node u = new Node("u",4);
		Node v = new Node("v",6);
		Node w = new Node("w",9);
		Node x = new Node("x",5);
		Node y = new Node("y",9);
		Node z = new Node("z",4);
		Node aa = new Node("aa",7);
		Node bb = new Node("bb",2);
		Node cc = new Node("cc",3);
		Node dd = new Node("dd",7);
		Node ee = new Node("ee",8);
		a.addChild(b);
		a.addChild(c);
		b.addChild(d);
		b.addChild(e);
		c.addChild(f);
		c.addChild(g);
		d.addChild(h);
		d.addChild(i);
		e.addChild(j);
		e.addChild(k);
		f.addChild(l);
		f.addChild(m);
		g.addChild(n);
		g.addChild(o);
		h.addChild(p);
		h.addChild(q);
		i.addChild(r);
		i.addChild(s);
		j.addChild(t);
		j.addChild(u);
		k.addChild(v);
		k.addChild(w);
		l.addChild(x);
		l.addChild(y);
		m.addChild(z);
		m.addChild(aa);
		n.addChild(bb);
		n.addChild(cc);
		o.addChild(dd);
		o.addChild(ee);
		
		return a;
	}
	
	public Node buildGame3() {
		Node a = new Node("a",0);
		Node b = new Node("b",0);
		Node c = new Node("c",0);
		Node d = new Node("d",0);
		Node e = new Node("e",0);
		Node f = new Node("f",0);
		Node g = new Node("g",0);
		Node h = new Node("h",0);
		Node i = new Node("i",0);
		Node j = new Node("j",7);
		Node k = new Node("k",5);
		Node l = new Node("l",2);
		Node m = new Node("m",9);
		Node n = new Node("n",3);
		Node o = new Node("o",4);
		Node p = new Node("p",9);
		Node q = new Node("q",8);
		Node r = new Node("r",1);
		Node s = new Node("s",7);
		
		a.addChild(b);
		a.addChild(c);
		a.addChild(d);
		b.addChild(e);
		b.addChild(f);
		c.addChild(g);
		d.addChild(h);
		d.addChild(i);
		e.addChild(j);
		f.addChild(k);
		f.addChild(l);
		f.addChild(m);
		g.addChild(n);
		g.addChild(o);
		h.addChild(p);
		h.addChild(q);
		i.addChild(r);
		i.addChild(s);
		
		
		return a;
	}
}
