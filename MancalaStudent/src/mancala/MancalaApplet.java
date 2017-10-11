package mancala;
/*
 * Shannon Duvall
 * The Mancala applet.
 * 
 */

import game.Player;
import java.applet.Applet;
import java.awt.*;
import java.awt.event.*;
import javax.swing.JOptionPane;

public class MancalaApplet extends Applet implements MouseListener{
	private static final long serialVersionUID = 1L;

	MancalaGame game;
	BoardDrawer board;
	// Keep up with the last clicked point in case there's a GUI player.
	Point lastClicked = null;

	/*
	 *  Called by the applet once at the start.
	 */
	public void init(){	
		game = new MancalaGame();
		board = new BoardDrawer();
		setBackground(Color.gray);
		this.setSize(600,500);
		addMouseListener(this);		// Enables the mouse
	}
	
	public Point getLastClicked(){
		return lastClicked;
	}

	/*
	 * This method displays the Round winner's name
	 */
	public void proclaimWinner(Player p){
		JOptionPane.showMessageDialog(null,
				p.getName()+ " wins the game!",
				"WINNER!",
				JOptionPane.WARNING_MESSAGE);
	}

	/*
	 *  Called by repaint() to explicitly redraw the applet.
	 */
	public void paint(Graphics page)
	{
		board.draw(page, game.getState(), game.getTurn());
	}

	/*
	 * Handle a mouse click event. This signals that 
	 * some player may play.
	 */
	public void mouseClicked(MouseEvent event){
		lastClicked = event.getPoint();// returns a x,y coordinate
		game.clickReceived(board.getPit(lastClicked));
		if(game.gameOver()){
			proclaimWinner(game.getWinner());
		}
		repaint();
	}

	// Required mouse methods
	public void mousePressed(MouseEvent event) {}
	public void mouseDragged(MouseEvent event) {}
	public void mouseReleased(MouseEvent event) {}
	public void mouseEntered(MouseEvent event) {}
	public void mouseExited(MouseEvent event) {}
	public void mouseMoved(MouseEvent event) {}
}

