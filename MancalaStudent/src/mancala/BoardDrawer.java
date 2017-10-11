package mancala;
/*
 * Shannon Duvall
 * This class draws a Mancala board
 */
import java.awt.*;

public class BoardDrawer {
	// colors for drawing the board
	private static final Color MANCALA_COLOR = Color.WHITE;
	private static final Color PIT_COLOR = Color.WHITE;
	private static final Color SOUTH_COLOR = Color.RED;
	private static final Color NORTH_COLOR = Color.BLUE;
	private static final Color MARKER_COLOR = Color.GREEN;
	private static final Color BOARD_COLOR = new Color(139,69,19);
	// board location on screen
	private final int BOARDX = 100;
	private final int BOARDY = 100;
	private final int BOARD_HEIGHT = 200;
	private final int BOARD_WIDTH = 400;
	// These are the number of pits between the mancalas. 
	// The number of pits can be changed and everything else will work okay.
	// I'm just that awesome.
	private final int NUM_PITS = 6;
	// The offset divides the board into pits and two mancalas.
	// It is the width of one board "section".
	private final double OFFSET = BOARD_WIDTH/(NUM_PITS+2);
	// Some pixels between pits
	private final int PADDING = 10;
	// Some pixels between pit and writing inside the pit
	private final int STRING_PADDING = (int)OFFSET/2;

	/*
	 * Draw the board.
	 * Inputs are the Graphics object for drawing, an array of ints 
	 * representing the values of each pot on the board, and a boolean 
	 * representing whether or not it is North player's turn.
	 */
	public void draw(Graphics page, int[] board, boolean northsTurn){
		// Draw a brown board on a white background.
		page.setColor(MANCALA_COLOR);
		page.fillRect(0,0,BOARD_WIDTH+300,BOARD_HEIGHT+300);
		Color brown = BOARD_COLOR;
		page.setColor(brown);
		page.fillRect(BOARDX, BOARDY, BOARD_WIDTH, BOARD_HEIGHT);
		// Put a small green square (25x25) beside whoever's turn it is.
		page.setColor(MARKER_COLOR);
		if(northsTurn){
			page.fillRect(250, 35, 25, 25);
		}
		else{
			page.fillRect(250, 385, 25, 25);
		}
		drawPits(page, board, NORTH_COLOR, true);
		drawPits(page, board, SOUTH_COLOR, false);
	}
	
	/*
	 * This method draws the pits, label, and mancala for one side of the board.
	 * Inputs are: the graphics object for drawing, the array of ints telling 
	 * how many stones in each pit & mancala, the Color for this half of the board, 
	 * and whether or not this is the top half of the board being drawn.
	 */
	public void drawPits(Graphics page, int[] board, Color playerColor, boolean isNorth){
		// These variables are what change, depending on if this is the top or bottom 
		// half of the board.
		int mancalaX = BOARDX;
		String label = "North";
		int labelY = 50;
		int pitY = BOARDY;
		// The # stones in the north mancala is last in the array.
		int myMancalaIndex = board.length-1;
		// Re-calculate the values if I'm drawing the southern half.
		if(!isNorth){
			mancalaX = (BOARDX+BOARD_WIDTH)-(int)OFFSET;
			//mancalaX = mancalaX + 350;
			label = "South";
			labelY = labelY+350;
			// The # stones in the south mancala is in the middle of the array, after each of the south pits.
			myMancalaIndex = NUM_PITS;
			pitY = pitY+150;
		}
		
		// Draw the mancala and write in it the number of stones in it.  Write "North" or "South".
		page.setColor(MANCALA_COLOR);
		page.fillOval(mancalaX+PADDING,BOARDY+PADDING,(int)OFFSET-2*PADDING, BOARD_HEIGHT-2*PADDING);
		page.setColor(playerColor);
		page.drawString(label, 300, labelY);
		page.drawString(""+board[myMancalaIndex], mancalaX+STRING_PADDING, BOARDY+BOARD_HEIGHT/2);
		
		// Draw the pits.
		for(int i = 1; i<=NUM_PITS; i++){
			int index = isNorth ? board.length-1-i : i-1;
			page.setColor(PIT_COLOR);
			page.fillOval((int)(BOARDX+OFFSET*i+PADDING),pitY+PADDING,(int)OFFSET-2*PADDING, (int)OFFSET-2*PADDING);
			page.setColor(playerColor);
			page.drawString(""+board[index],(int)(BOARDX+OFFSET*i+STRING_PADDING),pitY+STRING_PADDING);
		}
	}

	// Tell which pit got clicked on.
	public int getPit(Point clicked){
		// if in the pit area in x direction,
		if (clicked.x>=BOARDX+OFFSET && clicked.x<=BOARDX+BOARD_WIDTH-OFFSET){
			// if on top
			if (clicked.y>=BOARDY && clicked.y<=BOARDY+OFFSET){
				return (NUM_PITS*2)-(int)((clicked.x-(BOARDX+OFFSET))/OFFSET);
			}
			else if (clicked.y>=BOARDY+BOARD_HEIGHT-OFFSET && clicked.y<=BOARDY+BOARD_HEIGHT){
				return (int)((clicked.x-(BOARDX+OFFSET))/OFFSET);
			}
		}
		return -1;
	}
}
