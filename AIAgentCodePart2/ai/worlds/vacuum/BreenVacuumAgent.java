package ai.worlds.vacuum;

public class BreenVacuumAgent extends VacuumAgent{

	//getting width and height of 'world'
	boolean haveWidth;
	boolean haveHeight;
//	public boolean hitRightWall;
//	public boolean hitTopWall;
	int width;
	int height;
	boolean evenHeight;
	
	//Information about current location and direction
	boolean goingLeft;
	boolean goingDown;
	int currentXLoc;
	int currentYLoc;
	boolean haveSetCurrentLoc;
	
	
	public int getAction() {
//		System.out.println("current X " + currentXLoc);
//		System.out.println("current Y " + currentYLoc);
		if(seesDirt()) {
			return this.SUCK;
		}
		while(!haveWidth) {
			//System.out.println("current width " + width);
			return getWidth();
		}
		while(!haveHeight) {
			//System.out.println("Current height "+height);
			return getHeight();
		}
		if(!haveSetCurrentLoc) {
			currentXLoc = width;
			currentYLoc = height;
			haveSetCurrentLoc = true;
			goingLeft = true;
		}
		if(isHome()) {
			return this.OFF;
		}
		if(width > 2) {
			return evenHeight();
		}
		if(width <= 2) {
			return narrowGrid();
		}
		if(bumped()) {
			return this.LEFT;
		}
		return this.OFF;
	}

	//method that makes the bot go all the way to the right of the grid
	public int getWidth() {
		if(bumped()) {
			haveWidth = true;
			return this.LEFT;
		}
		else { 
			width++;
			return this.FORWARD;
		}
	}
	
	//method that makes the bot go to the top of the grid
	public int getHeight() {
		if(bumped()) {
			haveHeight = true;
			return this.LEFT;
		}
		else {
			height++;
			return this.FORWARD;
		}
	}
	
	
	//Method that will clean the room if the height of the room is even
	public int evenHeight() {
		
		//handles the left turn on the grid
		if(currentXLoc == 1 && currentYLoc !=0 && !goingDown && goingLeft) {
			goingDown = true;
			return this.LEFT;
		}
		if(currentXLoc == 1 && currentYLoc !=0 && goingDown && goingLeft) {
			currentYLoc = currentYLoc-1;
			goingLeft = false;
			return this.FORWARD;
		}
		if(currentXLoc == 1 && goingDown && !goingLeft) {
			goingDown = false;
			return this.LEFT;
		}
		//handles the right side turn on the grid
		if(currentXLoc == width-1 && !goingDown && !goingLeft) {
			goingDown = true;
			return this.RIGHT;
		}
		if(currentXLoc == width-1 && goingDown && !goingLeft) {
			currentYLoc = currentYLoc-1;
			goingLeft = true;
			return this.FORWARD;
		}
		if(currentXLoc == width-1 && goingDown && goingLeft) {
			goingDown = false;
			return this.RIGHT;
		}
		
		//tells you what direction the bot is going in
		if(goingLeft) {
			currentXLoc--;
			return this.FORWARD;
		}
		if(!goingLeft) {
			currentXLoc++;
			return this.FORWARD;
		}
		return this.OFF;
	}
	
	
	//Method that will clean the room if the height of the room is odd
	public int narrowGrid() {
		if(bumped()) {
			goingLeft = false;
			return this.LEFT;
		}
		return this.FORWARD;
	}
	
	
	//Random methods to use throughout
	
}















