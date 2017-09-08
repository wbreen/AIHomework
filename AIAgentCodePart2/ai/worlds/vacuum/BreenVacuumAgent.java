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
		if(height%2 == 0) {
			return evenHeight();
		}
		if(bumped()) {
			return this.LEFT;
		}
		return this.OFF;
	}

	//method that makes the bot go all the way to the right of the grid
	public int getWidth() {
		if(seesDirt()) {
			return this.SUCK;
		}
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
		if(seesDirt()) {
			return this.SUCK;
		}
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
		System.out.println("current X " + currentXLoc);
		if(seesDirt()) {
			return this.SUCK;
		}
		if(currentXLoc == 0 && currentYLoc !=0 && !goingDown && goingLeft) {
			goingDown = true;
			return this.LEFT;
		}
		if(currentXLoc == 0 && currentYLoc !=0 && goingDown && goingLeft) {
			currentYLoc = currentYLoc-1;
			goingLeft = false;
			return this.FORWARD;
		}
		if(currentXLoc == 0 && goingDown && !goingLeft) {
			goingDown = false;
			return this.LEFT;
		}
		if(goingLeft) {
			currentXLoc--;
			return this.FORWARD;
		}
		if(!goingLeft) {
			currentXLoc++;
			return this.FORWARD;
		}
		return this.FORWARD;
	}
	
	
	//Method that will clean the room if the height of the room is odd
	public int oddHeight() {
		if(seesDirt()) {
			return this.SUCK;
		}
		return this.FORWARD;
	}
	
	
	//Random methods to use throughout
	
}















