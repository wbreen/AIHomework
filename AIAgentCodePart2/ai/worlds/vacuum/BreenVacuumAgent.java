package ai.worlds.vacuum;

public class BreenVacuumAgent extends VacuumAgent{

	public boolean haveWidth;
	public boolean haveHeight;
	public boolean hitRightWall;
	public boolean hitTopWall;
	
	int width;
	int height;
	
	public int getAction() {
		while(!hitRightWall) {
			//System.out.println("current width " + width);
			return getWidth();
		}
		while(!hitTopWall) {
			//System.out.println("Current height "+height);
			return getHeight();
		}
		if(bumped()) {
			return this.LEFT;
		}
		return this.OFF;
	}

	public int getWidth() {
		if(seesDirt()) {
			return this.SUCK;
		}
		if(bumped()) {
			hitRightWall = true;
			return this.LEFT;
		}
		else { 
			width++;
			return this.FORWARD;
		}
	}
	public int getHeight() {
		if(seesDirt()) {
			return this.SUCK;
		}
		if(bumped()) {
			hitTopWall = true;
			return this.LEFT;
		}
		else {
			height++;
			return this.FORWARD;
		}
	}
}
