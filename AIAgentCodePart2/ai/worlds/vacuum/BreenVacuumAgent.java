package ai.worlds.vacuum;

public class BreenVacuumAgent extends VacuumAgent{

	public boolean haveWidth;
	public boolean haveHeight;
	public boolean hitRightWall;
	public boolean hitTopWall;
	
	public int width;
	public int height;
	
	public int getAction() {
		while(!hitRightWall) {
			getWidth();
		}
		if(bumped()) {
			return this.LEFT;
		}
		return this.FORWARD;
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
}
