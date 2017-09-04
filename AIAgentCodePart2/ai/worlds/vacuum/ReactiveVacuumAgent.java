package ai.worlds.vacuum;

import java.util.Vector;
/**
 * A vacuum agent which reacts to its percepts.
 * @author Jill Zimmerman -- jill.zimmerman@goucher.edu
 *
 */
public class ReactiveVacuumAgent extends VacuumAgent
{
	/**
	 * Determine the next action to be performed.
	 */
    public int getAction()
    {
	if (seesDirt()) return this.SUCK;
	else if (isHome()) {
	    int i = (int)Math.floor(Math.random()*3);
	    switch (i) {
		case 0:return this.OFF; 
		case 1: return this.FORWARD; 
		case 2: return this.LEFT;
	    }
	}
	else if (bumped()) {
	    int i = (int)Math.floor(Math.random()*2);
	    switch (i) {
		case 0: return this.RIGHT; 
		case 1: return this.LEFT;
	    }	
	}
	else {
	    int i = (int)Math.floor(Math.random()*5);
	    switch (i) {
		case 0: return this.FORWARD;
		case 1: return this.FORWARD;
		case 2: return this.FORWARD;
		case 3: return this.RIGHT;
		case 4: return this.LEFT;
	    }
	}
	return this.FORWARD;
    }
}