import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.plaf.metal.MetalLookAndFeel;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyledDocument;
import java.util.HashMap;

import ai.worlds.WorldCreatePanel;
import ai.search.SearchPanel;
import ai.logic.LogicPanel;
import ai.worlds.vacuum.*;
/**
 * The main class for the CS340 AI software package
 * @author Jill Zimmerman -- jill.zimmerman@goucher.edu
 *
 */


public class AI
{
	public static void main(String[] args)
	{
		HashMap<String, VacuumAgent> agentList = new HashMap<String, VacuumAgent>();
		agentList.put("Random Vacuum Agent", new RandomVacuumAgent());
		agentList.put("Reactive Vacuum Agent", new ReactiveVacuumAgent());
		agentList.put("Breen's Agent", new BreenVacuumAgent());
		/*agentList.put("Shannons Vacuum Agent", new ShannonsVacuum());
		agentList.put("Brian", new BriansModelVacuumAgent());
		agentList.put("Chris", new Christobot());
		agentList.put("Dan", new CresseAgent());
		agentList.put("Keith", new IRobotRoomba());
		agentList.put("Jamie", new JamiesModelBasedAgent());
		agentList.put("Nestor", new Nesbot());
		agentList.put("Steven", new NorrisModelAgent());
		agentList.put("Tess", new TessModelAgent());
		//agentList.put("Yakira", new YakirasDirtSuckerAgent());
		agentList.put("Brandon", new BRWModelBasedAgent());
		agentList.put("Shannons Harvester", new ShannonsHarvester());*/
		try {
			UIManager.setLookAndFeel("MetalLookAndFeel");
		} catch (Exception e) {}

		JFrame f = new JFrame("Artificial Intelligence");
		//f.setSize(1024, 750);
		f.setSize(1054, 750);
		f.setResizable(false);
		f.addWindowListener(new WindowAdapter() {
			public void windowClosing(WindowEvent e) {
				System.exit(0);
			}
		});
		f.getContentPane().setLayout(new BorderLayout());
		f.getContentPane().add(new AIPanel(f, agentList) , BorderLayout.NORTH);
		f.setVisible(true); 
	}
}

class AIPanel extends JPanel {

	public static final Color buttonColor = MetalLookAndFeel.getTextHighlightColor();
	private JFrame holder;
	public JTabbedPane centerPanel = new JTabbedPane();
	private WorldCreatePanel wcp;
	private SearchPanel sp;
	private LogicPanel lp;
	public AIPanel(JFrame h, HashMap<String, VacuumAgent> agents) {
		holder = h;
		wcp = new WorldCreatePanel(holder, agents);
		sp = new SearchPanel(holder);
		lp = new LogicPanel();
		JPanel aboutPanel = createAboutPanel();
		centerPanel.addTab("Agents and Environments", wcp);
		centerPanel.addTab("Searches", sp);
		centerPanel.addTab("Logic", lp);
		centerPanel.addTab("About", aboutPanel);
		setBorder(BorderFactory.createCompoundBorder(BorderFactory.createRaisedBevelBorder(), BorderFactory.createLoweredBevelBorder()));
		holder.getContentPane().add(centerPanel, BorderLayout.CENTER);
	}
	private JPanel createAboutPanel() {
		JPanel aboutPanel = new JPanel(new BorderLayout());
		JTextPane aboutText = new JTextPane();
		SimpleAttributeSet set = new SimpleAttributeSet();
		StyleConstants.setAlignment(set,StyleConstants.ALIGN_CENTER);
		aboutText.setParagraphAttributes(set,true);
		aboutText.setText("Artificial Intelligence Software Package\nfor CS 340 at Goucher College\n\nWritten by Jill Zimmerman in 2000\nRefactored by Jim Segedy (jim.segedy@gmail.com) in 2006");
		aboutText.setBackground(MetalLookAndFeel.getTextHighlightColor());
		aboutText.setEditable(false);
		aboutPanel.setBorder(BorderFactory.createCompoundBorder(BorderFactory.createRaisedBevelBorder(), BorderFactory.createLoweredBevelBorder()));
		aboutPanel.add(aboutText, BorderLayout.CENTER);
		return aboutPanel;
	}
}


