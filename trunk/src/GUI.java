
import javax.swing.*;
import java.awt.*;
import java.io.*;
import java.util.*;
import java.awt.Rectangle;

import javax.swing.JFileChooser;
import javax.swing.JTextArea;
import javax.swing.JScrollBar;
import java.awt.Dimension;
import java.awt.Button;
import java.awt.TextArea;


public class GUI extends JPanel {

	private Button check = null;
	private Button exit = null;
	private TextArea textArea = null;
	private Button open = null;
	private Button save = null;

	/**
	 * This method initializes 
	 * 
	 */
	public GUI() {
		super();
		initialize();
	}

	/**
	 * This method initializes this
	 * 
	 */
	private void initialize() {
        this.setLayout(null);
        this.setSize(new Dimension(781, 427));
        this.add(getButtonCheck(), null);
        this.add(getButtonExit(), null);
        this.add(getTextArea(), null);
        this.add(getButtonOpen(), null);
        this.add(getButtonSave(), null);
        this.setVisible(true);			
	}

	/**
	 * This method initializes button	
	 * 	
	 * @return java.awt.Button	
	 */
	private Button getButtonCheck() {
		if (check == null) {
			check = new Button();
			check.setBounds(new Rectangle(45, 383, 125, 30));
			check.setLabel("Check");
		}
		return check;
	}

	/**
	 * This method initializes button	
	 * 	
	 * @return java.awt.Button	
	 */
	private Button getButtonExit() {
		if (exit == null) {
			exit = new Button();
			exit.setBounds(new Rectangle(620, 379, 146, 38));
			exit.setLabel("Exit");
			exit.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
					System.exit(0); // TODO Auto-generated Event stub actionPerformed()
				}
			});
		}
		return exit;
	}

	/**
	 * This method initializes textArea	
	 * 	
	 * @return java.awt.TextArea	
	 */
	private TextArea getTextArea() {
		if (textArea == null) {
			textArea = new TextArea();
			textArea.setBounds(new Rectangle(16, 42, 750, 308));
			textArea.setText("%Try this example or insert your own text.");
		}
		return textArea;
	}

	/**
	 * This method initializes button	
	 * 	
	 * @return java.awt.Button	
	 */
	private Button getButtonOpen() {
		if (open == null) {
			open = new Button();
			open.setBounds(new Rectangle(32, 7, 63, 22));
			open.setLabel("Open");
			open.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
					load();
				}
			});
		}
		return open;
	}

	/**
	 * This method initializes button	
	 * 	
	 * @return java.awt.Button	
	 */
	private Button getButtonSave() {
		if (save == null) {
			save = new Button();
			save.setBounds(new Rectangle(131, 7, 60, 22));
			save.setLabel("Save");
			save.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
					save();
				}
			});
		}
		return save;
	}
	
	protected void load() {
	    final JFileChooser fc = new JFileChooser();
	    //int returnVal = fc.showOpenDialog(jFrame);
	    int returnVal = fc.showOpenDialog(this.getComponent(0));
	 
	      if (returnVal == JFileChooser.APPROVE_OPTION) {
	          File file = fc.getSelectedFile();
	          showText(file);
	      }
	}
	 
	private void showText(File file){
	    StringBuffer buf = new StringBuffer();
	    if(file.exists( )){
	        try {
	            BufferedReader reader = new BufferedReader(new FileReader(file));
	            String line = "";
	            while((line = reader.readLine( )) != null){
	                buf.append(line+"\n");
	            }
	            reader.close( );
	        }
	        catch (FileNotFoundException e) {
	            e.printStackTrace( );
	        }
	        catch (IOException e) {
	            e.printStackTrace( );
	        }
	    }
	   this.textArea.setText(buf.toString( ));
	}

	protected void save() {
	    final JFileChooser fc = new JFileChooser();
	    int returnVal = fc.showSaveDialog(this.getComponent(0));
	 
	      if (returnVal == JFileChooser.APPROVE_OPTION) {
	          File file = fc.getSelectedFile();
	          saveText(file);
	      }
	}
	 
	private void saveText(File file) {
	    try {
	        FileWriter writer = new FileWriter(file);
	        String text = this.textArea.getText( );
	        writer.write(text);
	        writer.flush();
	        writer.close();
	    }
	    catch (IOException e) {
	        e.printStackTrace( );
	    }
	}
	
	public int firstPosModified(String aa, String bb){
		if (aa.length() < 1 || bb.length() < 1){
			return -2;
		}

		String a = new String();
		String b = new String();
		if (aa.length() > bb.length()){
			a = aa;
			b = bb;
		}
		else {
			a = bb;
			b = aa;
		}
		int i = 0;
		for (i = 0; i < b.length(); i++){ 
			if (a.charAt(i) != b.charAt(i)){
				return i;
			}
			
		}
		if (i < a.length()){
			return i;
		}
		else {
			return -1;
		}
	}
	
	
/*	
	public static void main(String args[]){
		GUI x = new GUI();
	}
*/
}  //  @jve:decl-index=0:visual-constraint="26,33"