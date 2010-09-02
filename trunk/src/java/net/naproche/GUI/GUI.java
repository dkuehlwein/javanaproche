package net.naproche.GUI;

import javax.swing.*;
import java.awt.*;
import java.io.*;
import java.util.*;
import java.awt.Rectangle;

import javax.swing.JFileChooser;
import javax.swing.JTextArea;
import javax.swing.JScrollBar;

import net.naproche.preparser.*;
import net.naproche.preparser.Error;

import java.awt.Dimension;
import java.awt.Button;
import java.awt.TextArea;

import jpl.*;
import java.awt.Label;
import java.awt.TextField;
import java.lang.Integer;

public class GUI extends JPanel {

	private Button check = null;
	private Button exit = null;
	private TextArea textArea = null;
	private Button open = null;
	private Button save = null;
	private TextArea info = null;
	private TextField txtstart = null;
	private TextField txtend = null;
	private Button btnmark = null;
	
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
        this.setVisible(true);
        this.setSize(new Dimension(800, 600));
        this.add(getButtonCheck(), null);
        this.add(getButtonExit(), null);
        this.add(getTextArea(), null);
        this.add(getButtonOpen(), null);
        this.add(getButtonSave(), null);
        this.add(getInfo(), null);
        this.add(getTxtstart(), null);
        this.add(getTxtend(), null);
        this.add(getBtnmark(), null);
	}

	/**
	 * This method initializes button	
	 * 	
	 * @return java.awt.Button	
	 */
	private Button getButtonCheck() {
		if (check == null) {
			check = new Button();
			check.setBounds(new Rectangle(20, 550, 125, 30));
			check.setLabel("Check");
			check.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
					String tmp = textArea.getText();
					tmp = tmp.replaceAll("\\n", "#");// TBC???
					//System.out.println("TMP:" + tmp);
					tmp = tmp.replaceAll("\\\\", "!");
					//System.out.println("TMP:" + tmp);
					tmp = tmp.replaceAll("!", "\\\\\\\\");
					
					info.setVisible(true);
					//info.setText("OK!");
					try {
						preparse(tmp);
					}
					catch(Exception ex){
						info.setText("Es ist etwas schiefgegangen:\n" + ex.toString());
					}
					//info.setText(tmp);
				}
			});
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
			exit.setBounds(new Rectangle(650, 550, 125, 30));
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
			textArea.setBounds(new Rectangle(20, 50, 750, 300));
			//textArea.setText("Try this example or insert your own text. $\\frac{1}{2} = a$.");
			textArea.setText("$\\frac{1}{2} = a$.");
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
			open.setBounds(new Rectangle(20, 5, 60, 30));
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
			save.setBounds(new Rectangle(100, 5, 60, 30));
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
	
	public void preparse(String input){
		new Query("['src/prolog/load_jpl'].").oneSolution();
		Query create_naproche_input =
			// new Query("create_naproche_input('Let $n$ be in $\\\\mathbb{N}$. Then $n > 0$.',L).");
			// new Query("create_naproche_input('\\\\begin{Proof} trivial. \\\\end{Proof}',L).");
			new Query("create_naproche_input('" + input + 
					"',L).");

		String output;
		try{
			output = create_naproche_input.oneSolution().get("L").toString();
			LinkedList<Sentence> ll = Sentence.convertSentences(output);
			System.out.println("OK: " + ll);
			info.setText(ll.toString());
			info.setVisible(true);
		}
		catch (java.lang.NullPointerException ex){
			Query get_messages = new Query("get_messages(Messages)");
			output = get_messages.oneSolution().get("Messages").toString();
			LinkedList<Error> ll = Error.convertErrors(output);
			System.out.println("Nicht OK: " + ll);
			info.setText(ll.toString());
			info.setVisible(true);
		}
	}

	/**
	 * This method initializes info	
	 * 	
	 * @return java.awt.TextArea	
	 */
	private TextArea getInfo() {
		if (info == null) {
			info = new TextArea();
			info.setBounds(new Rectangle(20, 360, 750, 180));
			info.setVisible(false);
			info.setEditable(false);
		}
		return info;
	}

	/**
	 * This method initializes txtstart	
	 * 	
	 * @return java.awt.TextField	
	 */
	private TextField getTxtstart() {
		if (txtstart == null) {
			txtstart = new TextField();
			txtstart.setBounds(new Rectangle(400, 10, 50, 20));
			txtstart.setText("0");
		}
		return txtstart;
	}

	/**
	 * This method initializes txtend	
	 * 	
	 * @return java.awt.TextField	
	 */
	private TextField getTxtend() {
		if (txtend == null) {
			txtend = new TextField();
			txtend.setBounds(new Rectangle(480, 10, 50, 20));
			txtend.setText("1");
		}
		return txtend;
	}

	/**
	 * This method initializes btnmark	
	 * 	
	 * @return java.awt.Button	
	 */
	private Button getBtnmark() {
		if (btnmark == null) {
			btnmark = new Button();
			btnmark.setBounds(new Rectangle(550, 10, 100, 20));
			btnmark.setLabel("Markierung");
			btnmark.addActionListener(new java.awt.event.ActionListener() {
				public void actionPerformed(java.awt.event.ActionEvent e) {
					mark();
					
				}
			});
		}
		return btnmark;
	}
	
	public void mark(){
		int start, end;
		start = Integer.valueOf(txtstart.getText()).intValue();
		end = Integer.valueOf(txtend.getText()).intValue();
		
		info.setVisible(true);
		info.setText("Markierung sollte da sein");
		textArea.select(start, end);
		textArea.setFocusable(true);
		
	}
	
	/*	
	public static void main(String args[]){
		GUI x = new GUI();
	}
	*/
	
}  //  @jve:decl-index=0:visual-constraint="57,35"