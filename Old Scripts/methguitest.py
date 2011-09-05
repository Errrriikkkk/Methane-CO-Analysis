# File: hello2.py
import Tkinter, tkFileDialog, tkMessageBox, os, subprocess, shutil
from Tkinter import *
from tkFileDialog import askopenfilename
from tkMessageBox import askyesno
from subprocess import call
from shutil import copyfile

def callback():	
	print "called the callback!"

top = Tk()

class App:
	def __init__(self, parent):
		# GUI Structure
		mainWindow = Frame(parent, bg = "blue", height = 50)
		mainWindow.master.title("CO and Methane Analysis")
		mainWindow.pack()
		button1 = Button(mainWindow, text = "QUIT", fg = "red", command = mainWindow.quit)
		button1.pack(side=RIGHT)
		self.button2 = Button(mainWindow, text = "CO", command = self.CO)
		self.button2.pack(side = LEFT)
		self.button3 = Button(mainWindow, text = "METHANE", command = self.Methane)
		self.button3.pack(side=LEFT)
		self.button4 = Button(mainWindow, text = "WIND", fg = "blue", command = self.Wind)
		self.button4.pack(side = LEFT)
		self.button5 = Button(mainWindow, text = "CO Graphs", fg = "yellow", command = self.COGraphs)
		self.button5.pack(side = LEFT)
	# Methane Operations
	def Methane(self):
		# more stays true until the user is done setting up operations.
		more = True
		addtofile = list()
		while more == True:
			dirname = tkFileDialog.askdirectory(parent = top, 
												initialdir ="/", 
												title = 'Directory select')
			
			# Optional choice of the filename of an old hourly average file.
			filename = askopenfilename(parent = top, title = 'Old File?')
			site = dirname[(dirname.rfind("/")+1):len(dirname)]
			# Function calls to be added to the R scripts.
			stuff = '\nAllAnalysis("%(dirname)s", "%(filename)s", "%(site)s", FALSE)\n' %vars()
			# This simply determines whether the user has chosen a directory or not.
			if(len(filename)>=2):
				merge = tkMessageBox.askyesno(parent = top, 
											  title = 'Merge?', 
											  message = 'Would you like to merge the old hourly average methane file with the new data?')
				if(merge == TRUE): 
					stuff = '\nAllAnalysis("%(dirname)s", "%(filename)s", "%(site)s", TRUE)\n' %vars()
			addtofile.append(stuff)
			more = tkMessageBox.askyesno(parent = top,
										 title = 'More Methane?',
									     message = 'Would you like to set up another Methane operation?')
		
		print "Processing data..."
		# We make a copy of the R script so that the function calls dont build up
		temp = copyfile("C:\\My Documents\\Methane_CO_GUI\\Methane_For_GUI.r",
						"C:\\My Documents\\Methane_CO_GUI\\temp.r")
		inp = open("C:\\My Documents\\Methane_CO_GUI\\temp.r", "a")
		# Add function calls to the copy of the R script.
		for i in range(0, len(addtofile)):
			inp.write(addtofile[i])
		inp.close()
		print "%(filename)s" %vars()
		# Run the R script in the shell.
		dostuff = call('cd C:\My Documents\Methane_CO_GUI&Rscript temp.r', shell = True)
		# Profit.
	
	# CO Operations
	def CO(self):
		more = True
		addtofile = list()
		merge = FALSE
		while more == True:
			dirname = tkFileDialog.askdirectory(parent = top, 
												initialdir = "/", 
												title = 'Directory select')
			site = dirname[(dirname.rfind("/")+1):len(dirname)]
			# Create the string with the commands to add to the R script
			stuff = '\ndatas <- COImporter("%(dirname)s")\ndatas <- DataFixer(datas, "%(site)s") \nhourly <- COHourlyAverage(datas, "%(site)s")\ncat("\nFinished averaging CO data for", "%(site)s \n", sep = " ")\n' %vars()
			addtofile.append(stuff)
			more = tkMessageBox.askyesno(parent = top, 
										title ='More CO?', 
										message = 'Would you like to set up another CO operation?')
		
		
		temp = copyfile("C:\\My Documents\\Methane_CO_GUI\\C0_For_GUI.r", 
						"C:\\My Documents\\Methane_CO_GUI\\temp.r")
		inp = open("C:\\My Documents\\Methane_CO_GUI\\temp.r", "a")
		for i in range(0, len(addtofile)):
			inp.write(addtofile[i])
		inp.close()
			# Run the new Script!
		dostuff = call('cd C:\My Documents\Methane_CO_GUI&Rscript temp.r', shell = True)
			# Profit.
	def Wind(self):
		print "Wind is cool!"
	def COGraphs(self):
		# more stays true until the user is done setting up operations.
		more = True
		addtofile = list()
		dirname = tkFileDialog.askdirectory(parent = top, initialdir = "/", 
											title = 'Directory to write output to?')
		addtofile.append('\nsetwd("%(dirname)s")\n' %vars())
		while more == True:
			filename = askopenfilename(parent = top, initialdir = "/", 
									   title = 'File with compiled CO?')
			site = filename[filename.rfind("/") + 1 : filename.rfind("CO")]
			print '\nData for %(site)s will be processed' %vars()
			stuff = '\ndatas <- RFormat(read.csv("%(filename)s")) \nPrecisionGraphs(datas, "%(site)s")\n' %vars()
			addtofile.append(stuff)
			more = tkMessageBox.askyesno(parent = top, 
										 title ='More CO Precision?', 
										 message = 'Would you like to set up another CO Precision operation?')
		
		temp = copyfile("C:\\My Documents\\Methane_CO_GUI\\CO_Precision_Graphs.r",
						"C:\\My Documents\\Methane_CO_GUI\\temp.r")
		inp = open("C:\\My Documents\\Methane_CO_GUI\\temp.r", "a")
		for i in range(0, len(addtofile)):
			inp.write(addtofile[i])
		inp.close()
		print'\nCreating graphs...'
		call('cd C:\My Documents\Methane_CO_GUI&Rscript temp.r', shell = True)
		print'\nDone!'
		
app = App(top)
top.mainloop()
