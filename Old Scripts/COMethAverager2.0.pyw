# File: hello2.py
import Tkinter, tkFileDialog, tkMessageBox, os, subprocess, shutil
from Tkinter import *
from tkFileDialog import askopenfilename
from tkMessageBox import askyesno
from subprocess import call
from shutil import copyfile
from functools import partial

class App(Tkinter.Tk):
	def __init__(self, parent):
		Tkinter.Tk.__init__(self, parent)
		self.parent = parent
		self.initialize()
	def initialize(self):
		self.grid()
		cmds = list((self.quit, partial(self.SelectFrame, 1), partial(self.SelectFrame, 2), self.Wind, self.COGraphs))
		BNames = list(("QUIT", "CO", "Methane", "Wind", "CO Graphs"))
		FgCol = list(("red", "white", "white", "white", "white"))
		for i in range(0, 5):
			B = Tkinter.Button(self, text = BNames[i], bg = "blue", fg = FgCol[i], command = cmds[i])
			B.grid(column = 0, row = i, sticky = "EW", columnspan = 3)
		self.resizable(True, False)
		self.grid_columnconfigure(0, weight = 1)
	# Methane Operations
	def SelectFrame(self, gas):
		title = "CO Operations"
		process = self.CO
		T = Tkinter.text(self)
		T.grid()
		# Frame1 = Tkinter.Toplevel(self)
		Frame1 = Tkinter.Frame(self)
		Frame1.grid()
		
		if gas == 2:
			# Variables for old files
			self.file1 = Tkinter.StringVar()
			self.file2 = Tkinter.StringVar()
			self.file3 = Tkinter.StringVar()
			self.file4 = Tkinter.StringVar()
			self.file1.set("")
			self.file2.set("")
			self.file3.set("")
			self.file4.set("")
			
			# Labels for old files
			file1 = Tkinter.Entry(Frame1, textvariable = self.file1)
			file1.grid(column = 5, row = 0)
			file2 = Tkinter.Entry(Frame1, textvariable = self.file2)
			file2.grid(column = 5, row = 1)
			file3 = Tkinter.Entry(Frame1, textvariable = self.file3)
			file3.grid(column = 5, row = 2)
			file4 = Tkinter.Entry(Frame1, textvariable = self.file4)
			file4.grid(column = 5, row = 3)
			
			process = self.Methane
			title = "Methane Operations"
		
		# Frame1.title(title)
		i = 0
		
		# Variables for directories
		self.dirname1 = Tkinter.StringVar()
		self.dirname2 = Tkinter.StringVar()
		self.dirname3 = Tkinter.StringVar()
		self.dirname4 = Tkinter.StringVar()
		self.dirname1.set("")
		self.dirname2.set("")
		self.dirname3.set("")
		self.dirname4.set("")
		
		# Site Names
		self.site1 = Tkinter.StringVar()
		self.site2 = Tkinter.StringVar()
		self.site3 = Tkinter.StringVar()
		self.site4 = Tkinter.StringVar()
		self.site1.set("")
		self.site2.set("")
		self.site3.set("")
		self.site4.set("")
		
		# Site Name for selected directory
		site1 = Tkinter.Label(Frame1, textvariable = self.site1)
		site1.grid(column = 0, row = 0)
		site2 = Tkinter.Label(Frame1, textvariable = self.site2)
		site2.grid(column = 0, row = 1)
		site3 = Tkinter.Label(Frame1, textvariable = self.site3)
		site3.grid(column = 0, row = 2)
		site4 = Tkinter.Label(Frame1, textvariable = self.site4)
		site4.grid(column = 0, row = 3)
		
		# Labels for chosen directories
		dir1 = Tkinter.Entry(Frame1, textvariable = self.dirname1)
		dir1.grid(column = 2, row = 0)
		dir2 = Tkinter.Entry(Frame1, textvariable = self.dirname2)
		dir2.grid(column = 2, row = 1)
		dir3 = Tkinter.Entry(Frame1, textvariable = self.dirname3)
		dir3.grid(column = 2, row = 2)
		dir4 = Tkinter.Entry(Frame1, textvariable = self.dirname4)
		dir4.grid(column = 2, row = 3)
		
		# Create Buttons for Directories, oldfiles
		for i in range(0, 4):
			DirCmd = partial(self.DirSelect, i)
			B1 = Tkinter.Button(Frame1, text = "Select directory", bg = "blue", fg = "white", command = DirCmd)
			B1.grid(column = 1, row = i)
			if gas == 2:
				FileCmd = partial(self.FileSelect, i)
				B2 = Tkinter.Button(Frame1, text = "Select old file", bg = "blue", fg = "white", command = FileCmd)
				B2.grid(column = 3, row = i)
			
		BRun = Tkinter.Button(Frame1, text = "Process Data", bg = "blue",
								  fg = "white", command = process)
		BRun.grid(column = 6, row = 0, rowspan = 4, sticky = "NS")
		
	def Methane(self):
		site = list((self.site1.get(), self.site2.get(), self.site3.get(), self.site4.get()))
		addtofile = list()
		dirname = list((self.dirname1.get(), self.dirname2.get(), self.dirname3.get(), self.dirname4.get()))
		filename = list((self.file1.get(), self.file2.get(), self.file3.get(), self.file4.get()))
		for i in range(0, 3):
			dir = dirname[i]
			if dirname[i] != "":
				fil = filename[i]
				sit = site[i]
				stuff = '\nsystem.time(x <- MethaneAvg("%(dir)s", "%(fil)s", "%(sit)s", FALSE, TRUE))\n' %vars()
				addtofile.append(stuff)
			else :
				addtofile.append("")
			
		print "Processing data..."
		
		# We make a copy of the R script so that the function calls dont build up
		temp = copyfile("C:\\Users\\egregory\\Documents\\Methane_CO_GUI\\Methane_For_GUI_2.0.r",
						"C:\\Users\\egregory\\Documents\\Methane_CO_GUI\\temp.r")
		inp = open("C:\\Users\\egregory\\Documents\\Methane_CO_GUI\\temp.r", "a")
		
		# Add function calls to the copy of the R script.
		for i in range(0, 3):
			inp.write(addtofile[i])
		inp.close()
		
		# Run the R script in the shell.
		dostuff = call('cd C:\\Users\\egregory\\Documents\\Methane_CO_GUI&C:\\Users\\egregory\\Documents\\R\\R-2.12.1\\bin\\x64\\Rscript.exe temp.r', shell = True)
	
	def DirSelect(self, boxNum):
		directory = tkFileDialog.askdirectory(parent = self, 
											  initialdir ="/", 
											  title = 'Directory select')
		site = directory[(directory.rfind("/")+1):len(directory)]
		if boxNum == 0:
			self.dirname1.set(directory)
			self.site1.set(site)
		if boxNum == 1:
			self.dirname2.set(directory)
			self.site2.set(site)
		if boxNum == 2:
			self.dirname3.set(directory)
			self.site3.set(site)
		if boxNum == 3:
			self.dirname4.set(directory)
			self.site4.set(site)
	
	def FileSelect(self, boxNum):
		filename = tkFileDialog.askopenfilename(parent = self, 
												initialdir = "/",
												title = "Select an old file for this site")
		if boxNum == 0:
			self.file1.set(filename)
		if boxNum == 1:
			self.file2.set(filename)
		if boxNum == 2:
			self.file3.set(filename)
		if boxNum == 3:
			self.file4.set(filename)

	# CO Operations
	def CO(self):
		more = True
		addtofile = list()
		merge = FALSE
		while more == True:
			dirname = tkFileDialog.askdirectory(parent = self, 
												initialdir = "/", 
												title = 'Directory select')
			site = dirname[(dirname.rfind("/")+1):len(dirname)]
			print '\nData for %(site)s will be processed' %vars()
			# Create the string with the commands to add to the R script
			stuff = '\ndatas <- COImporter("%(dirname)s")\ndatas <- DataFixer(datas, "%(site)s") \nhourly <- COHourlyAverage(datas, "%(site)s")\ncat("\nFinished averaging CO data for", "%(site)s \n", sep = " ")\n' %vars()
			addtofile.append(stuff)
			more = tkMessageBox.askyesno(parent = self, 
										title ='More CO?', 
										message = 'Would you like to set up another CO operation?')
		
		
		temp = copyfile("C:\\Users\\egregory\\Documents\\Methane_CO_GUI\\C0_For_GUI.r", 
						"C:\\Users\\egregory\\Documents\\Methane_CO_GUI\\temp.r")
		inp = open("C:\\Users\\egregory\\Documents\\Methane_CO_GUI\\temp.r", "a")
		for i in range(0, len(addtofile)):
			inp.write(addtofile[i])
		inp.close()
			# Run the new Script!
		dostuff = call('cd C:\\Users\\egregory\\Documents\\Methane_CO_GUI&C:\\Users\\egregory\\Documents\\R\\R-2.12.1\\bin\\x64\\Rscript.exe temp.r', shell = True)
			# Profit.
	def Wind(self):
		print "Wind is cool!"
	def COGraphs(self):
		# more stays true until the user is done setting up operations.
		more = True
		addtofile = list()
		dirname = tkFileDialog.askdirectory(parent = self, initialdir = "/", 
											title = 'Directory to write output to?')
		addtofile.append('\nsetwd("%(dirname)s")\n' %vars())
		while more == True:
			filename = askopenfilename(parent = self, initialdir = "/", 
									   title = 'File with compiled CO?')
			site = filename[filename.rfind("/") + 1 : filename.rfind("CO")]
			print '\nData for %(site)s will be processed' %vars()
			stuff = '\ndatas <- RFormat(read.csv("%(filename)s")) \nPrecisionGraphs(datas, "%(site)s")\n' %vars()
			addtofile.append(stuff)
			more = tkMessageBox.askyesno(parent = self, 
										 title ='More CO Precision?', 
										 message = 'Would you like to set up another CO Precision operation?')
		
		temp = copyfile("C:\\Users\\egregory\\Documents\\Methane_CO_GUI\\CO_Precision_Graphs.r",
						"C:\\Users\\egregory\\Documents\\Methane_CO_GUI\\temp.r")
		inp = open("C:\\Users\\egregory\\Documents\\Methane_CO_GUI\\temp.r", "a")
		for i in range(0, len(addtofile)):
			inp.write(addtofile[i])
		inp.close()
		print'\nCreating graphs...'
		dostuff = call('cd C:\\Users\\egregory\\Documents\\Methane_CO_GUI&C:\\Users\\egregory\\Documents\\R\\R-2.12.1\\bin\\x64\\Rscript.exe temp.r', shell = True)
		print'\nDone!'

if __name__ == "__main__":
	app = App(None)
	app.title("Methane/CO Averager")
	app.mainloop()
