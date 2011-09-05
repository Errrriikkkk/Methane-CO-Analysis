# File: hello2.py

# TODO: add function to run trajectory models in bulk
# User Fields:
# 1. dataset to get times from
# 2. Site name, From a set list of possibilities.  Could select multiple?
# 3. Hours backward which we want to run the model
# 4. start.mo, start.year, end.mo, end.year (selected from dataset, if input)
# 5. height (default 10)
# 6. runsplit, default FALSE
# 7. overwrite, default FALSE
# 8. vars, default NULL
# 9. output.dir, default NULL
# 10. PST.to.UTC, default FALSE

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
		self.open = Tkinter.IntVar()
		self.open.set(0)
		self.grid()
		cmds = list((self.quit, partial(self.CreateFrame, 1), partial(self.CreateFrame, 2), self.Wind, self.COGraphs, partial(self.CreateFrame, 3)))
		BNames = list(("QUIT", "CO", "Methane", "Wind", "CO Graphs", "Maps"))
		FgCol = list(("red", "white", "white", "white", "white", "white"))
		for i in range(0, 6):
			B = Tkinter.Button(self, text = BNames[i], bg = "blue", fg = FgCol[i], command = cmds[i])
			B.grid(column = 0, row = i, sticky = "EW", columnspan = 3)
		self.resizable(True, False)
		self.grid_columnconfigure(0, weight = 1)
	
	def TrajMaps(self):
		title = "Trajectory Maps"
		if self.open.get() == 1:
			self.Frame1.destroy()
			self.status.destroy()
		self.open.set(1)
		
		# Fields:
		# 1. datas: dataframe with trajectories, and a gas measurement or two (file)
		# 2. bin.length: how many degrees should the grid be divided into? (double), 0.01 increments?
		# 3. Month: do we want to subset the data by a particular month? (
		# 4. map.type: if using "gg = TRUE", do we want to try "state" or "county?"
		# 5. plotit: should the function actually output the map, or just process the data?
		# 6. zeros: should data cells with "0" for the number of high measurements be included?
		# 7. site: Which sites should we include dots for?
		# 8. too.few: How many measurements in the "Total" column is too few to be plotted?
		# 9. gg : ggplot map?
		# 10. g.mpas: google map?
		# 11. start.time/end.time: start/end hour? on a 24 hour scale
		# 12. gmap.type: There are a few possible types of gmaps, default "mobile"
		# 13. colors: name of color palette for RColorBrewer, default "Reds"
		# 14. points: Should points be plotted individually, or should they be binned? default FALSE
		
	def CreateFrame(self, gas):
		# Close any old windows 
		if self.open.get() == 1:
			self.Frame1.destroy()
			self.Frame2.destroy()
			self.Frame3.destroy()
			#self.status.destroy()
		self.open.set(1)
		self.title = Tkinter.StringVar()
		self.FrameTitle = Tkinter.Label(self, textvariable = self.title, bg = "black",
										fg = "white")
		self.FrameTitle.grid(column = 0, row = 6,  sticky = "EW")	
		self.Frame1 = Tkinter.Frame(self)
		self.Frame1.grid(column = 0, row = 7)
		# self.status = Tkinter.Text(self)
		# self.status.grid(column = 0, row = 8)
		
		# Specific parameters for when the gas is "CO"
		if gas == 1:
			process = self.CO
			title = "CO Operations"
		
		# If the operation selected is "Methane", we create possible
		# oldfile inputs.
		if gas == 2:
			process = self.Methane
			title = "Methane Operations"
		
		# Specific operations when dealing with Mapping operations
		if gas == 3:
			# Declare textvariables, other parameters
			self.Map = Tkinter.IntVar()
			self.Length = Tkinter.DoubleVar()
			self.TooFew = Tkinter.IntVar()
			self.Back = Tkinter.IntVar()
			
			mos = ["NULL", 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
			hours = ["NULL", 0,  1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23]
			self.Length.set(0.05)
			process = self.Maps
			title = "Map Operations"
			
			self.BinLabel = Tkinter.Label(self.Frame1, text = "Bin Length (degrees)", bg = "green", fg = "black")
			self.BinLabel.grid(column = 3, row = 0, sticky = "EW")
			self.BinLength = Tkinter.Entry(self.Frame1, textvariable = self.Length)
			self.BinLength.grid(column = 3, row = 1)
			
			self.TooFewLabel = Tkinter.Label(self.Frame1, text = "Too Few", bg = "green", fg = "black")
			self.TooFewLabel.grid(column = 3, row = 2, sticky = "EW")
			self.TooFewE = Tkinter.Entry(self.Frame1, textvariable = self.TooFew)
			self.TooFewE.grid(column = 3, row = 3)
			
			
			# Third Frame for Map Specifics
			self.Frame3 = Tkinter.Frame(self)
			self.Frame3.grid(column = 0, row = 9)
			
			# Gmaps Info
			gmaptypes = ("roadmap", "mobile", "satellite", "terrain", "hybrid", "mapmaker-road")
			self.GmapTypeList = Listbox(self.Frame3, exportselection = FALSE)
			for item in gmaptypes:
				self.GmapTypeList.insert(END, item)
			self.GmapTypeList.grid(column = 5, row = 2)
			
			ggtypes = ("state", "county")
			self.GGTypeList = Listbox(self.Frame3, exportselection = FALSE)
			for item in ggtypes:
				self.GGTypeList.insert(END, item)
			self.GGTypeList.grid(column = 6, row = 2)

			
			self.Map.set(0)
			self.GGLabel = Tkinter.Label(self.Frame3, text = "GGPlot2 Options", bg = "green", fg = "black")
			self.GGLabel.grid(column = 6, row = 0, sticky = "EW")
			self.GGButton = Tkinter.Radiobutton(self.Frame3, text = "Use GGPlot Maps", variable = self.Map, value = 1)
			self.GGButton.grid(column = 6, row = 1, sticky = "EW")
			
			self.GmapsLabel = Tkinter.Label(self.Frame3, text = "Google Maps Options", bg = "green", fg = "black")
			self.GmapsLabel.grid(column = 5, row = 0, sticky = "EW")
			self.GmapsButton = Tkinter.Radiobutton(self.Frame3, text = "Use Google Maps", variable = self.Map, value = 0)
			self.GmapsButton.grid(column = 5, row = 1, sticky = "EW")
			
			
			
			# Create the listbox for month selection
			self.Frame2 = Tkinter.Frame(self)
			self.Frame2.grid(column = 0, row = 8)

			StartMonthScroll = Scrollbar(self.Frame2, orient = VERTICAL)
			EndMonthScroll = Scrollbar(self.Frame2, orient = VERTICAL)
			StartDayScroll = Scrollbar(self.Frame2, orient = VERTICAL)
			EndDayScroll = Scrollbar(self.Frame2, orient = VERTICAL)
			# Label for Subsetting by Time Period
			self.TimeSubsetLabel = Tkinter.Label(self.Frame2, text = "Subset by Dates", bg = "green", fg = "black")
			self.TimeSubsetLabel.grid(column = 0, row = 0, sticky = "EW", columnspan = 11)
			# Start Year
			self.StartYearLabel = Tkinter.Label(self.Frame2, text = "Start Year", bg = "black", fg = "white")
			self.StartYearLabel.grid(column = 1, row = 1, sticky = "EW")
	
			self.StartYearList = Listbox(self.Frame2, exportselection = FALSE)
			for item in range(2010, 2016):
				self.StartYearList.insert(END, item)
			self.StartYearList.grid(column = 1, row = 2, rowspan = 2, sticky = "NS")
			
			# Start Month
			self.StartMonthLabel = Tkinter.Label(self.Frame2, text = "Start Month", bg = "black", fg = "white")
			self.StartMonthLabel.grid(column = 3, row = 1, sticky = "EW")
			
			self.StartMonthList = Listbox(self.Frame2, yscrollcommand = StartMonthScroll.set,
									 exportselection = FALSE)
			StartMonthScroll.config(command = self.StartMonthList.yview)
			StartMonthScroll.grid(column = 2, row = 2, rowspan = 4, sticky = "NS")
			for item in mos:
				self.StartMonthList.insert(END, item)
			self.StartMonthList.grid(column = 3, row = 2, rowspan = 4, sticky = "NS")
			
			# Start Day
			self.StartDayLabel = Tkinter.Label(self.Frame2, text = "Start Day", bg = "black", fg = "white")
			self.StartDayLabel.grid(column = 5, row = 1, sticky = "EW")
			
			self.StartDayList = Listbox(self.Frame2, yscrollcommand = StartDayScroll.set,
									 exportselection = FALSE)
			StartDayScroll.config(command = self.StartDayList.yview)
			StartDayScroll.grid(column = 4, row = 2, rowspan = 4, sticky = "NS")
			for item in range(1, 32):
				self.StartDayList.insert(END, item)
			self.StartDayList.grid(column = 5, row = 2, rowspan = 4, sticky = "NS")


			# Start Year
			#self.StartYearList
			# End Month
			self.EndMonthLabel = Tkinter.Label(self.Frame2, text = "End Month", bg = "black", fg = "white")
			self.EndMonthLabel.grid(column = 8, row = 1, sticky = "EW")
			
			self.EndMonthList = Listbox(self.Frame2, yscrollcommand = EndMonthScroll.set,
									 exportselection = FALSE)
			EndMonthScroll.config(command = self.EndMonthList.yview)
			EndMonthScroll.grid(column = 7, row = 2, rowspan = 4, sticky = "NS")
			for item in mos:
				self.EndMonthList.insert(END, item)
			self.EndMonthList.grid(column = 8, row = 2, rowspan = 4, sticky = "NS")
			
			# End Day
			self.EndDayLabel = Tkinter.Label(self.Frame2, text = "End Day", bg = "black", fg = "white")
			self.EndDayLabel.grid(column = 10, row = 1, sticky = "EW")
			
			self.EndDayList = Listbox(self.Frame2, yscrollcommand = EndDayScroll.set,
									  exportselection = FALSE)
			EndDayScroll.config(command = self.EndDayList.yview)
			EndDayScroll.grid(column = 9, row = 2, rowspan = 4, sticky = "NS")
			for item in range(1, 32):
				self.EndDayList.insert(END, item)
			self.EndDayList.grid(column = 10, row = 2, rowspan = 4, sticky = "NS")
			# End Year
			self.EndYearLabel = Tkinter.Label(self.Frame2, text = "End Year", bg = "black", fg = "white")
			self.EndYearLabel.grid(column = 6, row = 1, sticky = "EW")
	
			self.EndYearList = Listbox(self.Frame2, exportselection = FALSE)
			for item in range(2010, 2016):
				self.EndYearList.insert(END, item)
			self.EndYearList.grid(column = 6, row = 2, rowspan = 2, sticky = "NS")
			# Create the ListBox and Labels for "Site"
			self.StartHourScroll = Scrollbar(self.Frame3, orient = VERTICAL)
			self.EndHourScroll = Scrollbar(self.Frame3, orient = VERTICAL)
			
			self.SiteLabel = Tkinter.Label(self.Frame3, text = "Sites", bg = "black", fg = "white")
			self.SiteLabel.grid(column = 4, row = 0, sticky = "EW")

			self.SiteList = Listbox(self.Frame3, selectmode = MULTIPLE, exportselection = FALSE)
			for item in ["Arvin", "Mt Wilson", "Tranquillity", "Madera"]:
				self.SiteList.insert(END, item)
			self.SiteList.grid(column = 4, row = 1, rowspan = 4, sticky = "NS")
			# Title for Start Hour, end Hour section
			self.SubsetLabel = Tkinter.Label(self.Frame3, text = "Subset by Hour", 
											 bg = "green", fg = "black")
			self.SubsetLabel.grid(column = 0, row = 0, columnspan = 4, sticky = "EW")
			
			# Start Hour Selection
			self.StartLabel = Tkinter.Label(self.Frame3, text = "Start Hour", bg = "black", fg = "white")
			self.StartLabel.grid(column = 1, row = 1, sticky = "EW")

			self.StartList = Listbox(self.Frame3, yscrollcommand = self.StartHourScroll.set, 
									 exportselection = FALSE)
			for i in hours:
				self.StartList.insert(END, i)
			
			self.StartList.grid(column = 1, row = 2, rowspan = 4, sticky = "NS")
			self.StartHourScroll.config(command = self.StartList.yview)
			self.StartHourScroll.grid(column = 0, row = 2, rowspan = 4, sticky = "NS")
			# End Hour Selection
			self.EndLabel = Tkinter.Label(self.Frame3, text = "End Hour", bg = "black", fg = "white")
			self.EndLabel.grid(column = 3, row = 1, sticky = "EW")

			self.EndList = Listbox(self.Frame3, yscrollcommand = self.EndHourScroll, 
								   exportselection = FALSE)
			for i in hours:
				self.EndList.insert(END, i)

			self.EndList.grid(column = 3, row = 2, rowspan = 4, sticky = "NS")
			self.EndHourScroll.config(command = self.EndList.yview)
			self.EndHourScroll.grid(column = 2, row = 2, rowspan = 4, sticky = "NS")
		
		self.title.set(title)
		i = 0
		
		
		# Create Buttons for Directories, oldfiles
		for i in range(0, 4):
			
			# If Methane or CO, we need Directory Selection, and site names
			if gas == 1 or gas == 2:
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
				site1 = Tkinter.Label(self.Frame1, textvariable = self.site1)
				site1.grid(column = 0, row = 1)
				site2 = Tkinter.Label(self.Frame1, textvariable = self.site2)
				site2.grid(column = 0, row = 2)
				site3 = Tkinter.Label(self.Frame1, textvariable = self.site3)
				site3.grid(column = 0, row = 3)
				site4 = Tkinter.Label(self.Frame1, textvariable = self.site4)
				site4.grid(column = 0, row = 4)
				# Variables for directories
				self.dirname1 = Tkinter.StringVar()
				self.dirname2 = Tkinter.StringVar()
				self.dirname3 = Tkinter.StringVar()
				self.dirname4 = Tkinter.StringVar()
				self.dirname1.set("")
				self.dirname2.set("")
				self.dirname3.set("")
				self.dirname4.set("")
				
				# Labels for chosen directories
				dir1 = Tkinter.Entry(self.Frame1, textvariable = self.dirname1)
				dir1.grid(column = 2, row = 1)
				dir2 = Tkinter.Entry(self.Frame1, textvariable = self.dirname2)
				dir2.grid(column = 2, row = 2)
				dir3 = Tkinter.Entry(self.Frame1, textvariable = self.dirname3)
				dir3.grid(column = 2, row = 3)
				dir4 = Tkinter.Entry(self.Frame1, textvariable = self.dirname4)
				dir4.grid(column = 2, row = 4)
				
				# Buttons to fill in the labels
				DirCmd = partial(self.DirSelect, i)
				B1 = Tkinter.Button(self.Frame1, text = "Select directory", bg = "blue", fg = "white", command = DirCmd)
				B1.grid(column = 1, row = i + 1)
			
			# If Maps or Methane, we need file select
			if gas == 2 or gas == 3:
				if gas == 3:
					# File Selection Label
					self.FileLabel = Tkinter.Label(self.Frame1, text = "Trajectory Files", bg = "green", fg = "black")
					self.FileLabel.grid(column = 0, row = 0, columnspan = 3, sticky = "EW")
				# Variables for old files
				self.file1 = Tkinter.StringVar()
				self.file2 = Tkinter.StringVar()
				self.file3 = Tkinter.StringVar()
				self.file4 = Tkinter.StringVar()
				self.file1.set("")
				self.file2.set("")
				self.file3.set("")
				self.file4.set("")
				if gas == 2:
					col = 5
					
				if gas == 3:
					col = 2
				
				# Labels for old files
				file1 = Tkinter.Entry(self.Frame1, textvariable = self.file1)
				file1.grid(column = col, row = 1)
				file2 = Tkinter.Entry(self.Frame1, textvariable = self.file2)
				file2.grid(column = col, row = 2)
				file3 = Tkinter.Entry(self.Frame1, textvariable = self.file3)
				file3.grid(column = col, row = 3)
				file4 = Tkinter.Entry(self.Frame1, textvariable = self.file4)
				file4.grid(column = col, row = 4)
				
				# Buttons to fill in file labels
				FileCmd = partial(self.FileSelect, i)
				B2 = Tkinter.Button(self.Frame1, text = "Select File", bg = "blue", fg = "white", command = FileCmd)
				B2.grid(column = col - 2, row = i + 1)
			
		BRun = Tkinter.Button(self.Frame1, text = "Process Data", bg = "blue",
								  fg = "white", command = process)
		BRun.grid(column = 6, row = 1, rowspan = 4, sticky = "NS")
		
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
	def Maps(self):
	
		addtofile = list()
		addtofile.append('\ndatas <- list()')
		
		# Scan in the files provided by the user
		filename = list((self.file1.get(), self.file2.get(), self.file3.get(), self.file4.get()))
		for i in range(0, 4):
			fil = filename[i]
			print "\n%(fil)s" %vars()
			k = i + 1
			if fil != "":
				stuff = '\ndatas[[%(k)d]] <- Import(imp.file = "%(fil)s")' %vars()
			else:
				stuff = '\ndatas[[%(k)d]] <- NULL' %vars()
			addtofile.append(stuff)
		# Make the provided data into a single dataframe
		addtofile.append('\ndatas <- do.call(rbind, datas)')
		
	# Get all the variables we need, and make sure they're consistent
		binLength = self.Length.get()
		TooFew = self.TooFew.get()
		Map = self.Map.get()
		# Determine whether google maps or ggplot maps are selected
		if Map == 0:
			GMap = TRUE
			GG = FALSE
		else :
			GMap = FALSE
			GG = TRUE
		
		Site = self.SiteList.get(ACTIVE)
		GGType = self.GGTypeList.get(ACTIVE)
		GmapType = self.GmapTypeList.get(ACTIVE)
		StartYear = self.StartYearList.get(ACTIVE)
		StartMonth = self.StartMonthList.get(ACTIVE)
		StartDay = self.StartDayList.get(ACTIVE)
		if StartDay == "":
			StartDay = 1
		EndYear = self.EndYearList.get(ACTIVE)
		if EndYear == "":
			EndYear = StartYear
		EndMonth = self.EndMonthList.get(ACTIVE)
		if EndMonth == "":
			EndMonth = StartMonth
		EndDay = self.EndDayList.get(ACTIVE)
		if EndDay == "":
			EndDay == 30
		StartHour = self.StartList.get(ACTIVE)
		EndHour = self.EndList.get(ACTIVE)
		
		# Create the function call 
		stuff = '\noutput <- Boxer(datas, bin.length = %(binLength)f, map.type = "%(GGType)s", start.month = %(StartMonth)s, start.year = %(StartYear)d, start.day = %(StartDay)d, end.month = %(EndMonth)s, end.year = %(EndYear)d, too.few = %(TooFew)d, gg = %(GG)d, g.maps = %(GMap)d, gmap.type = "%(GmapType)s", start.time = %(StartHour)s, end.time = %(EndHour)s, output.map = TRUE, site = "%(Site)s")' %vars()
		addtofile.append(stuff)
		print stuff
		# Copy the script, add the function calls
		temp = copyfile("C:\\Users\\egregory\\Documents\\Methane_CO_GUI\\Maps_For_GUI.r",
						 "C:\\Users\\egregory\\Documents\\Methane_CO_GUI\\temp.r")
		inp = open("C:\\Users\\egregory\\Documents\\Methane_CO_GUI\\temp.r", "a")
		for i in range(0, len(addtofile)):
			inp.write(addtofile[i])
		inp.close()
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
