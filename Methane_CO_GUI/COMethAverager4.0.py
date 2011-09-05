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
		# To speed up file scanning and therefore mapmaking...
		self.scanit = Tkinter.IntVar()
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
	
		
	def CreateFrame(self, gas):
		# Close any old windows 
		if self.open.get() == 1:
			self.Frame1.destroy()
			self.Frame2.destroy()
			self.Frame3.destroy()
			#self.status.destroy()
		self.open.set(1)
		self.scanit.set(1)
		
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
			self.Cutoff = Tkinter.IntVar()
			self.OutputMap = Tkinter.IntVar()
			self.OutputMap.set(1)
			self.OutputData = Tkinter.IntVar()
			self.OutputData.set(1)
			self.Compares = Tkinter.IntVar()
			self.Compares.set(0)
			self.Color = Tkinter.StringVar()
			self.Color.set("Reds")
			
			# Compare the inputted data?
			self.Compare = Tkinter.IntVar()
			
			# Dates
			self.StartYear = Tkinter.IntVar()
			self.StartMonth = Tkinter.IntVar()
			self.StartDay = Tkinter.IntVar()
			
			self.EndYear = Tkinter.IntVar()
			self.EndMonth = Tkinter.IntVar()
			self.EndDay = Tkinter.IntVar()
			
			# Hours
			self.StartHour = Tkinter.StringVar()
			self.EndHour = Tkinter.StringVar()
			self.CompareColor = Tkinter.StringVar()
			self.CompareMap = Tkinter.StringVar()
			
			self.Length.set(0.05)
			process = self.RunMap
			title = "Map Operations"
			
			# Bin Length
			self.BinLengthL = Tkinter.Label(self.Frame1, text = "Bin Length (degrees)", bg = "green", fg = "black")
			self.BinLengthL.grid(column = 3, row = 0, sticky = "EW")
			self.BinLengthE = Tkinter.Entry(self.Frame1, textvariable = self.Length)
			self.BinLengthE.grid(column = 3, row = 1)
			
			# Too Few
			self.TooFewL = Tkinter.Label(self.Frame1, text = "Too Few", bg = "green", fg = "black")
			self.TooFewL.grid(column = 3, row = 2, sticky = "EW")
			self.TooFewE = Tkinter.Entry(self.Frame1, textvariable = self.TooFew)
			self.TooFewE.grid(column = 3, row = 3)
			
			# Hours Backward
			self.BackL = Tkinter.Label(self.Frame1, text = "Hours Back", bg = "green", fg = "black")
			self.BackL.grid(column = 4, row = 0, sticky = "EW")
			self.BackE = Tkinter.Entry(self.Frame1, textvariable = self.Back)
			self.BackE.grid(column = 4, row = 1)
			self.Back.set(12)
			
			# Cutoff percentile
			self.CutoffL = Tkinter.Label(self.Frame1, text = "Top Percentile", bg = "green", fg = "black")
			self.CutoffL.grid(column = 4, row = 2, sticky = "EW")
			self.CutoffE = Tkinter.Entry(self.Frame1, textvariable = self.Cutoff)
			self.CutoffE.grid(column = 4, row = 3)
			self.Cutoff.set(25)
			
			# Compare maps?
			self.CompareL = Tkinter.Label(self.Frame1, text = "Compare Map", bg = "green", fg = "black")
			self.CompareL.grid(column = 5, row = 0, sticky = "EW")
			self.CompareB = Tkinter.Checkbutton(self.Frame1, text = "Compare Maps", variable = self.Compare)
			self.CompareB.grid(column = 5, row = 1, sticky = "EW")
			self.Compare.set(0)
			
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

			# Label for Subsetting by Time Period
			self.TimeSubsetLabel = Tkinter.Label(self.Frame2, text = "Subset by Dates", bg = "green", fg = "black")
			self.TimeSubsetLabel.grid(column = 0, row = 0, sticky = "EW", columnspan = 3)
			
			# Start Year
			self.StartYearL = Tkinter.Label(self.Frame2, text = "Start Year", bg = "black", fg = "white")
			self.StartYearL.grid(column = 0, row = 1, sticky = "EW")
			self.StartYearE = Tkinter.Entry(self.Frame2, textvariable = self.StartYear)
			self.StartYearE.grid(column = 0, row = 2, sticky = "NS")
			
			# Start Month
			self.StartMonthL = Tkinter.Label(self.Frame2, text = "Start Month", bg = "black", fg = "white")
			self.StartMonthL.grid(column = 1, row = 1, sticky = "EW")
			self.StartMonthE = Tkinter.Entry(self.Frame2, textvariable = self.StartMonth)
			self.StartMonthE.grid(column = 1, row = 2)
		
			# Start Day
			self.StartDayL = Tkinter.Label(self.Frame2, text = "Start Day", bg = "black", fg = "white")
			self.StartDayL.grid(column = 2, row = 1, sticky = "EW")
			self.StartDayE = Tkinter.Entry(self.Frame2, textvariable = self.StartDay)
			self.StartDayE.grid(column = 2, row = 2, sticky = "NS")
			
			# End Year
			self.EndYearL = Tkinter.Label(self.Frame2, text = "End Year", bg = "black", fg = "white")
			self.EndYearL.grid(column = 0, row = 3, sticky = "EW")
	
			self.EndYearE = Tkinter.Entry(self.Frame2, textvariable = self.EndYear)
			self.EndYearE.grid(column = 0, row = 4, sticky = "NS")
			
			# End Month
			self.EndMonthL = Tkinter.Label(self.Frame2, text = "End Month", bg = "black", fg = "white")
			self.EndMonthL.grid(column = 1, row = 3, sticky = "EW")
			self.EndMonthE = Tkinter.Entry(self.Frame2, textvariable = self.EndMonth)
			self.EndMonthE.grid(column = 1, row = 4, sticky = "NS")
			
			# End Day
			self.EndDayLabel = Tkinter.Label(self.Frame2, text = "End Day", bg = "black", fg = "white")
			self.EndDayLabel.grid(column = 2, row = 3, sticky = "EW")
			self.EndDayE = Tkinter.Entry(self.Frame2, textvariable = self.EndDay)
			self.EndDayE.grid(column = 2, row = 4, sticky = "NS")
			
			# Outputs
			self.OutputL = Tkinter.Label(self.Frame2, text = "Outputs", bg = "green", fg = "black")
			self.OutputL.grid(column = 3, row = 3, columnspan = 2, sticky = "EW")
			# Output Map
			self.OutputMapC = Tkinter.Checkbutton(self.Frame2, text = "Output Map", variable = self.OutputMap)
			self.OutputMapC.grid(column = 3, row = 4)
			# Output Data
			self.OutputDataC = Tkinter.Checkbutton(self.Frame2, text = "Output Data", variable = self.OutputData)
			self.OutputDataC.grid(column = 4, row = 4)
			
			self.SiteLabel = Tkinter.Label(self.Frame3, text = "Sites", bg = "black", fg = "white")
			self.SiteLabel.grid(column = 4, row = 0, sticky = "EW")

			self.SiteList = Listbox(self.Frame3, selectmode = MULTIPLE, exportselection = FALSE)
			for item in ["Arvin", "Mt Wilson", "Tranquillity", "Madera"]:
				self.SiteList.insert(END, item)
			self.SiteList.grid(column = 4, row = 1, rowspan = 4, sticky = "NS")
			
			# Title for Start Hour, end Hour section
			self.SubsetLabel = Tkinter.Label(self.Frame2, text = "Subset by Hour", 
											 bg = "green", fg = "black")
			self.SubsetLabel.grid(column = 3, row = 0, columnspan = 2, sticky = "EW")
			
			# Start Hour Selection
			self.StartHourL = Tkinter.Label(self.Frame2, text = "Start Hour", bg = "black", fg = "white")
			self.StartHourL.grid(column = 3, row = 1, sticky = "EW")
			self.StartHourE = Tkinter.Entry(self.Frame2, textvariable = self.StartHour)
			self.StartHourE.grid(column = 3, row = 2)
			
			# End Hour Selection
			self.EndHourL = Tkinter.Label(self.Frame2, text = "End Hour", bg = "black", fg = "white")
			self.EndHourL.grid(column = 4, row = 1, sticky = "EW")
			self.EndHourE = Tkinter.Entry(self.Frame2, textvariable = self.EndHour)
			self.EndHourE.grid(column = 4, row = 2)
			
			Bsetup = Tkinter.Button(self.Frame3, text = "Add to Queue", command = self.Maps, bg = "blue", fg = "white")
			Bsetup.grid(column = 7, row = 0, rowspan = 3, sticky = "NS")
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
		# If the files have not already been scanned in, do it.
				# Create a common map, if they want it
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
		StartYear = self.StartYear.get()
		StartMonth = self.StartMonth.get()
		StartDay = self.StartDay.get()
		if StartDay == "":
			StartDay = 1
		EndYear = self.EndYear.get()
		if EndYear == "":
			EndYear = StartYear
		EndMonth = self.EndMonth.get()
		if EndMonth == "":
			EndMonth = StartMonth
		EndDay = self.EndDay.get()
		if EndDay == "":
			EndDay == 30
		StartHour = self.StartHour.get()
		EndHour = self.EndHour.get()
		OutputMap = self.OutputMap.get()
		OutputData = self.OutputData.get()
		Cutoff = self.Cutoff.get()
		Back = self.Back.get()	
		# If the user wants to compare maps, do the following:
		# 1. Output formatted datasets, with NO MAP
		# 2. Bind all of the datasets together
		# 3. Create a map which includes all of those datasets
		# 4. Re-supply the data to the google map plotting function, along with the super map.
		if self.scanit.get() == 1:
			addtofile.append('\ndatas <- list()')
			# Scan in the files provided by the user
			filename = list((self.file1.get(), self.file2.get(), self.file3.get(), self.file4.get()))
			for i in range(0, 4):
				fil = filename[i]
				print "\n%(fil)s" %vars()
				k = i + 1
				if fil != "":
					stuff = '\ndatas[[%(k)d]] <- Import(imp.file = "%(fil)s", nrow = 2000000)' %vars()
				else:
					stuff = '\ndatas[[%(k)d]] <- NULL' %vars()
				addtofile.append(stuff)
			# Make the provided data into a single dataframe
			addtofile.append('\ndatas <- do.call(rbind, datas)')
		
			temp = copyfile("C:\\Users\\egregory\\Documents\\Methane_CO_GUI\\Maps_For_GUI2.0.r",
							"C:\\Users\\egregory\\Documents\\Methane_CO_GUI\\temp.r")	
			# If the user selects the compare option, we create a new list will all of the 
			# data to be compared.
			addtofile.append('\ndatas1 <- list()\nmap.name <- list()')
			self.scanit.set(0)
		if self.Compare.get() == 1:
			k = self.Compares.get() + 1
			if self.Compares.get() == 1:
				self.CompareColor.set(self.Color.get())
				self.CompareMap.set(GmapType)
			self.Compares.set(k)
			# Create a dataset and a map.name, but dont plot it yet.
			stuff = '\ndatas1[[%(k)d]] <- FilterData(datas, bin.length = %(binLength)f, cutoff = %(Cutoff)d, back = %(Back)d, start.month = %(StartMonth)s, start.year = %(StartYear)d, start.day = %(StartDay)d, end.month = %(EndMonth)s, end.year = %(EndYear)d, end.day = %(EndDay)d, too.few = %(TooFew)d, start.time = %(StartHour)s, end.time = %(EndHour)s)\nmap.name[[%(k)d]] <- FileName(bin.length = %(binLength)f, cutoff = %(Cutoff)d, back = %(Back)d, start.month = %(StartMonth)s, start.year = %(StartYear)d, start.day = %(StartDay)d, end.month = %(EndMonth)s, end.year = %(EndYear)d, end.day = %(EndDay)d, start.time = %(StartHour)s, end.time = %(EndHour)s, site = "%(Site)s")' %vars()

		# Create the normal function call 
		else :
			stuff = '\noutput <- HysplitMaps(datas, bin.length = %(binLength)f, cutoff = %(Cutoff)d, back = %(Back)d, map.type = "%(GGType)s", start.month = %(StartMonth)s, start.year = %(StartYear)d, start.day = %(StartDay)d, end.month = %(EndMonth)s, end.year = %(EndYear)d, end.day = %(EndDay)d, too.few = %(TooFew)d, gg = %(GG)d, g.maps = %(GMap)d, gmap.type = "%(GmapType)s", start.time = %(StartHour)s, end.time = %(EndHour)s, site = "%(Site)s", output.map = %(OutputMap)d, output.data = %(OutputData)d)' %vars()
		
		addtofile.append(stuff)
		inp = open("C:\\Users\\egregory\\Documents\\Methane_CO_GUI\\temp.r", "a")
		for i in range(0, len(addtofile)):
			inp.write(addtofile[i])
		inp.close()
	
	def RunMap(self):
		if self.scanit.get() == 0:
			# If the user selected the "compare" option, we must bind datas1
			# and then plot maps
			if self.Compares.get() > 0:
				col = self.CompareColor.get()
				type = self.CompareMap.get()
				site = self.Site.get()
				# Bind the data, create master map
				inp = open("C:\\Users\\egregory\\Documents\\Methane_CO_GUI\\temp.r", "a")
				stuff = '\nfor.map <- do.call(rbind, datas1)\nMyMap <- MasterMap(for.map, gmap.type = "%(type)s")' %vars()
				inp.write(stuff)
			
				for i in range(1, self.Compares.get() + 1):
					stuff = '\nHysplitMaps(datas1[[%(i)d]], colors = "%(col)s", site = %(site)d, filter = FALSE, gg = FALSE, map.name = map.name[[%(i)d]], g.maps = TRUE, MyMap = MyMap, output.map = TRUE, order.output = TRUE, output.data = TRUE, return.map = FALSE)'%vars()
					inp.write(stuff)
				inp.close()
			dostuff = call('cd C:\\Users\\egregory\\Documents\\Methane_CO_GUI&C:\\Users\\egregory\\Documents\\R\\R-2.12.1\\bin\\x64\\Rscript.exe temp.r', shell = True)
			self.Frame1.destroy()
			self.Frame2.destroy()
			self.Frame3.destroy()
		else :
			print "\nPlease select a dataset to process."
		
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
		site = list((self.site1.get(), self.site2.get(), self.site3.get(), self.site4.get()))
		addtofile = list()
		dirname = list((self.dirname1.get(), self.dirname2.get(), self.dirname3.get(), self.dirname4.get()))
		for i in range(0, 3):
			dir = dirname[i]
			if dirname[i] != "":
				sit = site[i]
				stuff = '\ndatas <- COImporter("%(dir)s")\ndatas <- DataFixer(datas, "%(sit)s") \nhourly <- COHourlyAverage(datas, "%(sit)s")\ncat("\nFinished averaging CO data for", "%(sit)s \n", sep = " ")\n' %vars()
				addtofile.append(stuff)
			else :
				addtofile.append("")
			
		print "Processing data..."
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
		self.open.set(0)
if __name__ == "__main__":
	app = App(None)
	app.title("Methane/CO Averager")
	app.mainloop()
