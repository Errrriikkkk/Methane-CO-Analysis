import time, sys, tkFileDialog
from tkFileDialog import askopenfilename
from subprocess import call

currentPath = sys.argv[0]
t = currentPath.rfind("\\")
currentPath = currentPath[0:t]
currentPath = currentPath.replace("\\", "\\\\")
print(currentPath)
tempPath = currentPath + "\\\COMethAverager4.0.py"
tempPath = tempPath.replace("Program Files", "Progra~1")
GUIPath = currentPath + "\\\COMethAverager5.0.py"
GUIPath = GUIPath.replace("Program Files", "Progra~1")
GUIFile = open(GUIPath, "a")
print(tempPath)
file = open(tempPath)
lines = file.readlines()
file.close()
RscriptFile = tkFileDialog.askopenfilename(initialdir = "/", title = "Please select the 'Rscript.exe' file on your system.  It should be under ~\\R\\bin\\x64\\ ")
RscriptFile = RscriptFile.replace("Program Files", "Progra~1")
print(RscriptFile)
for line in lines:
	line = line.replace("C:\\\\Users\\\\egregory\\\\Documents\\\\Methane_CO_GUI", currentPath)
	line = line.replace("C:\\\\Users\\\\egregory\\\\Documents\\\\R\\\\R-2.12.1\\\\bin\\\\x64\\\\Rscript.exe", RscriptFile)
	GUIFile.write(line)
GUIFile.close()
stuff = 'cd ' + currentPath + '&' +  RscriptFile + ' installPackages.R'
print stuff
call(stuff, shell = True)
time.sleep(500)

