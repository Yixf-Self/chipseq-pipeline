##Need to a""dd config parser...

import ConfigParser
import os
config = ConfigParser.SafeConfigParser()
if os.path.exists("/Process10/Config/config.ini"):
	config.read("/Process10/Config/config.ini")
	inifile = open("/Process10/Config/configInstall.ini",'w')
	config.set("Executables","meme",)
	config.set("Executables","python",)
	config.set("Executables","perl",)
	config.set("Executables","bwa",)
	config.set("Executables","samtools",)
	config.set("Executables","picard",)
	config.set("Executables","rsync",)
	config.set("Executables","bedtools",)
	config.set("Executables","java",)
	config.set("Executables","rexec",)
	config.set("Executables","bigwig",)
	config.set("Executables","macs",)
	config.set("Executables","ame",)
	config.set("Executables","sicer",)
	config.set("Custom Scripts","tpicscreatecoverage",)
	config.set("Custom Scripts","tpicszeta",)
	config.set("Custom Scripts","tpics",)
	config.set("Libraries","rlibs",)
	config.set("Libraries","pythonlibs",)
	config.set("Libraries","perllibs",)
	config.set("Libraries","javalibs",)
	config.set("Executables","tfdb",)
	config.write(inifile)
	inifile.close()
