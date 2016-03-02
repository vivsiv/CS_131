import sys
import os

SERVER_NAMES = ["Alford", "Bolden", "Hamilton","Parker","Welsh"]
syscall = "python prototype.py {0} ".format(SERVER_NAMES[0])
for server in SERVER_NAMES[1:]:
	syscall += "& python prototype.py {0} ".format(server)
print syscall
os.system(syscall)