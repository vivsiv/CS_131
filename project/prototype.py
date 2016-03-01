from twisted.internet import reactor
from twisted.internet.protocol import Protocol, Factory
from twisted.protocols.basic import LineReceiver

import json


API_KEY = "AIzaSyBjuWeKFD5xhWepLVmx1ZeVc-8qsqOoUBs"
BASE_PLACES_URL = "https://maps.googleapis.com/maps/api/place/nearbysearch/json?"

class HerdProtocol(LineReceiver):
	def __init__(self,factory):
		self.factory = factory

	def connectionMade(self):
		self.sendLine("Make a request")
		print("Make a request")

	def connectionLost(self,reason):
		self.sendLine("Connection closed")
		print("Connection closed")

	def lineReceived(self,line):
		request = line.split(" ")
		self.sendLine("Request type: " + request[0])
		print "Request type: " + request[0]
		if (request[0] == "IAMAT"):
			if (len(request) == 4):
				self.handle_IAMAT(request[1:])
			else:
				self.sendLine("Invalid IAMAT request")
		elif (request[0] == "AT"):
			if (len(request) == 6):
				self.handle_AT(request[1:])
			else:
				self.sendLine("Invalid AT request")
		elif (request[0] == "WHATSAT"):
			if (len(request) == 4):
				self.handle_WHATSAT(request[1:])
			else:
				self.sendLine("Invalid WHATSAT request")
		else :
			self.sendLine("?")
			print("Couldn't Identify request")

	#IAMAT <kiwi.cs.ucla.edu> <+34.068930-118.445127> <1400794645.392014450>
	#IAMAT <client-name> <lat-long> <time>
	def handle_IAMAT(self,request):
		client_name, lat_long, time = request
		iamat_request = "IAMAT <{0}> <{1}> <{2}>".format(client_name, lat_long, time)
		self.sendLine("Received IAMAT: " + iamat_request)
		print iamat_request

	#AT <Alford> <+0.563873386> <kiwi.cs.ucla.edu> <+34.068930-118.445127> <1400794699.108893381>
	#AT <server-id> <time-diff> <client-name> <lat-long> <time>
	def handle_AT(self,request):
		server_id, time_diff, client_name, lat_long, time = request
		at_request = "AT <{0}> <{1}> <{2}> <{3}> <{4}>".format(server_id, time_diff, client_name, lat_long, time)
		self.sendLine("Received AT: " + at_request)
		print at_request

	#WHATSAT <kiwi.cs.ucla.edu> <10> <5>
	#WHATSAT <client-name> <radius-from-client> <results-bound>
	def handle_WHATSAT(self,request):
		client_name, radius, bound = request
		whatsat_request = "WHATSAT  <{0}> <{1}> <{2}>".format(client_name, radius, bound)
		self.sendLine("Received WHATSAT: " + whatsat_request)
		print whatsat_request

		# params = []
		# params["location"] = ",".join(request[2].replace("+", " +",).replace("-", " -").split(" ")[1:])
		# params["time"] = request[3]
		# print "Processing WHATSAT request"

	def buildRequest(self,params):
		request = BASE_PLACES_URL
		request += (key + "=" + API_KEY)
		if (len(params) > 0):
			for param in params:
				request += (param + "=")
				request += params[param]
		request



class HerdFactory(Factory):
    def buildProtocol(self, addr):
        return HerdProtocol(self)

# class ClientProtocol(Protocol):
# 	def __init__(self,factory):
# 		self.factory = factory

# 	def dataReceived(self,data):

# class ClientFactory(ClientFactory):

def herd_main():
	reactor.listenTCP(8001, HerdFactory())
	reactor.run()

if __name__ == '__main__':
	herd_main()
