from twisted.internet import reactor, protocol
from twisted.internet.protocol import Protocol, Factory
from twisted.protocols.basic import LineReceiver
from twisted.web.client import getPage
from twisted.application import service, internet

import ssl
import time
import json

BASE_PLACES_URL = "https://maps.googleapis.com/maps/api/place/nearbysearch/json?"
API_KEY = "AIzaSyBjuWeKFD5xhWepLVmx1ZeVc-8qsqOoUBs"

class HerdProtocol(LineReceiver):
	def __init__(self,factory):
		self.factory = factory

	def connectionMade(self):
		self.factory.num_connections += 1
		self.sendLine("Make a request")

	def connectionLost(self,reason):
		self.sendLine("Connection closed")

	def lineReceived(self,line):
		request = line.split(" ")

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
	#IAMAT <client-name> <lat-long> <request-time>
	def handle_IAMAT(self,request):
		client_name, lat_long, request_time = request
		iamat_request = "IAMAT <{0}> <{1}> <{2}>".format(client_name, lat_long, request_time)
		self.sendLine("Received IAMAT: " + iamat_request)
		print iamat_request

		time_diff = time.time() - float(request_time)
		iamat_response = "AT {0} {1} {2}".format("Alford", time_diff, " ".join(request))
		self.factory.clients[client_name] = iamat_response
		self.sendLine(iamat_response)
		print "Sent IAMAT response: " + iamat_response

	#AT <Alford> <+0.563873386> <kiwi.cs.ucla.edu> <+34.068930-118.445127> <1400794699.108893381>
	#AT <server-id> <time-diff> <client-name> <lat-long> <request-time>
	def handle_AT(self,request):
		server_id, time_diff, client_name, lat_long, request_time = request
		at_request = "AT <{0}> <{1}> <{2}> <{3}> <{4}>".format(server_id, time_diff, client_name, lat_long, request_time)
		self.sendLine("Received AT: " + at_request)
		print at_request

	#WHATSAT <kiwi.cs.ucla.edu> <10> <5>
	#WHATSAT <client-name> <radius-from-client> <results-bound>
	def handle_WHATSAT(self,request):
		client_name, radius, bound = request
		whatsat_request = "WHATSAT  <{0}> <{1}> <{2}>".format(client_name, radius, bound)
		self.sendLine("Received WHATSAT: " + whatsat_request)
		print whatsat_request

		print "Processing WHATSAT request"
		requested_client = self.factory.clients[client_name]
		if requested_client is not None:
			server_id, time_diff, client_name, lat_long, request_time = requested_client.split(" ")[1:]
			params = {}
			params["location"] = ",".join(lat_long.replace("+", " +",).replace("-", " -").split(" ")[1:])
			params["radius"] = radius
			
			places_request = self.buildRequest(params)
			print "Requesting: " + places_request
			places_response = getPage(url = places_request)
			places_response.addCallback(callback = lambda x:(self.parseResponse(x)))
			# json_response = json.loads(places_response)
			# print "Got Response: " + json_response
			# self.sendLine(json_response)

	def buildRequest(self,params):
		request = BASE_PLACES_URL
		request += ("key=" + API_KEY)
		if (len(params) > 0):
			for param in params:
				request += "&"
				request += (param + "=")
				request += params[param]
		return request

	def parseResponse(self, response):
		json_response = json.loads(places_response)
		print "Got Response: " + json_response
		self.sendLine(json_response)

		

class HerdFactory(Factory):
	def __init__(self, name):
		self.num_connections = 0
		self.name = name
		self.clients = {}

	def buildProtocol(self, addr):
		return HerdProtocol(self)

# class ClientProtocol(Protocol):
# 	def __init__(self,factory):
# 		self.factory = factory

# 	def dataReceived(self,data):

# class ClientFactory(ClientFactory):

def herd_main():
	reactor.listenTCP(8001, HerdFactory("Alford"))
	reactor.run()

if __name__ == '__main__':
	herd_main()
