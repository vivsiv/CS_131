from twisted.internet import reactor
from twisted.internet.protocol import ServerFactory, ClientFactory
from twisted.protocols.basic import LineReceiver
from twisted.web.client import getPage
from twisted.application import service, internet

import ssl
import time
import json

BASE_PLACES_URL = "https://maps.googleapis.com/maps/api/place/nearbysearch/json?"
API_KEY = "AIzaSyBjuWeKFD5xhWepLVmx1ZeVc-8qsqOoUBs"

SERVERS = {
	"Alford" :
		{"port":8001, "neighbors":["Parker", "Welsh"]},
	"Bolden" :
		{"port":8002, "neighbors":["Parker", "Welsh"]},
	"Hamilton" :
		{"port":8003, "neighbors":["Parker"]},
	"Parker" :
		{"port":8004,"neighbors":["Alford","Bolden","Hamilton"]},
	"Welsh" :
		{"port":8005,"neighbors":["Alford","Bolden"]}
}

class HerdServerProtocol(LineReceiver):
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

		self.flood_neighbors(iamat_response)

	#AT <Alford> <+0.563873386> <kiwi.cs.ucla.edu> <+34.068930-118.445127> <1400794699.108893381>
	#AT <server-id> <time-diff> <client-name> <lat-long> <request-time>
	def handle_AT(self,request):
		server_id, time_diff, client_name, lat_long, request_time = request
		at_request = "AT <{0}> <{1}> <{2}> <{3}> <{4}>".format(server_id, time_diff, client_name, lat_long, request_time)
		self.sendLine("Received AT: " + at_request)
		print at_request

		self.flood_neighbors(at_request)

	def flood_neighbors(self,request):
		neighbors = SERVERS[self.factory.name]["neighbors"]
		print self.factory.name + ": flooding neighbors: " + ",".join(neighbors)
		for neighbor in neighbors:
			reactor.connectTCP('localhost', SERVERS[neighbor]["port"], HerdClientFactory(request))


	#WHATSAT <kiwi.cs.ucla.edu> <10> <5>
	#WHATSAT <client-name> <radius-from-client> <results-bound>
	def handle_WHATSAT(self,request):
		client_name, radius, bound = request
		print "Bound is: " + str(int(bound))
		whatsat_request = "WHATSAT  <{0}> <{1}> <{2}>".format(client_name, radius, bound)
		self.sendLine("Received WHATSAT: " + whatsat_request)
		print whatsat_request

		print "Processing WHATSAT request"
		requested_client = self.factory.clients[client_name]
		if requested_client is not None:
			self.sendLine(requested_client)
			server_id, time_diff, client_name, lat_long, request_time = requested_client.split(" ")[1:]
			params = {}
			params["location"] = ",".join(lat_long.replace("+", " +",).replace("-", " -").split(" ")[1:])
			params["radius"] = radius
			
			places_request = self.buildRequest(params)
			print "Requesting: " + places_request
			places_response = getPage(url = places_request)
			places_response.addCallback(callback = lambda response:(self.parseResponse(response,int(bound))))
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

	def parseResponse(self, response, bound):
		json_response = json.loads(response)
		json_response["results"] = json_response["results"][0:bound]
		write_response = json.dumps(json_response, indent=3) + '\n'
		self.transport.write(write_response)
		

class HerdServerFactory(ServerFactory):
	def __init__(self, name):
		self.num_connections = 0
		self.name = name
		self.clients = {}

	def buildProtocol(self, addr):
		return HerdServerProtocol(self)


class HerdClientProtocol(LineReceiver):
	def __init__(self,factory):
		self.factory = factory

	def connectionMade(self):
		print "Sending request: " + self.factory.request
		self.sendLine(self.factory.request)
		self.transport.loseConnection()


class HerdClientFactory(ClientFactory):
	def __init__(self,request):
		self.request = request

	def buildProtocol(self, addr):
		return HerdClientProtocol(self)


def herd_main():
	reactor.listenTCP(SERVERS["Alford"]["port"], HerdServerFactory("Alford"))
	reactor.run()

if __name__ == '__main__':
	herd_main()
