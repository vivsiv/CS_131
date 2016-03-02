from twisted.internet import reactor
from twisted.internet.protocol import ServerFactory, ClientFactory
from twisted.protocols.basic import LineReceiver
from twisted.web.client import getPage
from twisted.application import service, internet

import ssl
import time
import datetime
import json
import logging
import sys

BASE_PLACES_URL = "https://maps.googleapis.com/maps/api/place/nearbysearch/json?"
API_KEY = "AIzaSyBjuWeKFD5xhWepLVmx1ZeVc-8qsqOoUBs"

SERVERS = {
	"Alford" :
		{"port":9001, "neighbors":["Parker", "Welsh"]},
	"Bolden" :
		{"port":9002, "neighbors":["Parker", "Welsh"]},
	"Hamilton" :
		{"port":9003, "neighbors":["Parker"]},
	"Parker" :
		{"port":9004,"neighbors":["Alford","Bolden","Hamilton"]},
	"Welsh" :
		{"port":9005,"neighbors":["Alford","Bolden"]}
}

class HerdServerProtocol(LineReceiver):
	def __init__(self,factory):
		self.factory = factory

	def connectionMade(self):
		self.factory.num_connections += 1
		logging.info(" {0}: Connection Received. {1} connections open.".format(self.factory.name, self.factory.num_connections))
		self.sendLine("Make a request")

	def connectionLost(self,reason):
		self.factory.num_connections -= 1
		logging.info(" {0}: Connection Lost. {1} connections open.".format(self.factory.name, self.factory.num_connections))
		self.sendLine("Connection closed")

	def lineReceived(self,line):
		logging.info(" {0}: Line Received: {1}.".format(self.factory.name, line))
		request = line.split(" ")

		if (request[0] == "IAMAT"):
			if (len(request) == 4):
				self.handle_IAMAT(request[1:])
			else:
				logging.error(" {0}: Invalid IAMAT Request: {1}.".format(self.factory.name, line))
				self.sendLine("Invalid IAMAT request")
		elif (request[0] == "AT"):
			if (len(request) == 6):
				self.handle_AT(request[1:])
			else:
				logging.error(" {0}: Invalid AT Request: {1}.".format(self.factory.name, line))
				self.sendLine("Invalid AT request")
		elif (request[0] == "WHATSAT"):
			if (len(request) == 4):
				self.handle_WHATSAT(request[1:])
			else:
				logging.error(" {0}: Invalid WHATSAT Request: {1}.".format(self.factory.name, line))
				self.sendLine("Invalid WHATSAT request")
		else :
			self.sendLine("?")
			logging.error(" {0}: Invalid Request: {1}.".format(self.factory.name, line))
			print("Couldn't Identify Request")

	#IAMAT <kiwi.cs.ucla.edu> <+34.068930-118.445127> <1400794645.392014450>
	#IAMAT <client-name> <lat-long> <request-time>
	def handle_IAMAT(self,request):
		client_name, lat_long, request_time = request
		iamat_request = "IAMAT {0} {1} {2}".format(client_name, lat_long, request_time)
		self.sendLine("Received IAMAT: " + iamat_request)
		logging.info(" {0}: IAMAT Received: {1}.".format(self.factory.name, iamat_request))
		print iamat_request

		time_now = time.time()
		time_diff = time_now - float(request_time)
		iamat_response = "AT {0} {1} {2}".format(self.factory.name, time_diff, " ".join(request))
		self.factory.clients[client_name] = {"request_time":request_time, "request":iamat_response}

		self.sendLine(iamat_response)
		logging.info(" {0}: Sent IAMAT Response: {1}.".format(self.factory.name, iamat_response))
		print "Sent IAMAT response: " + iamat_response

		self.flood_neighbors(iamat_response)

	#AT <Alford> <+0.563873386> <kiwi.cs.ucla.edu> <+34.068930-118.445127> <1400794699.108893381>
	#AT <server-id> <time-diff> <client-name> <lat-long> <request-time>
	def handle_AT(self,request):
		server_id, time_diff, client_name, lat_long, request_time = request
		at_request = "AT {0} {1} {2} {3} {4}".format(server_id, time_diff, client_name, lat_long, request_time)

		self.sendLine("Received AT: " + at_request)
		logging.info(" {0}: AT Received: {1}.".format(self.factory.name, at_request))
		print at_request

		if client_name in self.factory.clients:
			past_request_time = self.factory.clients[client_name]["request_time"]
		 	if float(past_request_time) < float(request_time) :
		 		self.factory.clients[client_name] = {"request_time":request_time, "request":at_request}
		 		logging.info(" {0}: Updated AT for: {1} to {2}.".format(self.factory.name, client_name, at_request))
		 		self.flood_neighbors(at_request)
		 	else:
		 		logging.info(" {0}: Old AT for: {1} discarded {2}.".format(self.factory.name, client_name, at_request))
		else:
			self.factory.clients[client_name] = {"request_time":request_time, "request":at_request}			
			logging.info(" {0}: New AT for: {1} to {2}.".format(self.factory.name, client_name, at_request))
		 	self.flood_neighbors(at_request)

	def flood_neighbors(self,request):
		neighbors = SERVERS[self.factory.name]["neighbors"]
		print self.factory.name + ": flooding neighbors: " + ",".join(neighbors)
		for neighbor in neighbors:
			reactor.connectTCP('localhost', SERVERS[neighbor]["port"], HerdClientFactory(request))
			logging.info(" {0}: Flooded AT msg: {1} to: {2}.".format(self.factory.name, request, neighbor))


	#WHATSAT <kiwi.cs.ucla.edu> <10> <5>
	#WHATSAT <client-name> <radius-from-client> <results-bound>
	def handle_WHATSAT(self,request):
		client_name, radius, bound = request
		whatsat_request = "WHATSAT  {0} {1} {2}".format(client_name, radius, bound)

		self.sendLine("Received WHATSAT: " + whatsat_request)
		logging.info(" {0}: WHATSAT Received: {1}.".format(self.factory.name, whatsat_request))
		print whatsat_request

		print "Processing WHATSAT request"
		if client_name in self.factory.clients:
			requested_client = self.factory.clients[client_name]["request"]
			self.sendLine(requested_client)
			logging.info(" {0}: Client Found: {1}.".format(self.factory.name, requested_client))

			server_id, time_diff, client_name, lat_long, request_time = requested_client.split(" ")[1:]
			params = {}
			params["location"] = ",".join(lat_long.replace("+", " +",).replace("-", " -").split(" ")[1:])
			params["radius"] = radius
			
			places_request = self.buildRequest(params)

			print "Requesting: " + places_request
			logging.info(" {0}: API Request Sent: {1}.".format(self.factory.name, places_request))

			places_response = getPage(url = places_request)
			places_response.addCallback(
				callback = lambda response:(self.parseResponse(response,int(bound)))
			)
		else:
			self.sendLine("No Clients at: {0}".format(client_name))
			logging.warning(" {0}: No Clients found at: {1}.".format(self.factory.name, client_name))

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

		logging.info(" {0}: Received API Response: {1}".format(self.factory.name, json.dumps(json_response, indent=3)))
		write_response = json.dumps(json_response, indent=3) + '\n'
		self.transport.write(write_response)
		

class HerdServerFactory(ServerFactory):
	def __init__(self, name):
		self.num_connections = 0
		self.name = name
		self.clients = {}

		log_time = "_".join(str(datetime.datetime.now()).split(".")[0].split(" "))
		log_filename = self.name + "_" + log_time + ".log"
		logging.basicConfig(
			filename=log_filename,
			level=logging.DEBUG
		)
		logging.info(" {0}: Started server @ {1}".format(self.name, log_time))

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
	if (len(sys.argv) == 2):
		server_name = sys.argv[1]
		if server_name in SERVERS:
			reactor.listenTCP(SERVERS[server_name]["port"], HerdServerFactory(server_name))
			reactor.run()
		else:
			print "ERROR. Invalid Server name, valid servers are: "
			for server in SERVERS.keys():
				print server
			exit()
	else:
		print "ERROR. Usage: python prototype.py <server-name>"
		exit()
		

if __name__ == '__main__':
	herd_main()
