from twisted.internet import reactor
from twisted.internet.protocol import Protocol, Factory
from twisted.protocols.basic import LineReceiver


API_KEY = "AIzaSyBjuWeKFD5xhWepLVmx1ZeVc-8qsqOoUBs"

class HerdProtocol(LineReceiver):
	def __init__(self,factory):
		self.factory = factory

	def connectionMade(self):
		self.sendLine("Make a request")
		print("Make a request")

	def connectionLost(self):
		self.sendline("Connection closed")
		print("Connection closed")

	def lineReceived(self,line):
		request = line.split(" ")
		self.sendLine("Request type: " + request[0])
		print "Request type: " + request[0]

	# def handle_IAMAT(self,iamat_msg):

	# def handle_AT(self,at_msg):

	# def handle_WHATSAT(self,whatsat_req):


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
