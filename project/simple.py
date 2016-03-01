from twisted.internet import reactor
import traceback

def stack():
    print 'The python stack:'
    traceback.print_stack()

reactor.callWhenRunning(stack)
reactor.run()
