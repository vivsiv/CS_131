import java.util.concurrent.*;
class BetterSafeState implements State {
    private byte[] value;
    private byte maxval;

    BetterSafeState(byte[] v) { 
        value = v; 
        maxval = 127;
        swapLock = new ReentrantLock();
    }

    BetterSafeState(byte[] v, byte m) { 
        value = v; 
        maxval = m; 
        swapLock = new ReentrantLock();
    }

    public int size() { 
        return value.length; 
    }

    public byte[] current() { 
        return value; 
    }

    public synchronized boolean swap(int i, int j) {
    	if (value[i] <= 0 || value[j] >= maxval) {
    	    return false;
    	}
    	value[i]--;
    	value[j]++;
    	return true;
    }
}