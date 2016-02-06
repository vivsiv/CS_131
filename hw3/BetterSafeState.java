import java.util.concurrent.locks.ReentrantLock;
class BetterSafeState implements State {
    private byte[] value;
    private byte maxval;
    private final ReentrantLock swapLock = new ReentrantLock();

    BetterSafeState(byte[] v) { 
        value = v;
        maxval = 127;
    }

    BetterSafeState(byte[] v, byte m) { 
        value = v;
        maxval = m;
    }

    public int size() { 
        return value.length; 
    }

    public byte[] current() { 
        return value; 
    }

    public boolean swap(int i, int j) {
        while(true){
            if (swapLock.tryLock()){
                if (value[i] <= 0 || value[j] >= maxval) {
                    swapLock.unlock();
                    return false;
                }
        	    value[i]--;
        	    value[j]++;
                swapLock.unlock();
                return true;
            }
        }
    }
}