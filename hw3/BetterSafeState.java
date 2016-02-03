import java.util.concurrent.locks.ReentrantLock;
class BetterSafeState implements State {
    private byte[] value;
    private byte maxval;
    private ReentrantLock[] lockArr;
    private final ReentrantLock swapLock = new ReentrantLock();

    private void initLockArr(){
        lockArr = new ReentrantLock[value.length];
        for (int i = 0; i < value.length; i++){
            lockArr[i] = new ReentrantLock();
        }
    }

    BetterSafeState(byte[] v) { 
        value = v;
        //initLockArr(); 
        maxval = 127;
    }

    BetterSafeState(byte[] v, byte m) { 
        value = v;
        //initLockArr(); 
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
            // if (lockArr[i].tryLock() && lockArr[j].tryLock()){
                if (value[i] <= 0 || value[j] >= maxval) {
                    // lockArr[i].unlock();
                    // lockArr[j].unlock();
                    swapLock.unlock();
                    return false;
                }
        	    value[i]--;
        	    value[j]++;
                swapLock.unlock();
                // lockArr[i].unlock();
                // lockArr[j].unlock();  
                return true;
            }
            // else {
            //     if (lockArr[i].isHeldByCurrentThread()) lockArr[i].unlock();
            //     if (lockArr[j].isHeldByCurrentThread()) lockArr[j].unlock();
            // }
        }
    }
}