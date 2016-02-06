import java.util.concurrent.atomic.*;

class BetterSorryState implements State {
    private AtomicInteger[] value;
    private byte maxval;

    private void initArray(byte[] v){
        value = new AtomicInteger[v.length];
        for (int i = 0; i < v.length; i++){
            value[i] = new AtomicInteger((int)v[i]);
        } 
    }
   
    BetterSorryState(byte[] v) { 
        initArray(v); 
        maxval = 127;
    }

    BetterSorryState(byte[] v, byte m) { 
        initArray(v); 
        maxval = m;
    }

    public int size() { 
        return value.length; 
    }

    public byte[] current() {
        byte[] out = new byte[value.length];
        for (int i = 0; i < value.length; i++){
            out[i] = (byte) value[i].get();
        }
        return out; 
    }

    public boolean swap(int i, int j) {
    	if (value[i].get() <= 0 || value[j].get() >= maxval) {
    	    return false;
    	}
    	value[i].getAndDecrement();
    	value[j].getAndIncrement();
    	return true;
    }
}