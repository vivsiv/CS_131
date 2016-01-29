import java.util.concurrent.atomic.AtomicIntegerArray;

class GetNSetState implements State {
    private AtomicIntegerArray value;
    private int maxval;

    private void initValue(byte[] v){
        value = new AtomicIntegerArray(v.length);
        for (int i = 0; i < v.length; i++){
            int converted = (int) v[i];
            value.set(i,converted);
        } 
    }

    GetNSetState(byte[] v) {
        maxval = 127;
        initValue(v);
    }

    GetNSetState(byte[] v, byte m) { 
        maxval = m;
        initValue(v);
    }

    public int size() { 
        return value.length();
    }

    public byte[] current() { 
        byte[] out = new byte[maxval];
        for (int i = 0; i < value.length(); i++){
            out[i] = (byte) value.get(i);
        }
        return out; 
    }

    public boolean swap(int i, int j) {
    	if (value.get(i) <= 0 || value.get(j) >= maxval) {
    	    return false;
    	}
        int ival = value.get(i);
    	value.set(i, ival - 1);
        int jval = value.get(j);
    	value.set(j, jval + 1);
    	return true;
    }
}