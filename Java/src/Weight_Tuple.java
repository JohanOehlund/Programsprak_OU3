public class Weight_Tuple extends Weight{

    private Tuple wt;

    public Weight_Tuple(Object wt) {
        super.wt=wt;
        this.wt=(Tuple)wt;
    }

    @Override
    public void add(Object wt) {
        int t=(int)wt;
        this.wt.setWt_int(t+(this.wt.getWt_int()));
        //this.wt.setWt_string(""+t.getWt_string()+(this.wt.getWt_string()));
    }

    @Override
    public Object getWT() {
        return  wt.getWt_int();
    }

    @Override
    public void resetWT() {
        this.wt.setWt_int(0);
        this.wt.setWt_string("");
    }

    @Override
    public boolean compare(Object a) {
        int temp = (int) a;
        return temp > wt.getWt_int();
    }

}
