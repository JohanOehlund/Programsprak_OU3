public class Weight_String extends Weight{

    private String wt;

    public Weight_String(Object wt) {
        this.wt=""+wt;
    }

    @Override
    public void add(Object wt) {
        //System.out.println(wt);
        this.wt=this.wt+wt;
    }

    @Override
    public Object getWT() {
        return wt;
    }

    @Override
    public void resetWT() {
        wt="";
    }
}
