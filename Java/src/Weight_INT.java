public class Weight_INT extends Weight {
    private int wt;
    public Weight_INT(Object wt) {
        super.wt=wt;
        this.wt=(int) wt;
    }

    @Override
    public void add(Object a) {
        wt+=(int)a;
        System.out.println("WT: "+wt);
        super.add(wt);
    }

    @Override
    public void resetWT() {
        wt=0;
        super.resetWT();
    }

}
